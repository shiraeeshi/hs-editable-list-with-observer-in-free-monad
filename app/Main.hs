{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE  DeriveFunctor #-}
module Main where

import Control.Monad (when, forM_)
import Control.Exception (try)
import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import Control.Monad.State.Strict (StateT, get, modify, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Prelude hiding (log)
import ViewUtils (clearScreen, showInRectangle, clearRectangle, showInGrid, drawGrid, highlightCell, printFromBottom)
import Free (Free(..), liftF, foldFree)

data RowData = Row { smth :: String } deriving Eq

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

data AppStateData m = AppState
  { rows :: [RowData]
  , activeCellY :: Maybe Int
  , debugMessages :: [String]
  , listeners :: AppStateListenersData m
  }

data AppStateListenersData m = AppStateListeners
  { rowsListeners :: [[RowData] -> m ()]
  , activeCellYListeners :: [Maybe Int -> m ()]
  , debugMessagesListeners :: [[String] -> m ()]
  }

addRowsListener :: Monad m => ([RowData] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addRowsListener listener (AppStateListeners rowsListeners _activeCellYListeners _debugMessagesListeners) =
  AppStateListeners (listener:rowsListeners) _activeCellYListeners _debugMessagesListeners

addActiveCellYListener :: Monad m => (Maybe Int -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addActiveCellYListener listener (AppStateListeners _rowsListeners activeCellYListeners _debugMessagesListeners) =
  AppStateListeners _rowsListeners (listener:activeCellYListeners) _debugMessagesListeners

addDebugMessagesListener :: Monad m => ([String] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addDebugMessagesListener listener (AppStateListeners _rowsListeners _activeCellYListeners debugMessagesListeners) =
  AppStateListeners _rowsListeners _activeCellYListeners (listener:debugMessagesListeners)



data EditableListOpsF r = GetList                          ([RowData]   -> r)
                        | GetActiveCellY                   ((Maybe Int) -> r)
                        | GetLogs                          ([String]    -> r)
                        | UpdateList           [RowData]   r
                        | UpdateActiveCellY    (Maybe Int) r
                        | Log                  String      r
                        deriving Functor

getList :: Free EditableListOpsF [RowData]
getList = liftF (GetList id) -- or Free (GetList return)

getActiveCellY :: Free EditableListOpsF (Maybe Int)
getActiveCellY = liftF (GetActiveCellY id) -- or Free (GetActiveCellY return)

getLogs :: Free EditableListOpsF [String]
getLogs = liftF (GetLogs id) -- or Free (GetLogs return)

updateList :: [RowData] -> Free EditableListOpsF ()
updateList l = liftF (UpdateList l ()) -- or Free (UpdateList l ())

updateActiveCellY :: (Maybe Int) -> Free EditableListOpsF ()
updateActiveCellY y = liftF (UpdateActiveCellY y ()) -- or Free (UpdateActiveCellY y ())

log :: String -> Free EditableListOpsF ()
log msg = liftF (Log msg ()) -- or Free (Log msg ())

newtype DictStateHolder a = Dict (StateT (AppStateData DictStateHolder) IO a) deriving (Functor, Applicative, Monad, MonadIO)

interpret' :: EditableListOpsF a -> DictStateHolder a
interpret' (GetList k) = do lst <- rows <$> (Dict get)
                            return (k lst)
interpret' (GetActiveCellY k) = do y <- activeCellY <$> (Dict get)
                                   return (k y)
interpret' (GetLogs k) = do logs <- debugMessages <$> (Dict get)
                            return (k logs)
interpret' (UpdateList l k) = do Dict $ modify $ \s -> s { rows = l }
                                 reacts <- (rowsListeners . listeners) <$> (Dict get)
                                 forM_ reacts ($ l) -- the same as forM_ reacts $ \react -> react l
                                 return k
interpret' (UpdateActiveCellY y k) = do Dict $ modify $ \s -> s { activeCellY = y }
                                        reacts <- (activeCellYListeners . listeners) <$> (Dict get)
                                        forM_ reacts ($ y) -- the same as forM_ reacts $ \react -> react y
                                        return k
interpret' (Log msg k) = do Dict $ modify $ \s -> s { debugMessages = take debugLinesCount (msg:(debugMessages s)) }
                            logs <- debugMessages <$> (Dict get)
                            reacts <- (debugMessagesListeners . listeners) <$> (Dict get)
                            forM_ reacts ($ logs) -- the same as forM_ reacts $ \react -> react logs
                            return k

interpret :: Free EditableListOpsF a -> DictStateHolder a
interpret = foldFree interpret'

dictStateAction :: AppStateData DictStateHolder -> DictStateHolder a -> IO ()
dictStateAction state (Dict action) = do
  runStateT action state
  return ()

debugLinesCount = 20

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  dictStateAction initialState $ do
    interpret initRows
    loop
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows

    initialState :: AppStateData DictStateHolder
    initialState = AppState [] Nothing [] initListeners

    initRows :: Free EditableListOpsF ()
    initRows = updateList initialRows

    initListeners :: AppStateListenersData DictStateHolder
    --initListeners =
    --    addRowsListener (mainRowsListener)
    --    (addActiveCellYListener (activeCellYListener)
    --    (addDebugMessagesListener (debugMessagesListener)
    --    (empty)))
    initListeners =
        addRowsListener (mainRowsListener)
        $ addActiveCellYListener (activeCellYListener)
        $ addDebugMessagesListener (debugMessagesListener)
        $ empty
      where
        empty = AppStateListeners [] [] []

    mainRowsListener :: [RowData] -> DictStateHolder ()
    mainRowsListener rows = do
      activeCellCoords <- fmap (\y -> (0, y)) <$> (interpret $ getActiveCellY)
      liftIO $ showInGrid
                 xUpperLeft
                 yUpperLeft
                 columnCount
                 columnWidth
                 activeCellCoords
                 (map (\row -> [smth row]) rows)
      interpret $ log "updated rows"

    activeCellYListener :: Maybe Int -> DictStateHolder ()
    activeCellYListener activeCellY = do
      let activeCellCoords = fmap (\y -> (0, y)) activeCellY
      liftIO $ drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount
      case activeCellCoords of
        Nothing -> return ()
        Just coordsPair -> do
          liftIO $ highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount coordsPair
          interpret $ log "highlighted cell"

    debugMessagesListener :: [String] -> DictStateHolder ()
    debugMessagesListener debugMessages = do
      liftIO $ printFromBottom
                 xUpperLeft
                 (yUpperLeft+12+debugLinesCount)
                 debugMessages

    loop :: DictStateHolder ()
    loop = do
      key <- liftIO $ getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
              activeCellY <- interpret $ getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ max 0 (y-1)
                    Nothing -> Just 0
              interpret $ updateActiveCellY newActiveCellY
              interpret $ log $ "↑ " ++ show(newActiveCellY)
              loop
          "\ESC[B" -> do -- down
              activeCellY <- interpret $ getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ min (rowCount-1) (y+1)
                    Nothing -> Just 0
              interpret $ updateActiveCellY newActiveCellY
              interpret $ log $ "↓ " ++ show(newActiveCellY)
              loop
          "\n" -> do -- enter
              activeCellY <- interpret $ getActiveCellY
              rows <- interpret $ getList
                
              let
                  eitherValue :: Either String String
                  eitherValue =
                    case activeCellY of
                      Nothing -> Left "there's no selected cell"
                      Just cellIndex ->
                        if cellIndex < 0 || cellIndex >= (length rows)
                          then Left $ "index out of bounds: " ++ (show cellIndex)
                          else Right $ smth $ rows !! cellIndex

                  showEditField :: String -> DictStateHolder ()
                  showEditField value = do
                    let
                      txt = "edit cell value:"
                      lentxt = length txt
                      yPos = 0
                      xPos = (columnCount * (columnWidth + 1)) + 3
                      replaceNth lst idx val = if idx < 1 then val:(tail lst) else (head lst) : (replaceNth (tail lst) (idx - 1) val)
                    liftIO $ showInRectangle xPos yPos lentxt [txt, value]
                    key <- liftIO $ getKey
                    case key of
                      "\n" -> do
                        case activeCellY of
                          Nothing -> return ()
                          Just cellIndex -> do
                            liftIO $ clearRectangle xPos yPos lentxt 2
                            rows <- interpret $ getList
                            interpret $ updateList $ replaceNth rows cellIndex (Row value)
                            loop
                      "\DEL" -> showEditField (if (length value) == 0 then value else init value)
                      c -> showEditField (value ++ c)
              case eitherValue of
                Left e -> do
                  interpret $ log $ "error: " ++ (show e)
                  loop
                Right v -> do
                  showEditField v
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
