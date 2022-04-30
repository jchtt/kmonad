{-|
Module      : KMonad.Model.Hooks
Description : Component for handling hooks
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

Part of the KMonad deferred-decision mechanics are implemented using hooks,
which will call predicates and actions on future keypresses and/or timer events.
The 'Hooks' component is the concrete implementation of this functionality.

In the sequencing of components, this happens second, right after the
'KMonad.App.Dispatch.Dispatch' component.

-}
module KMonad.Model.Hooks
  ( Hooks
  , mkHooks
  , pull
  , register
  , block
  , unblock
  )
where

import KMonad.Prelude

import Data.Time.Clock.System
import Data.Unique

import KMonad.Model.Action hiding (register)
import KMonad.Keyboard
import KMonad.Util

import RIO.Partial (fromJust)

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $hooks



-- -- | A 'Hook' contains the 'KeyPred' and 'Callback'
-- newtype Hook = Hook (KeyPred, Callback IO)
-- makeWrapped ''Hook

-- -- | Create a new 'Hook' value
-- mkHook :: MonadUnliftIO m => KeyPred -> Callback m -> m Hook
-- mkHook p c = withRunInIO $ \u -> pure $ Hook (p, (u . c))

--------------------------------------------------------------------------------
-- $env

data Entry = Entry
  { _time  :: SystemTime
  , _eHook :: Hook IO
  }
makeLenses ''Entry

instance HasHook Entry IO where hook = eHook

type Store = M.HashMap Unique Entry

-- | The 'Hooks' environment that is required for keeping track of all the
-- different targets and callbacks.
data Hooks = Hooks
  { _eventSrc         :: IO WrappedEvent           -- ^ Where we get our events from
  , _injectTmr        :: TMVar Unique          -- ^ Used to signal timeouts
  , _priorityHooks    :: TVar Store            -- ^ Store of priority hooks
  , _hooks            :: TVar Store            -- ^ Store of hooks
  , _blocked          :: TVar Int              -- ^ How many locks have been applied to the hooks
  , _blockBuf         :: TVar [WrappedEvent]       -- ^ Internal buffer to store events while closed
  }
makeLenses ''Hooks

-- | Create a new 'Hooks' environment which reads events from the provided action
mkHooks' :: MonadUnliftIO m => m WrappedEvent -> m Hooks
mkHooks' s = withRunInIO $ \u -> do
  itr <- atomically $ newEmptyTMVar
  hks <- atomically $ newTVar M.empty
  phks <- atomically $ newTVar M.empty
  blk <- atomically $ newTVar 0
  buf <- atomically $ newTVar []
  pure $ Hooks (u s) itr phks hks blk buf

-- | Create a new 'Hooks' environment, but as a 'ContT' monad to avoid nesting
mkHooks :: MonadUnliftIO m => m WrappedEvent -> ContT r m Hooks
mkHooks = lift . mkHooks'

-- | Convert a hook in some UnliftIO monad into an IO version, to store it in Hooks
ioHook :: MonadUnliftIO m => Hook m -> m (Hook IO)
ioHook h = withRunInIO $ \u -> do

  t <- case _hTimeout h of
    Nothing -> pure Nothing
    Just t' -> pure . Just $ Timeout (t'^.delay) (u (_action t'))
  let f = \e -> u $ (_keyH h) e
  pure $ Hook t f


--------------------------------------------------------------------------------
-- $op
--
-- The following code deals with simple operations on the environment, like
-- inserting and removing hooks.

-- | Insert a hook, along with the current time, into the store
register :: (HasLogFunc e)
  => Hooks
  -> Hook (RIO e)
  -> Bool
  -> RIO e ()
register hs h prio = do
  -- Insert an entry into the store
  tag <- liftIO newUnique
  let targetStore = if prio then priorityHooks else hooks
  e   <- Entry <$> liftIO getSystemTime <*> ioHook h
  atomically $ modifyTVar (hs^.targetStore) (M.insert tag e)
  -- If the hook has a timeout, start a thread that will signal timeout
  case h^.hTimeout of
    Nothing -> logDebug $ "Registering untimed hook: " <> display (hashUnique tag) <> ", priority " <> displayShow prio
    Just t' -> void . async $ do
      logDebug $ "Registering " <> display (t'^.delay)
              <> "ms hook: " <> display (hashUnique tag)
      threadDelay $ 1000 * (fromIntegral $ t'^.delay)
      atomically $ putTMVar (hs^.injectTmr) tag

-- | Cancel a hook by removing it from the store
cancelHook :: (HasLogFunc e)
  => TVar Store
  -> Unique
  -> RIO e Bool
cancelHook st tag = do
  e <- atomically $ do
    m <- readTVar $ st
    let v = M.lookup tag m
    when (isJust v) $ modifyTVar st (M.delete tag)
    pure v
  case e of
    Nothing -> do
      logDebug $ "Tried cancelling expired hook: " <> display (hashUnique tag)
      pure False
    Just e' -> do
      logDebug $ "Cancelling hook: " <> display (hashUnique tag)
      liftIO $ e' ^. hTimeout . to fromJust . action
      pure True
  -- atomically $ do
  --   prioTag <- tryTakeTMVar (hs^.priorityTag)
  --   case prioTag of
  --     Just t -> when (t /= tag) $ putTMVar (hs^.priorityTag) t
  --     Nothing -> return ()
  --   case prioTag of
  --     Just t -> 
  --   when (t <> tag) $ 
  -- -- TODO: This is ugly, and should probably also be done atomically
  -- case prioTag of
  --   Just t -> do
  --     _ <- when (t == tag) $ atomically $ tryTakeTMVar (hs^.priorityTag)
  --     return ()
  --   Nothing -> return ()

-- | Increase the block-count by 1
block :: HasLogFunc e => Hooks -> RIO e ()
block hs = do
  n <- atomically $ do
    modifyTVar' (hs^.blocked) (+1)
    readTVar (hs^.blocked)
  logDebug $ "Hooks block level set to: " <> display n

-- | Set Hooks to unblocked mode, return a list of all the stored events
-- that should be rerun, in the correct order (head was first-in, etc).
--
-- NOTE: After successfully unblocking, blockBuf will be empty, it is the
-- caller's responsibility to insert the returned events at an appropriate
-- location in the 'KMonad.App.App'.
--
-- We do this in KMonad by writing the events into the
-- 'KMonad.Model.Dispatch.Dispatch's rerun buffer. (this happens in the
-- "KMonad.App" module.)
unblock :: HasLogFunc e => Hooks -> RIO e [WrappedEvent]
unblock hs = do
  n <- atomically $ do
    modifyTVar' (hs^.blocked) (\n -> n - 1)
    readTVar (hs^.blocked)
  case n of
    0 -> do
      es <- atomically $ readTVar (hs^.blockBuf)
      atomically $ writeTVar (hs^.blockBuf) []
      logDebug $ "Unblocking hooks input stream, " <>
        if null es
        then "no stored events"
        else "rerunning:\n" <> (display . unlines . map textDisplay $ reverse es)
      pure $ reverse es
    n -> do
      logDebug $ "Hooks block level set to: " <> display n
      pure []

-- | Put the WrappedEvent into blockBuf to play back later
store :: HasLogFunc e => Hooks -> WrappedEvent -> RIO e ()
store hs e = do
  atomically $ modifyTVar' (hs^.blockBuf) (e:)
  readTVarIO (hs^.blockBuf) >>= \es -> do
    let xs = map ((" - " <>) . textDisplay) es
    logDebug . display . unlines $ "Storing event, current store: ":xs

--------------------------------------------------------------------------------
-- $run
--
-- The following code deals with how we check hooks against incoming events, and
-- how this updates the 'Hooks' environment.

-- | Run the function stored in a Hook on the event and the elapsed time
runEntry :: MonadIO m => SystemTime -> KeyEvent -> Entry -> m Catch
runEntry t e v = liftIO $ do
  (v^.keyH) $ Trigger ((v^.time) `tDiff` t) e

-- | Run all hooks on the current event and reset the store
runHooks :: (HasLogFunc e)
  => TVar Store
  -> KeyEvent
  -- -> RIO e (Maybe KeyEvent)
  -> RIO e Catch
runHooks st e = do
  logDebug "Running hooks"
  m   <- atomically $ swapTVar st M.empty
  now <- liftIO getSystemTime
  foldMapM (runEntry now e) (M.elems m)

--------------------------------------------------------------------------------
-- $loop
--
-- The following code deals with how to use the 'Hooks' component as part of a
-- pull-chain. It contains logic for how to try to pull events from upstream and
-- check them against the hooks, and for how to keep stepping until an unhandled
-- event comes through.

-- | Pull 1 event from the '_eventSrc'. If that action is not caught by any
-- callback, then return it (otherwise return Nothing). At the same time, keep
-- reading the timer-cancellation inject point and handle any cancellation as it
-- comes up.
step :: (HasLogFunc e)
  => Hooks                  -- ^ The 'Hooks' environment
  -> RIO e (Maybe KeyEvent) -- ^ An action that returns perhaps the next event
step h = do

  -- Asynchronously start reading the next event
  a <- async . liftIO $ h^.eventSrc

  -- Handle any timer event first, and then try to read from the source
  let next = (Left <$> takeTMVar (h^.injectTmr)) `orElse` (Right <$> waitSTM a)

  -- Keep taking and cancelling timers until we encounter a key event, then run
  -- the hooks on that event.
  let read = atomically next >>= \case
      --Left  t -> cancelHook (h^.hooks) t >> read -- We caught a cancellation
        Left t -> handleTag t >> read
        Right (WrappedTag t) -> handleTag t >> pure Nothing
        Right (WrappedKeyEvent c e) -> handleEvent c e           -- We caught a real event
  read

  where
    handleTag t = do
      n <- readTVarIO (h^.blocked)
      case n of
        0 -> handleTagOpen t
        _ -> handleTagClosed t

    handleEvent c e = do
      cPrio <- runHooks (h^.priorityHooks) e
      n <- readTVarIO (h^.blocked)
      case n of
        0 -> do
          cReg <- runHooks (h^.hooks) e
          case c <> cPrio <> cReg of
            NoCatch -> pure $ Just e
            Catch -> pure Nothing
        _ -> store h (WrappedKeyEvent (c <> cPrio) e) >> pure Nothing

    handleTagOpen t = do
      _ <- cancelHook (h^.priorityHooks) t
      cancelHook (h^.hooks) t >> pure ()

    handleTagClosed t = do
      found <- cancelHook (h^.priorityHooks) t
      unless found $ store h (WrappedTag t)
      

-- | Keep stepping until we succesfully get an unhandled 'KeyEvent'
pull :: HasLogFunc e
  => Hooks
  -> RIO e KeyEvent
pull h = step h >>= maybe (pull h) pure
