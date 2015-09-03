module Kagamin.Modules where
import Data.Text (Text)
import Web.Slack (Slack, ChannelId, Submitter)

-- | Result of executing a given hook: either the hook did nothing, modified
--   the message text for the next hook in the pipeline, or requested
--   processing to cease.
data HookResult = Next | Modify Text | Stop

-- | Hook for handling received messages.
type MsgHook = ChannelId -> Submitter -> Text -> Slack () HookResult

data KagaModule = KagaModule {
    -- | Hook for messages directed at Kagamin.
    kagaMsgHook   :: MsgHook,

    -- | Hook for messages *not* directed at Kagamin.
    kagaOtherHook :: MsgHook,

    -- | Hook for *all* messages. Processed after all other messages.
    kagaAllHook   :: MsgHook,

    -- | Hook for saving the state of a module.
    kagaSaveHook  :: FilePath -> IO (),

    -- | Hook for loading the state of a module.
    kagaLoadHook  :: FilePath -> IO ()
  }

-- | Hook that does nothing.
nopHook :: MsgHook
nopHook _ _ _ = return Next

-- | Default KagaModule: does nothing, ever.
defaultModule :: KagaModule
defaultModule = KagaModule {
    kagaMsgHook   = nopHook,
    kagaOtherHook = nopHook,
    kagaAllHook   = nopHook,
    kagaSaveHook  = const $ return (),
    kagaLoadHook  = const $ return ()
  }
