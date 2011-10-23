module HEP.Automation.MadGraph.Log 
  ( Priority
  , LogChan
  , Message
  , defaultLogChan
  , debugMsg  
  , infoMsg
  , noticeMsg
  , warningMsg 
  , errorMsg 
  , criticalMsg
  , alertMsg
  , emergencyMsg
  , debugMsgDef  
  , infoMsgDef
  , noticeMsgDef
  , warningMsgDef 
  , errorMsgDef 
  , criticalMsgDef
  , alertMsgDef
  , emergencyMsgDef

  ) where

import Control.Monad.IO.Class

import qualified System.Log.Logger as L
import System.Log.Logger (Priority)
--import qualified System.Log.Handler.Syslog as LS
--import qualified System.Log.Handler.Simple as LHS
--import qualified System.Log.Handler (setFormatter) 
--import qualified System.Log.Formatter as LF

type LogChan = String 
type Message = String 

defaultLogChan :: String
defaultLogChan = "MadGraphAuto"

convertMonadIO2 :: (MonadIO m) => 
                   (String -> String -> IO ()) 
                -> LogChan -> Message -> m ()
convertMonadIO2 f ch msg = liftIO (f ch msg)

debugMsg :: (MonadIO m) => LogChan -> Message -> m ()
debugMsg = convertMonadIO2 L.debugM

infoMsg :: (MonadIO m) => LogChan -> Message -> m ()
infoMsg = convertMonadIO2 L.infoM 

noticeMsg :: (MonadIO m) => LogChan -> Message -> m ()
noticeMsg = convertMonadIO2 L.noticeM

warningMsg :: (MonadIO m) => LogChan -> Message -> m ()
warningMsg = convertMonadIO2 L.warningM

errorMsg :: (MonadIO m) => LogChan -> Message -> m ()
errorMsg = convertMonadIO2 L.errorM

criticalMsg :: (MonadIO m) => LogChan -> Message -> m ()
criticalMsg = convertMonadIO2 L.criticalM

alertMsg :: (MonadIO m) => LogChan -> Message -> m ()
alertMsg = convertMonadIO2 L.alertM

emergencyMsg :: (MonadIO m) => LogChan -> Message -> m ()
emergencyMsg = convertMonadIO2 L.emergencyM



debugMsgDef :: (MonadIO m) =>  Message -> m ()
debugMsgDef = debugMsg defaultLogChan

infoMsgDef :: (MonadIO m) =>  Message -> m ()
infoMsgDef = infoMsg defaultLogChan 

noticeMsgDef :: (MonadIO m) =>  Message -> m ()
noticeMsgDef = noticeMsg defaultLogChan 

warningMsgDef :: (MonadIO m) =>  Message -> m ()
warningMsgDef = warningMsg defaultLogChan

errorMsgDef :: (MonadIO m) =>  Message -> m ()
errorMsgDef = errorMsg defaultLogChan

criticalMsgDef :: (MonadIO m) =>  Message -> m ()
criticalMsgDef = criticalMsg defaultLogChan

alertMsgDef :: (MonadIO m) =>  Message -> m ()
alertMsgDef = alertMsg defaultLogChan

emergencyMsgDef :: (MonadIO m) =>  Message -> m ()
emergencyMsgDef = emergencyMsg defaultLogChan







