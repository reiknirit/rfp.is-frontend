module AppM where

import Prelude
import Control.Monad.Reader.Trans   (class MonadAsk, ReaderT
                                    , ask, asks, runReaderT)
import Data.Argonaut                (encodeJson, decodeJson)
import Data.Maybe                   (Maybe(..))
import Data.Either                  (Either(..))
import Effect.Aff                   (Aff)
import Effect.Aff.Class             (class MonadAff)
import Effect.Class                 (class MonadEffect
                                    , liftEffect)
import Effect.Console               as Console
import Type.Equality                (class TypeEquals, from)
import Routing.Duplex               (print)
import Routing.Hash                 (setHash)

import Api.Request                  (FormDataRequestMethod(..)
                                    ,RequestMethod(..)
                                    ,mkRequest
                                    ,mkFormDataRequest)
import Api.Endpoint                 (Endpoint(..))
import Capability.LogMessages       (class LogMessages
                                    ,logMessage)
import Capability.Navigate          (class Navigate)
import Data.Environment             (Environment(..), Env)
import Data.Log                     as Log
import Data.Route                   as Route
import Data.URL                     (BaseURL)
import Resource.Attachment          (class ManageAttachment)
import Resource.Submission          (class ManageSubmission)

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask
    liftEffect case env.environment of
      Production -> pure unit
      _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< setHash <<< print Route.routeCodec 

instance manageAttachmentAppM :: ManageAttachment AppM where
  uploadAttachment formData = do
    req <- mkFormDataRequest
      { endpoint: AttachmentUpload 
      , method: PostFormData $ Just formData
      }
    case req of
      Just json -> do
        let attachment = decodeJson json
        case attachment of
          Right attach -> pure $ Just attach
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure Nothing
      Nothing -> pure Nothing

instance manageSubmissionAppM :: ManageSubmission AppM where
  createSubmission submission = do
    req <- mkRequest
      { endpoint: CreateSubmission
      , method: Post $ Just $ encodeJson submission
      }
    case req of
      Just json -> do
        let sub = decodeJson json
        case sub of
          Right s -> pure $ Just s
          Left err -> do
            logMessage $ Log.Log { message: err }
            pure Nothing
      Nothing -> pure Nothing
