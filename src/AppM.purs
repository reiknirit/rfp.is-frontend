module AppM where

import Prelude

import Affjax (printError)
import Api.Endpoint (Endpoint(..))
import Api.Request (FormDataRequestMethod(..), RequestMethod(..), mkRequest, mkFormDataRequest)
import Capability.LogMessages (class LogMessages, logMessage)
import Capability.Navigate (class Navigate)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut (decodeJson, encodeJson, printJsonDecodeError)
import Data.Auth (APIAuth(..), apiAuth)
import Data.Either (Either(..))
import Data.Environment (Environment(..), Env)
import Data.Log as Log
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Data.URL (BaseURL)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console as Console
import Resource.Attachment (class ManageAttachment)
import Resource.Submission (class ManageSubmission)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

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

genericRequest endpoint method auth = do
  req <- mkRequest
    { endpoint: endpoint
    , method: method
    , auth: auth
    }
  case req of
    Right json -> do
      let et = decodeJson json.body
      case et of
        Right e -> pure $ Just e
        Left err -> do
          logMessage $ printJsonDecodeError err
          pure Nothing
    Left err -> do
      logMessage $ printError err
      pure Nothing

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask
    liftEffect case env.environment of
      Production -> pure unit
      _ -> Console.log log

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< setHash <<< print Route.routeCodec 

instance manageAttachmentAppM :: ManageAttachment AppM where
  uploadAttachment formData = do
    req <- mkFormDataRequest
      { endpoint: AttachmentUpload 
      , method: PostFormData $ Just formData
      , auth: Just apiAuth
      }
    case req of
      Right json -> do
        let attachment = decodeJson json.body
        case attachment of
          Right attach -> pure $ Just attach
          Left err -> do
            logMessage $ printJsonDecodeError err
            pure Nothing
      Left err -> do
        logMessage $ printError err
        pure Nothing

instance manageSubmissionAppM :: ManageSubmission AppM where
  createSubmission submission 
    = genericRequest
        CreateSubmission
        (Post $ Just $ encodeJson submission)
        (Just apiAuth)
