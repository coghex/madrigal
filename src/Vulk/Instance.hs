{-# LANGUAGE Strict #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | vulkan instance
module Vulk.Instance where
import Prelude()
import UPrelude
import Control.Exception ( bracket )
import Control.Monad ( when, (=<<) )
import Control.Monad.State.Class ( gets, modify )
import Control.Monad.IO.Class
import qualified Data.Vector                       as V
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.List ( partition )
import Data.Foldable ( toList, for_ )
import qualified Data.Text                         as T
import           Data.Text.Encoding
import Say
import Prog ( Prog, MonadIO(liftIO), MonadReader(ask) )
import Prog.Data ( Env(..), State(..), LoopControl(..) )
import qualified Vulk.GLFW as GLFW
import           Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_portability_enumeration
import Vulkan.Core10
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Zero

vulkInstanceCreateInfo ∷ Prog ε σ ( InstanceCreateInfo
                                    '[DebugUtilsMessengerCreateInfoEXT
                                     , ValidationFeaturesEXT] )
vulkInstanceCreateInfo = do
  glfwReqExts ← liftIO $ traverse BS.packCString =<< GLFW.getRequiredInstanceExtensions
  let glfwReqExts' = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME : glfwReqExts
  availableExtensionNames <-
    toList
    .   fmap extensionName
    .   snd
    <$> enumerateInstanceExtensionProperties Nothing
  availableLayerNames <-
    toList . fmap layerName . snd <$> enumerateInstanceLayerProperties

  let requiredLayers     = []
      optionalLayers     = ["VK_LAYER_KHRONOS_validation"]
      requiredExtensions = [EXT_DEBUG_UTILS_EXTENSION_NAME] <> glfwReqExts'
      optionalExtensions = [EXT_VALIDATION_FEATURES_EXTENSION_NAME]
  extensions ← partitionOptReq "extension"
                               availableExtensionNames
                               optionalExtensions
                               requiredExtensions
  layers ← partitionOptReq "layer"
                           availableLayerNames
                           optionalLayers
                           requiredLayers
  let instanceCreateFlags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
  pure
    $ zero
        { applicationInfo       = Just zero { applicationName = Just "madrigal"
                                            , apiVersion = API_VERSION_1_0 }
        , enabledLayerNames     = V.fromList layers
        , enabledExtensionNames = V.fromList extensions
        , flags                 = instanceCreateFlags
        }
    ::& debugUtilsMessengerCreateInfo
    :&  ValidationFeaturesEXT [] []
    :&  ()

partitionOptReq
  :: (Show a, Eq a, MonadIO m) => T.Text -> [a] -> [a] -> [a] -> m [a]
partitionOptReq type' available optional required = do
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
      tShow                 = T.pack . show
  for_ optMissing
    $ \n -> sayErr $ "Missing optional " <> type' <> ": " <> tShow n
  case reqMissing of
    []  -> pure ()
    [x] -> sayErr $ "Missing required " <> type' <> ": " <> tShow x
    xs  -> sayErr $ "Missing required " <> type' <> "s: " <> tShow xs
  pure (reqHave <> optHave)

debugUtilsMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo = zero
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                        .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }
-- | A debug callback which prints the message prefixed with "Validation: " to
-- stderr.
foreign import ccall unsafe "DebugCallback.c &debugCallback"
  debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT
-- | A debug callback the same as 'debugCallbackPtr' except it will call
-- @abort@ when @VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT@ is set.
foreign import ccall unsafe "DebugCallback.c &debugCallbackFatal"
  debugCallbackFatalPtr :: PFN_vkDebugUtilsMessengerCallbackEXT

-- | Assign a name to a handle using 'setDebugUtilsObjectNameEXT', note that
-- the @VK_EXT_debug_utils@ extension must be enabled.
nameObject :: (HasObjectType a, MonadIO m) => Device -> a -> BS.ByteString -> m ()
nameObject device object name = setDebugUtilsObjectNameEXT
  device
  (uncurry DebugUtilsObjectNameInfoEXT (objectTypeAndHandle object) (Just name))

