module Main where

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import           Data.Aeson.Lens (key)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Vector                as Vector
import           Data.Yaml
import           Options.Applicative

import           Cli
import           Prelude

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options) (fullDesc <> progDesc "Manipulate YAML")

run :: Options -> IO ()
run (Options fp cmd)  = do
  o <- decodeFileThrow fp
  runOptions o fp cmd

runOptions :: Value -> FilePath -> SubCommand -> IO ()
runOptions s fp = \case
  Get k -> do
    let v = s ^? key k
    stdout s
  Add (Update k v inplace) -> runAdd s k v >>= finish inplace fp
  Set (Update k v inplace) -> runSet s (Text.splitOn "::" k) v >>= finish inplace fp
  Json -> stdout s

  where
    stdout = putLByteString . Aeson.encodePretty
    finish inplace fp o =
      if inplace
        then encodeFile fp o
        else stdout o

runAdd :: MonadThrow m => Value -> Text -> Text -> m Value
runAdd (Object o) k v = case HashMap.lookup k o of
    Nothing -> panic "Key is not there"
    Just (Array a) -> do
      v' <- decodeThrow (Text.encodeUtf8 v)
      let a' = Vector.cons v' a
      pure $ Object (HashMap.insert k (Array a') o)
    _ -> panic "Key is not an Array"
runAdd _ _ _ = panic "Not implemented"

runSet :: MonadThrow m => Value -> [Text] -> Text -> m Value
runSet (Object o) [k] v =
  return $ Object (HashMap.insert k (String v) o)
-- runSet s (k:kx) v = s ^? at kx
runSet _ _ _ = panic "Not implemented"
