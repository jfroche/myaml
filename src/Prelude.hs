-- | General project specific utilities.
module Prelude (
    module Exports
  , ppFailure
  , PP.pretty
) where

import           Control.Lens                 as Exports (at, makeClassy,
                                                          makeFieldsNoPrefix,
                                                          makeLenses, strict,
                                                          toListOf, view, _1,
                                                          _2, _Just)
import           Control.Lens.Operators       as Exports hiding ((<.>))
import           Control.Monad.Catch          as Exports (MonadThrow)
import           Data.Foldable                as Exports (foldr1)
import           Data.List.NonEmpty           as Exports (cons)
import           Data.Semigroup               as Exports hiding (Arg (..))
import           Data.String                  as Exports (String)
import           GHC.Exts                     as Exports (fromList)
import           Numeric.Natural              as Exports
import           Protolude                    as Exports hiding (Down,
                                                          First (..), Last (..),
                                                          break, getFirst,
                                                          getLast, (%), (<&>),
                                                          (<>))
import qualified System.Directory             as Directory
import           System.FilePath              as Exports ((</>))

import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP


import qualified System.IO

red = PP.annotate (PP.color PP.Red)

ppFailure :: MonadIO io => PP.Doc PP.AnsiStyle -> io ()
ppFailure msg = liftIO $ PP.putDoc $ (red "FAILURE: " <> msg) <> PP.line
