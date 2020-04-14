module Smuggler.Options (Options (..), parseCommandLineOptions)where

import Data.Char (toLower)
import           Plugins

data ImportAction = NoImportProcessing | PreserveInstanceImports | MinimiseImports

data ExportAction = NoExportProcessing | ExplicitExports | ReplaceExports

data Options
  = Options
      { importAction :: ImportAction,
        exportAction :: ExportAction,
        newSuffix :: Maybe String
      }

defaultOptions :: Options
defaultOptions = Options PreserveInstanceImports ExplicitExports Nothing

parseCommandLineOption :: Options -> CommandLineOption -> Options
parseCommandLineOption opts clo = case toLower <$> clo of
  "noimportprocessing"      -> opts { importAction = NoImportProcessing }
  "preserveinstanceimports" -> opts { importAction = PreserveInstanceImports }
  "minimiseimports"         -> opts { importAction = MinimiseImports }
  "noexportprocessing"      -> opts { exportAction = NoExportProcessing }
  "explicitexports"         -> opts { exportAction = ExplicitExports }
  "replaceexports"          -> opts { exportAction = ReplaceExports }
  suffix                    -> opts { newSuffix = Just suffix }


parseCommandLineOptions :: [CommandLineOption] -> Options
parseCommandLineOptions = foldl' parseCommandLineOption defaultOptions
