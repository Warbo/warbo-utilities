module StoreTest where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO
import System.Process (runProcess, readProcess)

type Class  = String
type Method = String

data Args = Args {clean :: IO (),
                  clear :: IO (),
                  site  :: String,
                  tests :: Map.Map Class [Method]}

basePath = "/var/www/domains/wandisco.com/drupal7/htdocs/sites/"

parseArgs args = let given   = (`elem` args)
                     cmd o c = if given o
                                  then drush c
                                  else return () in
                 Args {clean = cmd "-clean" ["test-clean"],
                       clear = cmd "-clear" ["cc", "all"],
                       site  = if given "-training"
                                  then "svn-training.wandisco.com"
                                  else "default",
                       tests = parseTests $ filter (('-' /=) . head) args}

parseTests = let parseTest = uncurry Map.singleton . splitOn "::" in
                 Map.unionsWith (++) . map parseTest

getUrl site = let settings = basePath ++ site ++ "/settings.php"
                  req      = "require_once '" ++ settings ++ "';"
                  echo     = "echo isset($base_url)? $base_url : 'f';"
                  strip    = filter (not . isSpace) in
                  do url <- run "php" ["-r", req ++ echo]
                     let stripped = strip url
                     return if stripped == "f"
                               then error "Could not find $base_url"
                               else stripped

main = getArgs >>= parseArgs >>= testRunner

run c args = readProcess c args ""

drush args = let cwd = if runProcess "drush" args (Just basePath

testRunner :: Args -> IO ()
testRunner args = do clear args
                     clean args
                     drush testRunner args
