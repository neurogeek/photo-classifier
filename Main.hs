module Main 
where

import PhotoClassifier ( processDir )
import Control.Monad
import System.Environment
import System.Console.GetOpt

data Flag = Verbose
        | ProcessDir String
        | OutputDir String
        | StartDate String  
        deriving (Show, Eq)

data Options = Options { optVerbose :: Bool
               , optProcessDir :: String
               , optOutputDir :: String
               , optStartDate :: String }
               deriving Show

defaultOptions = Options { optVerbose = False
                    , optProcessDir = ""
                    , optOutputDir = ""
                    , optStartDate = "" }

options :: [ OptDescr (Options -> IO Options) ]
options = [ Option ['v'] ["verbose"] (NoArg 
    (\opt -> return opt {optVerbose = True})) 
        "Be Verbose.",

    Option ['i'] ["process-dir"] (ReqArg (\arg opt -> 
        return opt {optProcessDir = arg}) "DIR") 
            "Path to the folder to be processed.",

    Option ['s'] ["start-date"] (ReqArg (\arg opt -> 
        return opt {optProcessDir = arg}) "DATE") 
            "Start date to calculate week number. Format: YYYY-MM-DD",

    Option ['o'] ["output-dir"] (ReqArg (\arg opt -> 
        return opt {optOutputDir = arg}) "DIR") "Output directory."]

-- parseArgs
-- Applies getOpt to the command line arguments.
parseArgs :: [String] -> IO ([Options -> IO Options], [String])
parseArgs args =
    case getOpt RequireOrder options args of
        (o,n,[]  ) -> return (o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: photo-classifier [OPTION...]"

main :: IO ()
main = do
    args <- getArgs
    (opts, non_opts) <- parseArgs args
    opts' <- foldl (>>=) (return defaultOptions) opts
    
    processDir (optProcessDir opts') (optOutputDir opts') (optStartDate opts')
