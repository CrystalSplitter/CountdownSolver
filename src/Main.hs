module Main where

import           CountdownSolver                ( countdownInfixFmt )
import qualified System.Console.GetOpt         as GO
import qualified System.Exit                   as E
import           System.Environment
import           System.IO

data Flag = Help | ReversePolishNotation deriving (Eq, Ord, Enum, Show, Bounded)
data CountdownArgument = CountdownArgument {des :: Int, nums :: [Int]} deriving (Show)

flags =
  [ GO.Option ['h'] ["help"] (GO.NoArg Help) "Print this help message"
  , GO.Option ['r']
              ["rpn"]
              (GO.NoArg ReversePolishNotation)
              "Return output in Reverse Polish Notation"
  ]

usageInfo :: String
usageInfo = GO.usageInfo (unlines [header, desc]) flags ++ "\n" ++ unlines
  ("Examples:\n" : examples)
 where
  header   = "Usage: countdown [-h] DES NUM [NUM ...]"
  desc     = "British gameshow mathematical solver."
  examples = map
    ("  " ++)
    [ "$ countdown 666 25 3 8 1 7 4"
    , "666 = ((25 + 8) * 3 - 4) * 7 + 1"
    , ""
    , "$ countdown 420 69 700 7 28 3 14"
    , "420 = (69 + 28 - 7) * 14 / 3"
    ]

parse :: [String] -> IO CountdownArgument
parse argv = case GO.getOpt GO.Permute flags argv of
  (fls, args, []) -> do
    if Help `elem` fls
      then do
        hPutStrLn stderr usageInfo
        E.exitSuccess
      else do
        return $ CountdownArgument { des  = read (head args)
                                   , nums = map read (tail args)
                                   }
  (_, _, errs) -> do
    hPutStrLn stderr (concat errs ++ usageInfo)
    E.exitWith (E.ExitFailure 1)


main = do
  countdownArg <- getArgs >>= parse
  putStrLn $ countdownInfixFmt (nums countdownArg) (des countdownArg)
