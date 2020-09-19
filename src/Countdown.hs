module Countdown
  ( CountdownOp
  , Component
  , countdown
  , countdownFmt
  , countdownInfixFmt
  )
where

import qualified Data.List                     as L
import qualified Data.Maybe                    as M


data CountdownOp = CDPlus | CDMinus | CDMultiply | CDDivide deriving (Show)
data Component = Component { operator :: CountdownOp, value :: Int } deriving (Show)
newtype Tree = Node (Either Bool [(Component, Tree)]) deriving (Show)


precedence :: CountdownOp -> Int
precedence CDPlus     = 1
precedence CDMinus    = 1
precedence CDMultiply = 2
precedence CDDivide   = 2

applyOp :: Int -> Component -> Maybe Int
applyOp cur (Component CDPlus     n) = Just $ cur + n
applyOp cur (Component CDMinus    n) = Just $ cur - n
applyOp cur (Component CDMultiply n) = Just $ cur * n
applyOp cur (Component CDDivide n) =
  if cur `mod` n == 0 then Just $ cur `div` n else Nothing


-- Returns a list of Components and their result if applied to the current number.
getValidApplications :: Int -> [Int] -> [(Component, Int)]
getValidApplications _ [] = []
getValidApplications cur nums =
  let ops        = [CDPlus, CDMinus, CDMultiply, CDDivide]
      components = [ Component op n | op <- ops, n <- nums ]
      isValidStep c = maybe False (> 0) (applyOp cur c)
  in  [ (c, M.fromMaybe 0 (applyOp cur c))
      | c <- components
      , isValidStep c
      , M.isJust $ applyOp cur c
      ]


-- | Generate a recursive Tree structure based on the provided
-- number list, the desired value, and the current value achieved.
--
-- ==== __Examples__
-- >>> countdownTree [1, 2] 2 1
-- Node (Right [(Component {operator = CDPlus, value = 1},Node (Right [(Component {operator = CDPlus, value = 2},Node (Left False)),(Component {operator = CDMultiply, value = 2},Node (Left True))])),(Component {operator = CDPlus, value = 2},Node (Left True))])
--
countdownTree :: [Int] -> Int -> Int -> Tree
countdownTree xs des cur
  | des == cur
  = Node (Left True)
  | null xs && des /= cur
  = Node (Left False)
  | otherwise
  = let
      applications = getValidApplications cur xs
      partialHelper c nextCur =
        countdownTree (L.delete (value c) xs) des nextCur
      children =
        [ (c, partialHelper c nextCur) | (c, nextCur) <- applications ]
    in
      Node (Right children)


countdownTraversal :: Tree -> Maybe [Component]
countdownTraversal (Node (Left x)) = if x then Just [] else Nothing
countdownTraversal (Node (Right xs)) =
  let paths =
          [ c : M.fromJust (countdownTraversal t)
          | (c, t) <- xs
          , M.isJust (countdownTraversal t)
          ]
  in  case paths of
        [] -> Nothing
        _  -> Just $ head paths

-- | Produce components which, when applied in order on the number 0, will
-- reach the desired number (the second argument). Components are generated
-- from the possible operations which can be applied using the numbers in
-- the first argument.
--
-- ==== __Examples__
--
-- >>> countdown [102, 5, 7, 9, 2] 300
-- Just [Component {operator = CDPlus, value = 102},Component {operator = CDDivide, value = 2},Component {operator = CDPlus, value = 9},Component {operator = CDMultiply, value = 5}]
--
-- >>> countdown [10] 20
-- Nothing
--
countdown :: [Int] -> Int -> Maybe [Component]
countdown nums des = countdownTraversal $ countdownTree nums des 0

countdownFmtHelper :: Maybe [Component] -> [Char]
countdownFmtHelper Nothing   = "NO SOLUTION"
countdownFmtHelper (Just []) = ""
countdownFmtHelper (Just solution) =
  let x  = head solution
      xs = tail solution
      v  = value x
      op = operator x
  in  show v
        ++ ' '
        :  (case op of
             CDPlus     -> "+ "
             CDMinus    -> "- "
             CDMultiply -> "* "
             CDDivide   -> "/ "
           )
        ++ countdownFmtHelper (Just xs)


-- | Produce a countdown solution in Reverse Polish Notation format as a [Char]
-- >>> countdownFmt [50, 101, 7, 6, 2, 10] 720
-- "RPN => 0 50 + 101 + 7 - 10 * 2 / "
countdownFmt :: [Int] -> Int -> [Char]
countdownFmt nums des = "RPN => 0 " ++ countdownFmtHelper (countdown nums des)


operatorChar :: CountdownOp -> Char
operatorChar CDPlus     = '+'
operatorChar CDMinus    = '-'
operatorChar CDMultiply = '*'
operatorChar CDDivide   = '/'

componentOpChar :: Component -> [Char]
componentOpChar x = [operatorChar $ operator x]


countdownInfixFmtHelper :: [Component] -> Maybe CountdownOp -> [Char]
countdownInfixFmtHelper []  _ = ""
countdownInfixFmtHelper [x] _ = show $ value x
countdownInfixFmtHelper (x : xs) Nothing =
  countdownInfixFmtHelper xs (Just $ operator x)
    ++ " "
    ++ (componentOpChar x)
    ++ " "
    ++ show (value x)
countdownInfixFmtHelper (x : xs) (Just prevOp)
  | (precedence (operator x)) >= (precedence prevOp)
  = countdownInfixFmtHelper xs (Just $ operator x)
    ++ " "
    ++ componentOpChar x
    ++ " "
    ++ show (value x)
  | otherwise
  = "("
    ++ countdownInfixFmtHelper xs (Just $ operator x)
    ++ " "
    ++ componentOpChar x
    ++ " "
    ++ show (value x)
    ++ ")"


countdownInfixFmt :: [Int] -> Int -> [Char]
countdownInfixFmt nums des =
  let solution = countdown nums des
  in  maybe
        "NO SOLUTION"
        (\x -> show des ++ " = " ++ countdownInfixFmtHelper (reverse x) Nothing)
        solution

