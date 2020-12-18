{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day18 (test, solve) where

import AoCUtils
import Control.Applicative
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as TextIO
import Text.Earley

data Expr where
  ESum :: Expr -> Expr -> Expr
  EProd :: Expr -> Expr -> Expr
  ENum :: !Int -> Expr

deriving instance Show Expr

skipSpace :: Prod r e Char String
skipSpace = many (satisfy (== ' '))

eval :: Expr -> Int
eval (ESum e1 e2) = eval e1 + eval e2
eval (EProd e1 e2) = eval e1 * eval e2
eval (ENum n) = n

input :: IO [Text]
input = Text.lines <$> TextIO.readFile "input/day18.txt"

solve' :: (Text -> Expr) -> [Text] -> Int
solve' parse = sum . map (eval . parse)

parseExprPart1 :: Text -> Expr
parseExprPart1 = head . fst . fullParses (parser grammar)
  where
    grammar = mdo
      expr <- rule $ parens <|> num <|> sum <|> prod
      num <- rule $ ENum . read <$> some (satisfy isDigit)
      sum <- rule $ ESum <$> expr <* skipSpace <* token '+' <* skipSpace <*> (num <|> parens)
      prod <- rule $ EProd <$> expr <* skipSpace <* token '*' <* skipSpace <*> (num <|> parens)
      parens <- rule $ token '(' *> skipSpace *> expr <* skipSpace <* token ')'
      pure expr

part1 :: [Text] -> Int
part1 = solve' parseExprPart1

parseExprPart2 :: Text -> Expr
parseExprPart2 = head . fst . fullParses (parser grammar)
  where
    grammar = mdo
      let notProd = num <|> sum <|> parens
      expr <- rule $ parens <|> num <|> sum <|> prod
      num <- rule $ ENum . read <$> some (satisfy isDigit)
      sum <- rule $ ESum <$> notProd <* skipSpace <* token '+' <* skipSpace <*> notProd
      prod <- rule $ EProd <$> notProd <* skipSpace <* token '*' <* skipSpace <*> expr
      parens <- rule $ token '(' *> skipSpace *> expr <* skipSpace <* token ')'
      pure expr

part2 :: [Text] -> Int
part2 = solve' parseExprPart2

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "parseExpr 2 * 3 + (4 * 5)" 26 (eval $ parseExprPart1 "2 * 3 + (4 * 5)")
         , mkTestCase "parseExprPart1 5 + (8 * 3 + 9 + 3 * 4 * 3)"
                      437
                      (eval $ parseExprPart1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
         , mkTestCase "parseExprPart1 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
                      12240
                      (eval $ parseExprPart1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
         , mkTestCase "parseExprPart1 ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
                      13632
                      (eval $ parseExprPart1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
         , mkTestCase "part1 input" 25190263477788 (part1 input')
         , mkTestCase "part2 input" 297139939002972 (part2 input')
         ]
--test
solve :: IO ()
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
