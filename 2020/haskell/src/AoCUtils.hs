module AoCUtils
  ( ifM
  , mkTest
  , mkTestCase
  ) where

import Control.Monad (when)

mkTestCase :: (Show a, Eq a) => String -> a -> a -> IO ()
mkTestCase msg expected actual = do
  when (expected /= actual) $
       error $ "Failed: " ++ msg ++ ": expected: " ++ show expected ++ ", actual: " ++ show actual

mkTest :: [IO ()] -> IO ()
mkTest testCases = do
  mapM_ id testCases
  putStrLn "All tests passed."

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f = p >>= \b -> if b then t else f
