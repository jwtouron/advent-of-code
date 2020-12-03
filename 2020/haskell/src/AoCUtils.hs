module AoCUtils (mkTestCase, mkTest) where

import Control.Monad (when)

mkTestCase :: (Show a, Eq a) => String -> a -> a -> IO ()
mkTestCase msg expected actual = do
  when (expected /= actual) $
       error $ "Failed: " ++ msg ++ ": expected: " ++ show expected ++ ", actual: " ++ show actual

mkTest :: [IO ()] -> IO ()
mkTest testCases = do
  mapM_ id testCases
  putStrLn "All tests passed."
