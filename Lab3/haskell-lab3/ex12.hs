import Data.Char
import Data.List

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

formatStr :: [Char] -> [Char]
formatStr s = foldr1 (\w s -> w ++ " " ++ s) .
              map capitalize .
              filter (\x -> length x > 1) $
              words s

prodPrices :: [Char] -> Int
prodPrices p = case p of
  "A" -> 100
  "B" -> 500
  "C" -> 1000
  _ -> error "Unknown product"

products :: [[Char]]
products = ["A", "B", "C"]

discStr1 :: [Char] -> Double
discStr1 p
  | price > 999 = 0.3 * fromIntegral price
  | otherwise = 0.1 * fromIntegral price
  where
    price = prodPrices p

discStr2 :: [Char] -> Double
discStr2 p = 0.2 * fromIntegral (prodPrices p)

totalDiscount :: ([Char] -> Double) -> Double
totalDiscount discStr =
  foldl1 (+) .
  map discStr .
  filter (\p -> prodPrices p > 499)
