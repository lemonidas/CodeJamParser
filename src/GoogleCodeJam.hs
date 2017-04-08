{-# LANGUAGE TupleSections #-}
module GoogleCodeJam where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lex.Fractional as BSD
import Data.Char (isSpace)
import Text.Printf

import Data.IORef
import Data.Maybe

import Control.Monad
import Control.Applicative
import Control.Arrow

type Parser a = BSC.ByteString -> (a, BSC.ByteString)

readInput :: IO (IORef BSC.ByteString)
readInput = BS.getContents >>= newIORef 

int :: Parser Int
int = second (BSC.dropWhile isSpace) . fromJust . BSC.readInt

integer :: Parser Integer
integer = second (BSC.dropWhile isSpace) . fromJust . BSC.readInteger

int2 :: Parser (Int, Int)
int2 = uncurry (first . (,)) . second int . int

int3 :: Parser (Int, Int, Int)
int3 = uncurry (first . (uncurry (,,))) . second int . int2

double :: Parser Double
double = second (BSC.dropWhile isSpace) . fromJust . BSD.readDecimal

char :: Parser Char
char = BSC.head &&& BSC.tail

letter :: Parser Char
letter = second (BSC.dropWhile isSpace) . char 

string :: Parser String
string = (BSC.unpack *** BSC.dropWhile isSpace) . BSC.span (not . isSpace)

bytestring :: Parser BSC.ByteString 
bytestring = (second $ BSC.dropWhile isSpace) . BSC.span (not . isSpace)

readMany :: Parser a -> Int -> Parser [a]
readMany f n bs = first reverse $ 
                  foldr (\_ (l,bs) -> first (:l) $ f bs) ([],bs) [1..n]

intMany :: Int -> Parser [Int]
intMany = readMany int

next :: IORef BSC.ByteString -> Parser a -> IO a
next input parse = do
  bs <- readIORef input
  let (x, bs') = parse bs
  writeIORef input bs'
  return x
