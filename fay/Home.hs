{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Home where

import           FFIExample
import           FFI       (Nullable(..))
import           DOM
import           Data.Text (fromString)
import qualified Data.Text as T
import           Fay.Yesod
import           Prelude
import           SharedTypes

chr :: Int -> Char
chr = ffi "String.fromCharCode(%1)"

ord :: Char -> Int
ord = ffi "%1.charCodeAt(0)"

--thing :: Text -> Int
--thing t = map
--    where blah a = let b = (ord a) - 48 in
--                   if b < 0 then 0
--                   else if b > 9 then 0
--                        else b
--          rec 

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: Fay ()
main = do
    input <- getElementById "fibindex"
    result <- getElementById "fibresult"
    onKeyUp input $ do
        indexS <- getValue input
        index <- parseInt indexS
        setInnerHTML result $ T.pack $ show $fib index
--        if null indexS || (not $ all (\c -> let a = ord c in ((a >= 48) && (a <= 57))))
--        then call (GetFib 0) $ setInnerHTML result
--        else do
--          index <- parseInt indexS
--          case index of
--            Null -> call (GetFib 0) $ setInnerHTML result
--            _ -> call (GetFib index) $ setInnerHTML result

--        index <- parseInt indexS
--         case index of
--           Nullable i -> call (GetFib i) $ setInnerHTML result
--           _ -> call (GetFib 101) $ setInnerHTML result
