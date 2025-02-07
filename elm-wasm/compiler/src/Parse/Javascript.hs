{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module Parse.Javascript ( javascript )
where


import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.UTF8 as BS_UTF8
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified AST.Source as Src
import qualified AST.Utils.Javascript as Javascript
import Parse.Primitives (Parser, Row, Col)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- SHADER


javascript :: A.Position -> Parser E.Expr Src.Expr
javascript start@(A.Position row col) =
  do  block <- jsBlockQuote
      jsCode <- parseJavascript row col block
      end <- P.getPosition
      return (A.at start end (Src.Javascript (Javascript.fromChars block)))


-- BLOCK


jsBlockQuote :: Parser E.Expr [Char]
jsBlockQuote =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    let
      !pos4 = plusPtr pos 4
    in
    if pos4 <= end
      && P.unsafeIndex pos == 0x5B {- [ -}
      && P.unsafeIndex (plusPtr pos 1) == 0x6A {- j -}
      && P.unsafeIndex (plusPtr pos 2) == 0x73 {- s -}
      && P.unsafeIndex (plusPtr pos 3) == 0x7C {- | -}
    then
      let
        (# status, newPos, newRow, newCol #) =
          eatBlockQuote pos4 end row (col + 4)
      in
      case status of
        Good ->
          let
            !off = minusPtr pos4 (unsafeForeignPtrToPtr src)
            !len = minusPtr newPos pos4
            !block = BS_UTF8.toString (B.PS src off len)
            !newState = P.State src (plusPtr newPos 2) end indent newRow (newCol + 2)
          in
          cok block newState

        Unending ->
          cerr row col E.EndlessJavascript

    else
      eerr row col E.Start


data Status
  = Good
  | Unending


eatBlockQuote :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> (# Status, Ptr Word8, Row, Col #)
eatBlockQuote pos end row col =
  if pos >= end then
    (# Unending, pos, row, col #)

  else
    let !word = P.unsafeIndex pos in
    if word == 0x007C {- | -} && P.isWord (plusPtr pos 1) end 0x5D {- ] -} then
      (# Good, pos, row, col #)

    else if word == 0x0A {- \n -} then
      eatBlockQuote (plusPtr pos 1) end (row + 1) 1

    else
      let !newPos = plusPtr pos (P.getCharWidth word) in
      eatBlockQuote newPos end row (col + 1)


parseJavascript :: Row -> Col -> [Char] -> Parser E.Expr Src.Expr_
parseJavascript row col block =
  pure (Src.Javascript (Javascript.fromChars block))