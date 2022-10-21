{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module HW3.Parser
  ( parse
  ) where

    import HW3.Base
    import Text.Megaparsec (ParseErrorBundle, Parsec, choice, many, runParser, eof, optional, takeWhileP, between)
    import Text.Megaparsec.Char (char, space1, string, hexDigitChar)
    import Data.Void ( Void )
    import qualified Text.Megaparsec.Char.Lexer as L
    import Data.Maybe (fromMaybe)
    import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixR, InfixL, InfixN))
    import qualified Data.Text as T
    import Data.ByteString (ByteString, pack)
    import Data.Word (Word8)
    import Data.Char ( digitToInt )
    
    
    parse :: String -> Either (ParseErrorBundle String Void) HiExpr
    parse = runParser (newExpr <* eof) ""

    type Parser = Parsec Void String

    sc :: Parser ()
    sc = L.space
      space1
      (L.skipLineComment "//")
      (L.skipBlockComment "/*" "*/")

    funNames :: Parser HiFun
    funNames = choice
      [ HiFunDiv            <$ string "div"
      , HiFunMul            <$ string "mul"
      , HiFunAdd            <$ string "add"
      , HiFunSub            <$ string "sub"
      -- 
      , HiFunNotLessThan    <$ string "not-less-than"
      , HiFunNotGreaterThan <$ string "not-greater-than"
      , HiFunNotEquals      <$ string "not-equals"
      , HiFunNot            <$ string "not"
      , HiFunAnd            <$ string "and"
      , HiFunOr             <$ string "or"
      , HiFunLessThan       <$ string "less-than"
      , HiFunGreaterThan    <$ string "greater-than"
      , HiFunEquals         <$ string "equals"
      , HiFunIf             <$ string "if"
      -- 
      , HiFunLength         <$ string "length"
      , HiFunToUpper        <$ string "to-upper"
      , HiFunToLower        <$ string "to-lower"
      , HiFunReverse        <$ string "reverse"
      , HiFunTrim           <$ string "trim" 
      --
      , HiFunList           <$ string "list"
      , HiFunRange          <$ string "range"
      , HiFunFold           <$ string "fold" 
      -- 
      , HiFunPackBytes      <$ string "pack-bytes"
      , HiFunUnpackBytes    <$ string "unpack-bytes"
      , HiFunEncodeUtf8     <$ string "encode-utf8"
      , HiFunDecodeUtf8     <$ string "decode-utf8"
      , HiFunZip            <$ string "zip"
      , HiFunUnzip          <$ string "unzip"
      , HiFunSerialise      <$ string "serialise"
      , HiFunDeserialise    <$ string "deserialise" 
      -- 
      , HiFunRead           <$ string "read"
      , HiFunWrite          <$ string "write"
      , HiFunMkDir          <$ string "mkdir"
      , HiFunChDir          <$ string "cd" 
      -- 
      , HiFunParseTime      <$ string "parse-time"
      -- 
      , HiFunRand           <$ string "rand" 
      , HiFunEcho           <$ string "echo" ]

    numLiterals :: Parser Rational
    numLiterals = toRational <$> L.signed sc L.scientific

    bool :: Parser Bool
    bool = choice
      [ True  <$ string "true"
      , False <$ string "false" ]

    str :: Parser T.Text
    str = T.pack <$> between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

    parseList :: Parser HiExpr
    parseList =  do
      _   <- char '[' <* sc
      arg <- optional ((:) <$> newExpr <*> many (char ',' <* sc *> newExpr))
      _   <- char ']' <* sc
      pure (HiExprApply (HiExprValue (HiValueFunction HiFunList)) (fromMaybe [] arg))

    parseBytes :: Parser ByteString
    parseBytes = do
      _     <- string "[#" <* sc
      bytes <- optional (many (parseHex <* sc))
      _     <- string "#]" <* sc
      pure $ pack (fromMaybe [] bytes)

    parseHex :: Parser Word8
    parseHex = do
      a <- hexDigitChar
      b <- hexDigitChar
      _ <- sc
      pure (fromIntegral (digitToInt a * 16 + digitToInt b))

    value :: Parser HiValue
    value = choice
      [ HiValueFunction <$> funNames
      , HiValueNumber   <$> numLiterals
      , HiValueBool     <$> bool
      , HiValueNull     <$ string "null"
      , HiValueString   <$> str 
      , HiValueBytes    <$> parseBytes 
      , HiValueAction   <$> (HiActionCwd <$ string "cwd") 
      , HiValueAction   <$> (HiActionNow <$ string "now") 
      ]

    parens :: Parser a -> Parser a
    parens = between (symbol "(" <* sc) (symbol ")")

    expr' :: Parser HiExpr
    expr' = choice
      [ parens newExpr
      , HiExprValue <$> value
      , parseList ]

    expr :: Parser HiExpr
    expr = do
      e        <- sc *> expr' <* sc
      listArgs <- many args
      r        <- optional (char '!') <* sc
      case r of
        Nothing  -> pure (buildExpr e listArgs)
        (Just _) -> pure $ HiExprRun (buildExpr e listArgs)

    args :: Parser [HiExpr]
    args = do
      _   <- char '(' <* sc
      arg <- optional ((:) <$> newExpr <*> many (char ',' <* sc *> newExpr)) <* sc
      _   <- char ')' <* sc
      pure (fromMaybe [] arg)

    buildExpr :: HiExpr -> [[HiExpr]] -> HiExpr
    buildExpr e list = case list of
      []     -> e
      x : xs -> buildExpr (HiExprApply e x) xs

    symbol :: [Char] -> Parser [Char]
    symbol = L.symbol sc

    table :: [[Operator Parser HiExpr]]
    table =
      [ [ binaryL "*" (getFun HiFunMul)
        , binaryL "/" (getFun HiFunDiv) ]
      , [ binaryL "+" (getFun HiFunAdd)
        , binaryL "-" (getFun HiFunSub) ]
      , [ binary "<" (getFun HiFunLessThan)
        , binary ">" (getFun HiFunGreaterThan)
        , binary ">=" (getFun HiFunNotLessThan)
        , binary "<=" (getFun HiFunNotGreaterThan)
        , binary "==" (getFun HiFunEquals)
        , binary "/=" (getFun HiFunNotEquals) ]
      , [ binaryR "&&" (getFun HiFunAnd) ]
      , [ binaryR "||" (getFun HiFunOr) ] ]

    getFun :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
    getFun f e1 e2 = HiExprApply (HiExprValue (HiValueFunction f)) [e1, e2]

    binaryL :: [Char] -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
    binaryL name f = InfixL (f <$ symbol name)
    binary :: [Char] -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
    binary name f = InfixN (f <$ symbol name)
    binaryR :: [Char] -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
    binaryR name f = InfixR (f <$ symbol name)

    newExpr :: Parser HiExpr
    newExpr = makeExprParser (expr <* sc) table