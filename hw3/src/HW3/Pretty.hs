module HW3.Pretty
  ( prettyValue
  ) where

    import Prettyprinter (Doc, Pretty (pretty), slash, (<+>), dquotes)
    import Prettyprinter.Render.Terminal (AnsiStyle)
    import HW3.Base
    import Data.Scientific
    import GHC.Real (Ratio((:%)))
    import Data.Text
    import Data.Foldable (toList)
    import qualified Data.List as L
    import qualified Data.ByteString as B
    import Numeric (showHex)

    primesSGT :: [Integer]
    primesSGT = 2 : ops
        where
        ops = 3 : [n | (r:q:_, px) <- (L.zip . L.tails . (3:) . L.map (\x ->x*x)) ops (L.inits ops), 
                      n <- [r+2,r+4..q-2],  L.all ((> 0) . rem n) px] 

    pd:: Integer -> [Integer]
    pd n = [i | i <- L.takeWhile (<n) primesSGT,  (n `mod` i) == 0] 

    pnd :: Integer -> Int
    pnd = L.length . pd 

    prettyValue :: HiValue -> Doc AnsiStyle
    prettyValue (HiValueFunction HiFunAdd)            = pretty "add"
    prettyValue (HiValueFunction HiFunSub)            = pretty "sub"
    prettyValue (HiValueFunction HiFunMul)            = pretty "mul"
    prettyValue (HiValueFunction HiFunDiv)            = pretty "div"
    prettyValue (HiValueNumber (a :% b)) 
      | b == 1                                 = pretty a
      | (even b && mod b 5 == 0 && pnd b == 2) 
        || (even b && pnd b == 1)
        || (mod b 5 == 0 && pnd b == 2) 
        || b == 2 
        || b == 5                              = pretty . show . fst $ fromRationalRepetendUnlimited (a :% b)
      | abs a < b                              = pretty a <> slash <> pretty b
      | otherwise                              =
        let (c, r) = quotRem (abs a) b in
          if a > 0 
            then pretty c <+> pretty '+' <+> pretty r <> slash <> pretty b
            else pretty '-' <> pretty c <+> pretty '-' <+> pretty r <> slash <> pretty b
    prettyValue (HiValueFunction HiFunNot)            = pretty "not"
    prettyValue (HiValueFunction HiFunAnd)            = pretty "and"
    prettyValue (HiValueFunction HiFunOr)             = pretty "or"
    prettyValue (HiValueFunction HiFunLessThan)       = pretty "less-that"
    prettyValue (HiValueFunction HiFunGreaterThan)    = pretty "greater-that"
    prettyValue (HiValueFunction HiFunEquals)         = pretty "equal"
    prettyValue (HiValueFunction HiFunNotLessThan)    = pretty "not-less-that"
    prettyValue (HiValueFunction HiFunNotGreaterThan) = pretty "not-greater-that"
    prettyValue (HiValueFunction HiFunNotEquals)      = pretty "not-equal"
    prettyValue (HiValueFunction HiFunIf)             = pretty "if"
    prettyValue (HiValueBool True)                    = pretty "true"
    prettyValue (HiValueBool False)                   = pretty "false"
    prettyValue (HiValueFunction HiFunLength)         = pretty "length"
    prettyValue (HiValueFunction HiFunToUpper)        = pretty "to-upper"
    prettyValue (HiValueFunction HiFunToLower)        = pretty "to-lower"
    prettyValue (HiValueFunction HiFunReverse)        = pretty "reverse"
    prettyValue (HiValueFunction HiFunTrim)           = pretty "trim"
    prettyValue HiValueNull                           = pretty "null"
    prettyValue (HiValueString s)                     = dquotes (pretty (unpack s))
    prettyValue (HiValueFunction HiFunList)           = pretty "list"
    prettyValue (HiValueFunction HiFunRange)          = pretty "range"
    prettyValue (HiValueFunction HiFunFold)           = pretty "fold"
    prettyValue (HiValueList s)                       = 
      pretty "[" <+> pretty (L.intercalate ", " (L.map (show . prettyValue) (toList s))) <+> pretty "]" 
    prettyValue (HiValueFunction HiFunPackBytes)      = pretty "pack-bytes"
    prettyValue (HiValueFunction HiFunUnpackBytes)    = pretty "unpack-bytes"
    prettyValue (HiValueFunction HiFunEncodeUtf8)     = pretty "encode-utf8"
    prettyValue (HiValueFunction HiFunDecodeUtf8)     = pretty "decode-utf8"
    prettyValue (HiValueFunction HiFunZip)            = pretty "zip"
    prettyValue (HiValueFunction HiFunUnzip)          = pretty "unzip"
    prettyValue (HiValueFunction HiFunSerialise)      = pretty "serialise"
    prettyValue (HiValueFunction HiFunDeserialise)    = pretty "deserialise"
    prettyValue (HiValueBytes b)                      = 
      pretty ("[# " 
        ++ L.unwords (L.map (\x ->
            let res = x `showHex` ""
            in if Prelude.length res == 1
                  then "0" ++ res
                  else res) $ B.unpack b) 
        ++ " #]")
    prettyValue (HiValueFunction HiFunRead)           = pretty "read"
    prettyValue (HiValueFunction HiFunWrite)          = pretty "write"
    prettyValue (HiValueFunction HiFunMkDir)          = pretty "mkdir"
    prettyValue (HiValueFunction HiFunChDir)          = pretty "cd"
    prettyValue (HiValueAction (HiActionRead path))   = pretty "read(\"" <> pretty path <> pretty "\")"
    prettyValue (HiValueAction (HiActionWrite path b))= 
      pretty "write(\"" <> pretty path <> pretty ", " <> prettyValue (HiValueBytes b) <> pretty "\")"
    prettyValue (HiValueAction (HiActionMkDir path))  = pretty "mkdir(\"" <> pretty path <>  pretty "\")"
    prettyValue (HiValueAction (HiActionChDir path))  = pretty "cd(\"" <> pretty path <>  pretty "\")"
    prettyValue (HiValueAction HiActionCwd)           = pretty "cwd"
    prettyValue (HiValueFunction HiFunParseTime)      = pretty "parse-time"
    prettyValue (HiValueTime t)                       = pretty "parse-time(\"" <> pretty (show t) <> pretty "\")"
    prettyValue (HiValueAction HiActionNow)           = pretty "now"
    prettyValue (HiValueFunction HiFunRand)           = pretty "rand"
    prettyValue (HiValueAction (HiActionRand a b))    = 
      pretty "rand(" <> pretty a <> pretty ", " <> pretty b  <> pretty ")"
    prettyValue (HiValueFunction HiFunEcho)           = pretty "echo"
    prettyValue (HiValueAction (HiActionEcho t))      = pretty "echo(\"" <> pretty t <> pretty "\")"

    -- devisors :: Set Int -> Int -> Int -> Set Int
    -- devisors ans a b | a < b = ans
    -- devisors curAns a b = 
    --   if mod b a == 0
    --     then devisors (insert a curAns) (a + 1) b
    --     else devisors curAns (a + 1) b

    -- decimal :: Int -> Bool
    -- decimal a = size (delete 2 (delete 5 (devisors S.empty 2 a))) == 0

    -- pnd :: (Integral a, Num t) => a -> t
    -- pnd n = hpnd n n 2 0
    --     where hpnd s n2 k c | k >= s         = c
    --                         | n `mod` k == 0 = hpnd s (d n2 k) (k+1) (c+1)
    --                         | otherwise      = hpnd s n2 (k+1) c
    --           d n2 k        | n `mod` k == 0 = d (n `div` k) k
    --                         | otherwise      = n2

    -- -- Бесконечный список простых чисел
    -- primesSGT :: [Integer]
    -- primesSGT = 2 : ops
    --     where
    --     ops = 3 : [n | (r : q : _, px) <- (L.zip . tails . (3:) . L.map (\x ->x*x)) ops (inits ops),
    --                   n <- [r+2,r+4..q-2],  L.all ((> 0) . rem n) px]

    -- -- Список простых делителей заданного числа
    -- pd:: Integer -> [Integer]
    -- pd n = [i | i <- L.takeWhile (<n) $ primesSGT,  (n `mod` i) == 0]

    -- -- Написать функцию, вычисляющую число простых делителей числа pnd(x).
    -- pnd :: Integer -> Int 
    -- pnd = L.length . pd