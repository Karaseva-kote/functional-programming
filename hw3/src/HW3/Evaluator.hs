module HW3.Evaluator
  ( eval
  ) where

    import HW3.Base
    import Control.Monad.Except
      ( runExceptT
      , MonadError(throwError)
      , ExceptT (ExceptT)
      )
    import qualified Data.Text as T
    import GHC.Real (Ratio((:%)))
    import Data.Semigroup (stimes)
    import qualified Data.Sequence.Internal as S
    import Data.Sequence.Internal ((|>), (><), index)
    import Data.Foldable ( Foldable(toList) )
    import Data.Word (Word8)
    import qualified Data.ByteString as B
    import Data.Text.Encoding (decodeUtf8', encodeUtf8)
    import Codec.Compression.Zlib
    import Data.ByteString.Lazy.Char8 (fromStrict, toStrict)
    import Codec.Serialise (serialise, deserialise)
    import Data.Text.Encoding.Error (UnicodeException)
    import Text.Read (readMaybe)
    import Data.Time (UTCTime, addUTCTime, diffUTCTime)

    eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
    eval = runExceptT . eval'

    eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
    eval' (HiExprValue val)       = return val
    eval' (HiExprApply expr list) = do
      fun <- eval' expr
      tryApplyFun fun list
    eval' (HiExprRun e)           = applyUnaryFun getRunFun e

    getRunFun :: HiMonad m => HiValue -> ExceptT HiError m HiValue
    getRunFun (HiValueAction act) = ExceptT $ Right <$> runAction act
    getRunFun _                   = throwError HiErrorInvalidArgument

    tryApplyFun :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
    tryApplyFun (HiValueNumber _) _ = throwError HiErrorInvalidFunction
    tryApplyFun (HiValueBool _) _   = throwError HiErrorInvalidFunction
    tryApplyFun (HiValueAction _) _ = throwError HiErrorInvalidFunction
    tryApplyFun HiValueNull _       = throwError HiErrorInvalidFunction

    tryApplyFun (HiValueFunction HiFunDiv) (a : [b]) = applyFun (getFun "/") (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunAdd) (a : [b]) = applyFun (getFun "+") (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunSub) (a : [b]) = applyFun (getFun "-") (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunMul) (a : [b]) = applyFun (getFun "*") (eval' a) [b]

    tryApplyFun (HiValueFunction HiFunNot) [x]                  = applyUnaryFun notFun x
    tryApplyFun (HiValueFunction HiFunAnd) (a : [b])            = applyLazyFun (eval' a) [b] falseNull
    tryApplyFun (HiValueFunction HiFunOr) (a : [b])             = applyLazyFun (eval' a) [b] (not . falseNull)
    tryApplyFun (HiValueFunction HiFunLessThan) (a : [b])       = applyFun (getLessFun True) (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunGreaterThan) (a : [b])    = applyFun (getGreatFun True) (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunEquals) (a : [b])         = applyFun (getEqualFun True) (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunNotLessThan) (a : [b])    = applyFun (getLessFun False) (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunNotGreaterThan) (a : [b]) = applyFun (getGreatFun False) (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunNotEquals) (a : [b])      = applyFun (getEqualFun False) (eval' a) [b]

    tryApplyFun (HiValueFunction HiFunIf) (x1 : (x2 : [x3])) = do
      valx1 <- eval' x1
      case valx1 of
        HiValueBool True  -> eval' x2
        HiValueBool False -> eval' x3
        _                 -> throwError HiErrorInvalidArgument

    tryApplyFun (HiValueFunction HiFunLength) [x]  = applyUnaryFun (getStringFun "length") x
    tryApplyFun (HiValueFunction HiFunToUpper) [x] = applyUnaryFun (getStringFun "upper") x
    tryApplyFun (HiValueFunction HiFunToLower) [x] = applyUnaryFun (getStringFun "lower") x
    tryApplyFun (HiValueFunction HiFunReverse) [x] = applyUnaryFun (getStringFun "reverse") x
    tryApplyFun (HiValueFunction HiFunTrim) [x]    = applyUnaryFun (getStringFun "trim") x

    tryApplyFun (HiValueString s) [x]       =
      applyUnaryFun (getIndexFun T.length T.index T.singleton HiValueString s) x
    tryApplyFun (HiValueString s) (a : [b]) =
      applyFun (getSplitFun T.length T.drop T.take HiValueString s) (eval' a) [b]

    tryApplyFun (HiValueFunction HiFunList) x = applyFun getList (return (HiValueList S.empty)) x
    tryApplyFun (HiValueFunction HiFunRange) (a : [b]) = applyFun getRange (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunFold) (a : [b])  = applyFun getFoldFun (eval' a) [b]

    tryApplyFun (HiValueList s) [x]       =
      applyUnaryFun (getIndexFun S.length S.index S.singleton (`index` 0) s) x
    tryApplyFun (HiValueList s) (a : [b]) =
      applyFun (getSplitFun S.length S.drop S.take HiValueList s) (eval' a) [b]

    tryApplyFun (HiValueFunction HiFunPackBytes) [x]   = applyUnaryFun (getByteFun "pack") x
    tryApplyFun (HiValueFunction HiFunUnpackBytes) [x] = applyUnaryFun (getByteFun "unpack") x
    tryApplyFun (HiValueFunction HiFunEncodeUtf8) [x]  = applyUnaryFun (getByteFun "encode") x
    tryApplyFun (HiValueFunction HiFunDecodeUtf8) [x]  = applyUnaryFun (getByteFun "decode") x
    tryApplyFun (HiValueFunction HiFunZip) [x]         = applyUnaryFun (getByteFun "zip") x
    tryApplyFun (HiValueFunction HiFunUnzip) [x]       = applyUnaryFun (getByteFun "unzip") x
    tryApplyFun (HiValueFunction HiFunSerialise) [x]   = applyUnaryFun (getByteFun "serialise") x
    tryApplyFun (HiValueFunction HiFunDeserialise) [x] = applyUnaryFun (getByteFun "deserialise") x

    tryApplyFun (HiValueBytes s) [x]       =
      applyUnaryFun (getIndexFun B.length B.index B.singleton (HiValueNumber . toRational . B.head) s) x
    tryApplyFun (HiValueBytes s) (a : [b]) =
      applyFun (getSplitFun B.length B.drop B.take HiValueBytes s) (eval' a) [b]

    tryApplyFun (HiValueFunction HiFunRead) [x]        = applyUnaryFun (getAction (HiActionRead . T.unpack)) x
    tryApplyFun (HiValueFunction HiFunWrite) (a : [b]) = applyFun getWrite (eval' a) [b]
    tryApplyFun (HiValueFunction HiFunMkDir) [x]       = applyUnaryFun (getAction (HiActionMkDir . T.unpack)) x
    tryApplyFun (HiValueFunction HiFunChDir) [x]       = applyUnaryFun (getAction (HiActionChDir . T.unpack)) x

    tryApplyFun (HiValueFunction HiFunParseTime) [x]   = applyUnaryFun readTime x

    tryApplyFun (HiValueFunction HiFunRand) (a : [b])  = applyFun getRand (eval' a) [b]

    tryApplyFun (HiValueFunction HiFunEcho) [x]        = applyUnaryFun (getAction HiActionEcho) x

    tryApplyFun _ _                                    = throwError HiErrorArityMismatch

    applyFun :: HiMonad m
      => (HiValue -> HiValue -> ExceptT HiError m HiValue)
      -> ExceptT HiError m HiValue
      -> [HiExpr]
      -> ExceptT HiError m HiValue
    applyFun _ ans []            = ans
    applyFun fun curAns (x : xs) = do
      first  <- curAns
      second <- eval' x
      applyFun fun (fun first second) xs

    applyLazyFun :: HiMonad m
      => ExceptT HiError m HiValue
      -> [HiExpr]
      -> (HiValue -> Bool)
      -> ExceptT HiError m HiValue
    applyLazyFun ans [] _             = ans
    applyLazyFun curAns (x : xs) stop = do
      first  <- curAns
      if stop first 
        then return first
        else do
          second <- eval' x
          applyLazyFun (return second) xs stop

    falseNull :: HiValue -> Bool
    falseNull (HiValueBool False) = True
    falseNull HiValueNull         = True
    falseNull _                   = False

    applyUnaryFun :: HiMonad m
      => (HiValue -> ExceptT HiError m HiValue)
      -> HiExpr
      -> ExceptT HiError m HiValue
    applyUnaryFun fun x = do
      res <- eval' x
      fun res

    getFun :: Monad m => [Char] -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
    getFun "/" (HiValueNumber _) (HiValueNumber 0) = throwError HiErrorDivideByZero
    getFun "/" (HiValueNumber a) (HiValueNumber b) = return (HiValueNumber (a / b))
    getFun "/" (HiValueString a) (HiValueString b) =
      return (HiValueString $ T.intercalate (T.singleton '/') [a, b])

    getFun "+" (HiValueNumber a) (HiValueNumber b) = return (HiValueNumber (a + b))
    getFun "+" (HiValueString a) (HiValueString b) = return (HiValueString (T.append a b))
    getFun "+" (HiValueList a) (HiValueList b)     = return (HiValueList (a >< b))
    getFun "+" (HiValueBytes a) (HiValueBytes b)   = return (HiValueBytes (B.append a b))
    getFun "+" (HiValueTime t) (HiValueNumber a)   = return (HiValueTime (addUTCTime (realToFrac a) t))
    getFun "+" (HiValueNumber a) (HiValueTime t)   = return (HiValueTime (addUTCTime (realToFrac a) t))

    getFun "-" (HiValueNumber a) (HiValueNumber b) = return (HiValueNumber (a - b))
    getFun "-" (HiValueTime a) (HiValueTime b)     = return (HiValueNumber $ toRational (diffUTCTime a b))

    getFun "*" (HiValueNumber a) (HiValueNumber b)                = return (HiValueNumber (a * b))
    getFun "*" (HiValueString a) (HiValueNumber (b :% 1)) | b > 0 = return (HiValueString (stimes b a))
    getFun "*" (HiValueNumber (a :% 1)) (HiValueString b) | a > 0 = return (HiValueString (stimes a b))
    getFun "*" (HiValueList a) (HiValueNumber (b :% 1))   | b > 0 = return (HiValueList (stimes b a))
    getFun "*" (HiValueNumber (a :% 1)) (HiValueList b)   | a > 0 = return (HiValueList (stimes a b))
    getFun "*" (HiValueBytes a) (HiValueNumber (b :% 1))  | b > 0 = return (HiValueBytes (stimes b a))
    getFun "*" (HiValueNumber (a :% 1)) (HiValueBytes b)  | a > 0 = return (HiValueBytes (stimes a b))

    getFun _ _ _ = throwError HiErrorInvalidArgument

    notFun :: Monad m => HiValue -> ExceptT HiError m HiValue
    notFun (HiValueBool x) = return (HiValueBool (not x))
    notFun _               = throwError HiErrorInvalidArgument

    getLessFun :: Monad m => Bool -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
    getLessFun f (HiValueBool a) (HiValueBool b)         = return $ HiValueBool (f == (a < b))
    getLessFun f (HiValueNumber a) (HiValueNumber b)     = return $ HiValueBool (f == (a < b))
    getLessFun f (HiValueFunction a) (HiValueFunction b) = return $ HiValueBool (f == (a < b))
    getLessFun f (HiValueString a) (HiValueString b)     = return $ HiValueBool (f == (a < b))
    getLessFun f (HiValueBool _) (HiValueNumber _)       = return $ HiValueBool f
    getLessFun f (HiValueNumber _) (HiValueBool _)       = return $ HiValueBool (not f)
    getLessFun _ _ _                                     = throwError HiErrorInvalidArgument

    getGreatFun :: Monad m => Bool -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
    getGreatFun f (HiValueBool a) (HiValueBool b)         = return $ HiValueBool (f == (a > b))
    getGreatFun f (HiValueNumber a) (HiValueNumber b)     = return $ HiValueBool (f == (a > b))
    getGreatFun f (HiValueFunction a) (HiValueFunction b) = return $ HiValueBool (f == (a > b))
    getGreatFun f (HiValueString a) (HiValueString b)     = return $ HiValueBool (f == (a > b))
    getGreatFun f (HiValueBool _) (HiValueNumber _)       = return $ HiValueBool (not f)
    getGreatFun f (HiValueNumber _) (HiValueBool _)       = return $ HiValueBool f
    getGreatFun _ _ _                                     = throwError HiErrorInvalidArgument

    getEqualFun :: Monad m => Bool -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
    getEqualFun f (HiValueBool a) (HiValueBool b)         = return $ HiValueBool (f == (a == b))
    getEqualFun f (HiValueNumber a) (HiValueNumber b)     = return $ HiValueBool (f == (a == b))
    getEqualFun f (HiValueFunction a) (HiValueFunction b) = return $ HiValueBool (f == (a == b))
    getEqualFun f (HiValueString a) (HiValueString b)     = return $ HiValueBool (f == (a == b))
    getEqualFun f HiValueNull HiValueNull                 = return $ HiValueBool f
    getEqualFun f (HiValueList a) (HiValueList b)         = return $ HiValueBool (f == (a == b))
    getEqualFun f (HiValueBytes a) (HiValueBytes b)       = return $ HiValueBool (f == (a == b))
    getEqualFun f (HiValueAction a) (HiValueAction b)     = return $ HiValueBool (f == (a == b))
    getEqualFun f (HiValueTime a) (HiValueTime b)         = return $ HiValueBool (f == (a == b))
    getEqualFun f _ _                                     = return $ HiValueBool (not f)

    getStringFun :: Monad m => [Char] -> (HiValue -> ExceptT HiError m HiValue)
    getStringFun "length" (HiValueString s)  = return (HiValueNumber (toRational (T.length s)))
    getStringFun "length" (HiValueList s)    = return (HiValueNumber (toRational (S.length s)))
    getStringFun "length" (HiValueBytes s)   = return (HiValueNumber (toRational (B.length s)))
    getStringFun "upper" (HiValueString s)   = return (HiValueString (T.toUpper s))
    getStringFun "lower" (HiValueString s)   = return (HiValueString (T.toLower s))
    getStringFun "reverse" (HiValueString s) = return (HiValueString (T.reverse s))
    getStringFun "reverse" (HiValueList s)   = return (HiValueList (S.reverse s))
    getStringFun "reverse" (HiValueBytes s)  = return (HiValueBytes (B.pack $ reverse $ B.unpack s))
    getStringFun "trim" (HiValueString s)    = return (HiValueString (T.strip s))
    getStringFun _ _                         = throwError HiErrorInvalidArgument

    getIndexFun :: Monad m
      => (t -> Int)
      -> (t -> Int -> a)
      -> (a -> t)
      -> (t -> HiValue)
      -> t
      -> (HiValue -> ExceptT HiError m HiValue)
    getIndexFun len ind toType toVal s (HiValueNumber (a :% 1)) | 0 <= a && fromIntegral a < len s =
      return $ toVal $ toType $ ind s $ fromIntegral a
    getIndexFun _ _ _ _ _ (HiValueNumber (_ :% 1)) = return HiValueNull
    getIndexFun _ _ _ _ _ _                        = throwError HiErrorInvalidArgument

    getSplitFun :: Monad m
      => (t -> Int)
      -> (Int -> t -> t)
      -> (Int -> t -> t)
      -> (t -> HiValue)
      -> t
      -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
    getSplitFun len drop' take' toVal s HiValueNull (HiValueNumber (a :% 1))              =
      split 0 (fromIntegral a) len drop' take' toVal s
    getSplitFun len drop' take' toVal s (HiValueNumber (a :% 1)) HiValueNull              =
      split (fromIntegral a) (len s) len drop' take' toVal s
    getSplitFun len drop' take' toVal s (HiValueNumber (a :% 1)) (HiValueNumber (b :% 1)) =
      split (fromIntegral a) (fromIntegral b) len drop' take' toVal s
    getSplitFun len drop' take' toVal s HiValueNull HiValueNull                           =
      split 0 (len s) len drop' take' toVal s
    getSplitFun _ _ _ _ _ _ _                           = throwError HiErrorInvalidArgument

    split :: Monad m
      => Int
      -> Int
      -> (t -> Int)
      -> (Int -> t -> t)
      -> (Int -> t -> t)
      -> (t -> HiValue)
      -> (t -> ExceptT HiError m HiValue)
    split a b len drop' take' toVal s
      | a >= 0 && b >= 0 = return $ toVal $ split' a b drop' take' s
      | a < 0 && b >= 0  = return $ toVal $ split' (len s + a) b drop' take' s
      | a >= 0 && b < 0  = return $ toVal $ split' a (len s + b) drop' take' s
      | a < 0 && b < 0   = return $ toVal $ split' (len s + a) (len s + b) drop' take' s
      | otherwise        = throwError HiErrorInvalidArgument

    split' :: Int -> Int -> (Int -> t -> t) -> (Int -> t -> t) -> t -> t
    split' a b drop' take' s = drop' a (take' b s)

    getList :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
    getList (HiValueList a) b = return (HiValueList (a |> b))
    getList _ _               = throwError HiErrorInvalidArgument

    getRange :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
    getRange (HiValueNumber a) (HiValueNumber b) =
      return $ HiValueList $ S.fromList $ map HiValueNumber [a .. b]
    getRange _ _                                 = throwError HiErrorInvalidArgument

    getFoldFun :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
    getFoldFun (HiValueFunction fun) (HiValueList s) =
      foldFun (HiValueFunction fun) (map HiExprValue (toList s))
    getFoldFun _ _                                   = throwError HiErrorInvalidArgument

    foldFun :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
    foldFun (HiValueFunction HiFunDiv) (a : (b : xs)) = applyFun (getFun "/") (eval' a) (b : xs)
    foldFun (HiValueFunction HiFunAdd) (a : (b : xs)) = applyFun (getFun "+") (eval' a) (b : xs)
    foldFun (HiValueFunction HiFunSub) (a : (b : xs)) = applyFun (getFun "-") (eval' a) (b : xs)
    foldFun (HiValueFunction HiFunMul) (a : (b : xs)) = applyFun (getFun "*") (eval' a) (b : xs)
    foldFun (HiValueFunction HiFunAnd) (a : (b : xs)) = applyLazyFun (eval' a) (b : xs) falseNull
    foldFun (HiValueFunction HiFunOr) (a : (b : xs))  = applyLazyFun (eval' a) (b : xs) (not . falseNull)
    foldFun _ _                                       = throwError HiErrorInvalidArgument

    getByteFun :: Monad m => [Char] -> (HiValue -> ExceptT HiError m HiValue)
    getByteFun "pack" (HiValueList s)     = seqToBytes [] (toList s)
    getByteFun "unpack" (HiValueBytes b)  =
      return $ HiValueList $ S.fromList $ map (HiValueNumber . toRational) $ B.unpack b
    getByteFun "encode" (HiValueString s) = return $ HiValueBytes $ encodeUtf8 s
    getByteFun "decode" (HiValueBytes b)  = return $ eitherToHiValue (decodeUtf8' b)

    getByteFun "zip" (HiValueBytes b)          = return
      $ HiValueBytes
      $ toStrict . compressWith defaultCompressParams { compressLevel = bestCompression }
      $ fromStrict b
    getByteFun "unzip" (HiValueBytes b)        = return
      $ HiValueBytes
      $ toStrict . decompressWith defaultDecompressParams
      $ fromStrict b
    getByteFun "serialise" a                  = return $ HiValueBytes $ toStrict $ serialise $ show a
    getByteFun "deserialise" (HiValueBytes b) = return $ read $ deserialise $ fromStrict b
    getByteFun _ _                            = throwError HiErrorInvalidArgument

    seqToBytes :: Monad m => [Word8] -> [HiValue] -> ExceptT HiError m HiValue
    seqToBytes res [] = return $ HiValueBytes (B.pack (reverse res))
    seqToBytes res (HiValueNumber (a :% 1) : xs) | 0 <= a && a < 256 =
      seqToBytes (fromIntegral a : res) xs
    seqToBytes _ _ = throwError HiErrorInvalidArgument

    eitherToHiValue :: Either UnicodeException T.Text -> HiValue
    eitherToHiValue (Right t) = HiValueString t
    eitherToHiValue (Left _)  = HiValueNull

    getAction :: Monad m => (T.Text -> HiAction) -> (HiValue -> ExceptT HiError m HiValue)
    getAction fun (HiValueString s) = return $ HiValueAction $ fun s
    getAction _ _                   = throwError HiErrorInvalidArgument

    getWrite :: Monad m => (HiValue -> HiValue -> ExceptT HiError m HiValue)
    getWrite (HiValueString a) (HiValueString b) =
      return $ HiValueAction $ HiActionWrite (T.unpack a) (encodeUtf8 b)
    getWrite _ _                                 = throwError HiErrorInvalidArgument

    readTime :: Monad m => (HiValue -> ExceptT HiError m HiValue)
    readTime (HiValueString t) = return $ maybeToHiValue (readMaybe (T.unpack t))
    readTime _                 = throwError HiErrorInvalidArgument

    maybeToHiValue :: Maybe UTCTime -> HiValue
    maybeToHiValue (Just t) = HiValueTime t
    maybeToHiValue Nothing  = HiValueNull

    getRand :: Monad m => (HiValue -> HiValue -> ExceptT HiError m HiValue)
    getRand (HiValueNumber (a :% 1)) (HiValueNumber (b :% 1)) =
      return $ HiValueAction $ HiActionRand (fromIntegral a) (fromIntegral b)
    getRand _ _                                               = throwError HiErrorInvalidArgument
    -- a = (fromRight (HiExprValue (HiValueNumber 0)) (eval (fromRight (HiExprValue (HiValueNumber 0)) (parse "div(10,3)"))))