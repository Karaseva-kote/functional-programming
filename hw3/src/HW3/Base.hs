module HW3.Base 
  ( HiFun (..)
  , HiValue (..)
  , HiExpr (..)
  , HiError (..)
  , HiAction (..)
  , HiMonad (..)
  ) where

    import Data.Text ( Text )
    import Data.Sequence.Internal (Seq)
    import Data.ByteString (ByteString)
    import Data.Time (UTCTime)

    data HiFun 
      = HiFunDiv
      | HiFunMul
      | HiFunAdd
      | HiFunSub
      -- 
      | HiFunNot
      | HiFunAnd
      | HiFunOr
      | HiFunLessThan
      | HiFunGreaterThan
      | HiFunEquals
      | HiFunNotLessThan
      | HiFunNotGreaterThan
      | HiFunNotEquals
      | HiFunIf
      -- 
      | HiFunLength
      | HiFunToUpper
      | HiFunToLower
      | HiFunReverse
      | HiFunTrim
      -- 
      | HiFunList
      | HiFunRange
      | HiFunFold
      -- 
      | HiFunPackBytes
      | HiFunUnpackBytes
      | HiFunEncodeUtf8
      | HiFunDecodeUtf8
      | HiFunZip
      | HiFunUnzip
      | HiFunSerialise
      | HiFunDeserialise
      -- 
      | HiFunRead
      | HiFunWrite
      | HiFunMkDir
      | HiFunChDir
      -- 
      | HiFunParseTime
      -- 
      | HiFunRand
      -- 
      | HiFunEcho
      deriving (Show, Eq, Ord, Read) 

    data HiValue
      = HiValueNumber Rational
      | HiValueFunction HiFun
      | HiValueBool Bool
      -- 
      | HiValueNull
      | HiValueString Text 
      -- 
      | HiValueList (Seq HiValue)
      -- 
      | HiValueBytes ByteString
      -- 
      | HiValueAction HiAction
      -- 
      | HiValueTime UTCTime
      deriving (Show, Eq, Ord, Read)

    data HiExpr
      = HiExprValue HiValue
      | HiExprApply HiExpr [HiExpr]
      -- 
      | HiExprRun HiExpr
      deriving (Show, Eq, Ord, Read)

    data HiError
      = HiErrorInvalidArgument
      | HiErrorInvalidFunction
      | HiErrorArityMismatch
      | HiErrorDivideByZero
      deriving (Show, Eq, Ord, Read)

    data HiAction 
      = HiActionRead  FilePath
      | HiActionWrite FilePath ByteString
      | HiActionMkDir FilePath
      | HiActionChDir FilePath
      | HiActionCwd
      -- 
      | HiActionNow
      -- 
      | HiActionRand Int Int
      -- 
      | HiActionEcho Text
      deriving (Show, Eq, Ord, Read)

    class Monad m => HiMonad m where
      runAction :: HiAction -> m HiValue
    