{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Language.Haskell.TH.CodeT.Unsafe where

import Data.Int  (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

import Language.Haskell.TH (Name, Q, Quote, Type, appT, varT)

newtype CodeT m a = UnsafeCodeT { unTypeCodeT :: m Type }

appCodeT :: Quote m => CodeT m f -> CodeT m x -> CodeT m (f x)
appCodeT (UnsafeCodeT f) (UnsafeCodeT x) = UnsafeCodeT (appT f x)

type CodeTQ = CodeT Q

unsafeCodeTName :: Quote m => Name -> CodeT m a
unsafeCodeTName n = UnsafeCodeT (varT n)

-------------------------------------------------------------------------------
-- LiftT
-------------------------------------------------------------------------------

class LiftT a where
    codeT :: Quote m => CodeT m a

instance (LiftT f, LiftT x) => LiftT (f x) where
    codeT = appCodeT codeT codeT

-------------------------------------------------------------------------------
-- instances: Type
-------------------------------------------------------------------------------

instance LiftT Int where codeT = unsafeCodeTName ''Int
instance LiftT Word where codeT = unsafeCodeTName ''Word
instance LiftT Float where codeT = unsafeCodeTName ''Float
instance LiftT Double where codeT = unsafeCodeTName ''Double

instance LiftT Int8  where codeT = unsafeCodeTName ''Int8
instance LiftT Int16 where codeT = unsafeCodeTName ''Int16
instance LiftT Int32 where codeT = unsafeCodeTName ''Int32
instance LiftT Int64 where codeT = unsafeCodeTName ''Int64

instance LiftT Word8  where codeT = unsafeCodeTName ''Word8
instance LiftT Word16 where codeT = unsafeCodeTName ''Word16
instance LiftT Word32 where codeT = unsafeCodeTName ''Word32
instance LiftT Word64 where codeT = unsafeCodeTName ''Word64

instance LiftT Bool where codeT = unsafeCodeTName ''Bool
instance LiftT Ordering where codeT = unsafeCodeTName ''Ordering
instance LiftT Char where codeT = unsafeCodeTName ''Char

-------------------------------------------------------------------------------
-- instances: Tuples
-------------------------------------------------------------------------------

instance LiftT () where codeT = unsafeCodeTName ''()
instance LiftT (,) where codeT = unsafeCodeTName ''(,)
instance LiftT (,,) where codeT = unsafeCodeTName ''(,,)
instance LiftT (,,,) where codeT = unsafeCodeTName ''(,,,)
instance LiftT (,,,,) where codeT = unsafeCodeTName ''(,,,,)
instance LiftT (,,,,,) where codeT = unsafeCodeTName ''(,,,,,)
instance LiftT (,,,,,,) where codeT = unsafeCodeTName ''(,,,,,,)

-------------------------------------------------------------------------------
-- instances: Type -> Type
-------------------------------------------------------------------------------

instance LiftT [] where codeT = unsafeCodeTName ''[]
instance LiftT Maybe where codeT = unsafeCodeTName ''Maybe
instance LiftT IO where codeT = unsafeCodeTName ''IO

-------------------------------------------------------------------------------
-- instances: Type -> Type -> Type
-------------------------------------------------------------------------------

instance LiftT Either where codeT = unsafeCodeTName ''Either
