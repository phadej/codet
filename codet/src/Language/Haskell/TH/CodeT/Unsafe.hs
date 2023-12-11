{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE Unsafe                #-}
{-# HADDOCK not-home #-}
module Language.Haskell.TH.CodeT.Unsafe where

import Data.Int     (Int16, Int32, Int64, Int8)
import Data.Proxy   (Proxy (..))
import Data.Word    (Word16, Word32, Word64, Word8)
import GHC.TypeLits

import Language.Haskell.TH        (Name, Q, Quote, TyLit (..), Type, appT, litT, varT)
import Language.Haskell.TH.Syntax (mkNameG_d, mkNameG_tc)

newtype CodeT m a = UnsafeCodeT (m Type)

unTypeCodeT :: CodeT m a -> m Type
unTypeCodeT (UnsafeCodeT ty) = ty

appCodeT :: Quote m => CodeT m f -> CodeT m x -> CodeT m (f x)
appCodeT (UnsafeCodeT f) (UnsafeCodeT x) = UnsafeCodeT (appT f x)

type CodeTQ = CodeT Q

unsafeCodeTName :: Quote m => Name -> CodeT m a
unsafeCodeTName n = UnsafeCodeT (varT n)

unsafeCodeTNameD :: String -> String -> String -> forall m. Quote m => CodeT m a
unsafeCodeTNameD x y z = unsafeCodeTName (mkNameG_d x y z)

unsafeCodeTNameTC :: String -> String -> String -> forall m. Quote m => CodeT m a
unsafeCodeTNameTC x y z = unsafeCodeTName (mkNameG_tc x y z)

-------------------------------------------------------------------------------
-- LiftT
-------------------------------------------------------------------------------

class LiftT a where
    codeT :: Quote m => CodeT m a

instance (LiftT f, LiftT x) => LiftT (f x) where
    codeT = appCodeT codeT codeT

-------------------------------------------------------------------------------
-- instances: TypeLits
-------------------------------------------------------------------------------

instance KnownSymbol s => LiftT s where codeT = UnsafeCodeT $ litT $ return $ StrTyLit  $ symbolVal (Proxy @s)
instance KnownChar c   => LiftT c where codeT = UnsafeCodeT $ litT $ return $ CharTyLit $ charVal (Proxy @c)
instance KnownNat n    => LiftT n where codeT = UnsafeCodeT $ litT $ return $ NumTyLit  $ natVal (Proxy @n)

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

-------------------------------------------------------------------------------
-- instances: bytestring
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- instances: containers
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- instances: text
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- instances: transformers
-------------------------------------------------------------------------------

-- TODO: Proxy
-- TODO: Identity Compose Const

-------------------------------------------------------------------------------
-- instances: time
-------------------------------------------------------------------------------
