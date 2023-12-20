{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE Unsafe                #-}
#if __GLASGOW_HASKELL__ <908
{-# LANGUAGE UndecidableInstances  #-}
#endif
{-# OPTIONS_HADDOCK not-home #-}
module Language.Haskell.TH.CodeT.Unsafe where

import Data.Int     (Int16, Int32, Int64, Int8)
import Data.Proxy   (Proxy (..))
import Data.Word    (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, KnownSymbol, natVal, symbolVal)

#if MIN_VERSION_base(4,16,0)
import GHC.TypeLits (KnownChar, charVal)
#endif

import qualified GHC.Exts as Ki

import Language.Haskell.TH        (Name, Q, Quote, TyLit (..), Type, appT, conT, litT)
import Language.Haskell.TH.Syntax (mkNameG_d, mkNameG_tc)

-- instances
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Functor.Compose
import qualified Data.Functor.Const
import qualified Data.Functor.Identity
import qualified Data.IntMap
import qualified Data.IntSet
import qualified Data.Map
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Time
import qualified Data.Tree

-------------------------------------------------------------------------------
-- CodeT
-------------------------------------------------------------------------------

newtype CodeT m a = UnsafeCodeT (m Type)

unTypeCodeT :: CodeT m a -> m Type
unTypeCodeT (UnsafeCodeT ty) = ty

appCodeT :: Quote m => CodeT m f -> CodeT m x -> CodeT m (f x)
appCodeT (UnsafeCodeT f) (UnsafeCodeT x) = UnsafeCodeT (appT f x)

type CodeTQ = CodeT Q

unsafeCodeTName :: Quote m => Name -> CodeT m a
unsafeCodeTName n = UnsafeCodeT (conT n)

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
instance KnownNat n    => LiftT n where codeT = UnsafeCodeT $ litT $ return $ NumTyLit  $ natVal (Proxy @n)

#if MIN_VERSION_base(4,16,0)
instance KnownChar c   => LiftT c where codeT = UnsafeCodeT $ litT $ return $ CharTyLit $ charVal (Proxy @c)
#endif

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
-- instances: kinds
-------------------------------------------------------------------------------

instance LiftT (->) where codeT = unsafeCodeTName ''(->) -- perfectly, we want LiftT.FUN
instance LiftT Ki.TYPE where codeT = unsafeCodeTName ''Ki.TYPE

{-

We don't have an instance for Constraint. GHC is tricky.
https://gitlab.haskell.org/ghc/ghc/-/issues/24279

#if MIN_VERSION_base(4,18,0)
instance LiftT Ki.CONSTRAINT where codeT = unsafeCodeTName ''Ki.CONSTRAINT
#else
instance LiftT Ki.Constraint where codeT = unsafeCodeTName ''Ki.Constraint
#endif
-}

#if MIN_VERSION_base(4,16,0)
instance LiftT Ki.RuntimeRep where codeT = unsafeCodeTName ''Ki.RuntimeRep
instance LiftT Ki.BoxedRep where codeT = unsafeCodeTName 'Ki.BoxedRep

instance LiftT Ki.Levity where codeT = unsafeCodeTName ''Ki.Levity
instance LiftT Ki.Lifted where codeT = unsafeCodeTName 'Ki.Lifted
instance LiftT Ki.Unlifted where codeT = unsafeCodeTName 'Ki.Unlifted
#else
instance LiftT Ki.LiftedRep where codeT = unsafeCodeTName 'Ki.LiftedRep
instance LiftT Ki.UnliftedRep where codeT = unsafeCodeTName 'Ki.UnliftedRep
#endif

-------------------------------------------------------------------------------
-- instances: bytestring
-------------------------------------------------------------------------------

instance LiftT Data.ByteString.ByteString where codeT = unsafeCodeTName ''Data.ByteString.ByteString
instance LiftT Data.ByteString.Lazy.ByteString where codeT = unsafeCodeTName ''Data.ByteString.Lazy.ByteString
instance LiftT Data.ByteString.Builder.Builder where codeT = unsafeCodeTName ''Data.ByteString.Builder.Builder

-------------------------------------------------------------------------------
-- instances: containers
-------------------------------------------------------------------------------

instance LiftT Data.IntMap.IntMap where codeT = unsafeCodeTName ''Data.IntMap.IntMap
instance LiftT Data.IntSet.IntSet where codeT = unsafeCodeTName ''Data.IntSet.IntSet
instance LiftT Data.Map.Map where codeT = unsafeCodeTName ''Data.Map.Map
instance LiftT Data.Sequence.Seq where codeT = unsafeCodeTName ''Data.Sequence.Seq
instance LiftT Data.Set.Set where codeT = unsafeCodeTName ''Data.IntSet.IntSet
instance LiftT Data.Tree.Tree where codeT = unsafeCodeTName ''Data.Tree.Tree

-------------------------------------------------------------------------------
-- instances: text
-------------------------------------------------------------------------------

instance LiftT Data.Text.Text where codeT = unsafeCodeTName ''Data.Text.Text
instance LiftT Data.Text.Lazy.Text where codeT = unsafeCodeTName ''Data.Text.Lazy.Text
instance LiftT Data.Text.Lazy.Builder.Builder where codeT = unsafeCodeTName ''Data.Text.Lazy.Builder.Builder

-------------------------------------------------------------------------------
-- instances: transformers
-------------------------------------------------------------------------------

instance LiftT Data.Functor.Identity.Identity where codeT = unsafeCodeTName ''Data.Functor.Identity.Identity
instance LiftT Data.Functor.Compose.Compose where codeT = unsafeCodeTName ''Data.Functor.Compose.Compose
instance LiftT Data.Functor.Const.Const where codeT = unsafeCodeTName ''Data.Functor.Const.Const

-------------------------------------------------------------------------------
-- instances: time
-------------------------------------------------------------------------------

instance LiftT Data.Time.UTCTime where codeT = unsafeCodeTName ''Data.Time.UTCTime
instance LiftT Data.Time.Day where codeT = unsafeCodeTName ''Data.Time.Day
