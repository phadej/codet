{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.CodeT.Plugin #-}
{-# OPTIONS_GHC -dcore-lint #-}
module Main (main, Stuck) where

import Control.Monad.Trans.State  (State, evalState, get, put)
import Data.Generics              (gshow)
import Data.Proxy                 (Proxy)
import Data.String                (fromString)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory           (doesFileExist, setCurrentDirectory)
import System.FilePath            ((</>))
import Test.Tasty                 (defaultMain, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff)

import Language.Haskell.TH.CodeT

type family Stuck where

data Foo a = Foo a
type MyInt = Int

findPackageDir :: IO ()
findPackageDir = do
    try1 <- doesFileExist cabalFile
    if try1 then return () else do
        try2 <- doesFileExist (directory </> cabalFile)
        if try2 then setCurrentDirectory directory else fail $ "cannot find directory with " ++ cabalFile
  where
    directory = "codet-plugin"
    cabalFile = "codet-plugin.cabal"

main :: IO ()
main = do
    findPackageDir
    defaultMain $ testGroup "codet"
        [ goldenVsStringDiff "basic" diff "tests/codet-plugin-tests.txt" $ do
            return $ fromString $ unlines $ concat
                [ dispType (codeT @Int)
                , dispType (codeT @Proxy)
                , dispType (codeT @Just)
                , dispType (codeT @Foo)
                , dispType (codeT @('Foo))
                , dispType (codeT @[Int])
                , dispType (codeT @Show)
                , dispType (codeT @1)
                , dispType (codeT @'c')
                , dispType (codeT @"string")
                , dispType (codeT @MyInt) -- Int
                -- , dispType (codeT @Stuck) -- fails
                ]
        ]

diff :: FilePath -> FilePath -> [String]
diff ref new = ["diff", "-u", ref, new]

dispType :: CodeT P a -> [String]
dispType c =
    [ show (ppr ty)
    , gshow ty
    ]
  where
    ty = runP (unTypeCodeT c)

newtype P a = P (State Uniq a)
  deriving stock Functor
  deriving newtype (Applicative, Monad)

runP :: P a -> a
runP (P s) = evalState s 0

instance Quote P where
    newName s = do
        u <- P get
        P (put (u + 1))
        return (mkNameU s u)
