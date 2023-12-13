{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TypeApplications           #-}
module Main (main) where

import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Generics             (gshow)
import Language.Haskell.TH

import Data.String                (fromString)
import Language.Haskell.TH.Syntax
import System.Directory           (doesFileExist, setCurrentDirectory)
import System.FilePath            ((</>))
import Test.Tasty                 (defaultMain, testGroup)
import Test.Tasty.Golden          (goldenVsStringDiff)

import Language.Haskell.TH.CodeT

type MyInt = Int

findPackageDir :: IO ()
findPackageDir = do
    try1 <- doesFileExist cabalFile
    if try1 then return () else do
        try2 <- doesFileExist (directory </> cabalFile)
        if try2 then setCurrentDirectory directory else fail $ "cannot find directory with " ++ cabalFile
  where
    directory = "codet"
    cabalFile = "codet.cabal"

main :: IO ()
main = do
    findPackageDir
    defaultMain $ testGroup "codet"
        [ goldenVsStringDiff "basic" diff "tests/codet-tests.txt" $ do
            return $ fromString $ unlines $ concat
                [ dispType (codeT @Int)
                , dispType (codeT @[Int])
                , dispType (codeT @1)
                , dispType (codeT @'c')
                , dispType (codeT @"string")
                , dispType (codeT @MyInt) -- Int
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
