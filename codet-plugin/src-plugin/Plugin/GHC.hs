{-# LANGUAGE CPP #-}
module Plugin.GHC (
    module X,
    findClassConstraint,
    makeClassEvidence,
    findModulePluginM,
) where

import GHC.Core                as X
import GHC.Core.Class          as X
import GHC.Core.DataCon        as X
import GHC.Core.Make           as X
import GHC.Core.Predicate      as X
import GHC.Core.TyCon          as X
import GHC.Core.Type           as X
import GHC.Core.Utils          as X
import GHC.Data.FastString     as X (FastString)
import GHC.Driver.Session      as X
import GHC.Tc.Plugin           as X
import GHC.Tc.Types            as X
import GHC.Tc.Types.Constraint as X
import GHC.Tc.Types.Evidence   as X
import GHC.Types.Id            as X
import GHC.Types.Name          as X
import GHC.Unit.Types          as X
import GHC.Utils.Error         as X
import GHC.Utils.Outputable    as X



#if __GLASGOW_HASKELL__ >= 906
import Language.Haskell.Syntax.Module.Name as X (ModuleName, mkModuleName, moduleNameString)
#else
import GHC.Unit.Module.Name as X (ModuleName, mkModuleName, moduleNameString)
#endif

#if __GLASGOW_HASKELL__ >=904
import GHC.Driver.Env   (hsc_unit_env)
import GHC.Rename.Names (renamePkgQual)
#endif

#if __GLASGOW_HASKELL__ >=902
import GHC.Utils.Logger
#endif

import Control.Monad (guard)

findClassConstraint :: Class -> Ct -> Maybe (Ct, [Type])
findClassConstraint cls ct = do
   (cls', args) <- getClassPredTys_maybe (ctPred ct)
   guard (cls' == cls)
   return (ct, args)

makeClassEvidence :: Class -> [Type] -> CoreExpr -> EvTerm
makeClassEvidence cls args e = EvExpr appDc where
    tyCon = classTyCon cls
    dc    = tyConSingleDataCon tyCon
    appDc = mkCoreConApps dc $ map Type args ++ [e]

fatal :: SDoc -> TcPluginM ()
fatal doc = do
#if __GLASGOW_HASKELL__ >=902
    logger <- unsafeTcPluginTcM getLogger
#if __GLASGOW_HASKELL__ >= 904
    tcPluginIO $ fatalErrorMsg logger doc
#else
    dflags <- unsafeTcPluginTcM getDynFlags
    tcPluginIO $ fatalErrorMsg logger dflags doc
#endif
#else
    dflags <- unsafeTcPluginTcM getDynFlags
    tcPluginIO $ fatalErrorMsg dflags doc
#endif

findModulePluginM :: ModuleName -> FastString -> TcPluginM Module
findModulePluginM m pkg = do
#if __GLASGOW_HASKELL__ >=904
    hscEnv <- getTopEnv
    let pkgQual = renamePkgQual (hsc_unit_env hscEnv) m (Just pkg)
#else
    let pkgQual = Just pkg
#endif
    im <- findImportedModule m pkgQual
    case im of
        Found _ md -> return md
        _          -> do
            fatal $ text "Cannot find module" <+> ppr m
            fail "panic!"
