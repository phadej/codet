{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.TH.CodeT.Plugin (
    plugin,
) where

import Control.Monad (forM)
import Data.Maybe    (catMaybes)
import Data.String   (fromString)

import qualified GHC.Plugins as Plugins

import Plugin.GHC

-- | A GHC type-checker plugin which solves 'Language.Haskell.TH.CodeT.LiftT' instances.
--
-- At the moment plugin solves only type constructor instances,
-- e.g. for a data type
--
-- @
-- data Foo a = MkFoo a
-- @
--
-- the plugin will solve needed instances in @'Language.Hsakell.TH.CodeT.codeT' \@Foo@ and @'Language.Hsakell.TH.CodeT.codeT' \@('MkFoo)@.
-- (There is @(LiftT f, LiftT x) => LiftT (f x)@ existing instance, so plugin doesn't need to).
--
-- Noteably, the plugin solves only for algebraic type constructors (@data@, @newtype@, @class@) and
-- promoted data constructors. Specifically it doesn't solve for type-family type constructors.
--
-- Enable plugin with:
--
-- @
-- \{-# OPTIONS_GHC -fplugin=Language.Haskell.TH.CodeT.Plugin #-\}
-- @
--
plugin :: Plugins.Plugin
plugin = Plugins.defaultPlugin
    { Plugins.tcPlugin = Just . tcPlugin
    }

tcPlugin :: a -> TcPlugin
tcPlugin _ = TcPlugin
    { tcPluginInit    = tcPluginInit_
    , tcPluginSolve   = tcPluginSolve_
    , tcPluginStop    = const (return ())
#if __GLASGOW_HASKELL__ >=904
    , tcPluginRewrite = \_ -> Plugins.emptyUFM
#endif
    }

data PluginCtx = PluginCtx
    { liftTClass        :: Class
    , unsafeCodeTNameD  :: Id
    , unsafeCodeTNameTC :: Id
    }

tcPluginInit_ :: TcPluginM PluginCtx
tcPluginInit_ = do
    let codet :: FastString
        codet = fromString "codet"

    let codeTModuleName :: ModuleName
        codeTModuleName =  mkModuleName "Language.Haskell.TH.CodeT.Unsafe"

    liftTClass <- do
        md <- findModulePluginM codeTModuleName codet
        tcLookupClass =<< lookupOrig md (mkTcOcc "LiftT")

    unsafeCodeTNameD <- do
        md <- findModulePluginM codeTModuleName codet
        lookupOrig md (mkVarOcc "unsafeCodeTNameD")  >>= tcLookupId

    unsafeCodeTNameTC <- do
        md <- findModulePluginM codeTModuleName codet
        lookupOrig md (mkVarOcc "unsafeCodeTNameTC")  >>= tcLookupId

    return PluginCtx {..}

tcPluginSolve_ :: PluginCtx -> TcPluginSolver
tcPluginSolve_ ctx _evBindsVar _given wanteds = do
    solved' <- unsafeTcPluginTcM $ forM wanteds $ \wanted -> solveLiftT ctx wanted

    let solved :: [(EvTerm, Ct)]
        solved = catMaybes solved'

    let new :: [Ct]
        new = []
    return $ TcPluginOk solved new

solveLiftT :: PluginCtx -> Ct -> TcM (Maybe (EvTerm, Ct))
solveLiftT ctx wanted
    | Just (ct, [k, x]) <- findClassConstraint (liftTClass ctx) wanted
    , Just (xTyCon, _args) <- splitTyConApp_maybe x
    , isAlgTyCon xTyCon || isPromotedDataCon xTyCon
    -- , let ki = tyConKind xTyCon
    -- in 9.0 splitPiTysInvisible
    -- , (_invPis, _) <- splitInvisPiTys ki
    , let xTyConName = getName xTyCon
    , Just tcMod <- nameModule_maybe xTyConName
    -- TODO: check that 'args' count matches 'invPis'?

    = do
        let occ = nameOccName xTyConName

        -- tcPluginIO $ logOutput logger $ text "wanted:" <+> ppr x
        let pkg_str    = unitString (moduleUnit tcMod)
            mod_str    = moduleNameString (moduleName tcMod)
            occ_str    = occNameString occ

        pkg_str' <- mkStringExpr pkg_str
        mod_str' <- mkStringExpr mod_str
        occ_str' <- mkStringExpr occ_str

        let fun | isDataOcc occ = unsafeCodeTNameD ctx
                | isTcOcc occ   = unsafeCodeTNameTC ctx
                | otherwise     = Plugins.pprPanic "solveLiftT" (ppr xTyConName)

        let ev = mkCoreApps (Var fun) [Type k, Type x, pkg_str', mod_str', occ_str']
        let evidence = makeClassEvidence (liftTClass ctx) [k, x] ev

        return (Just (evidence, ct))

    | otherwise
    = return Nothing
