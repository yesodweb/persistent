{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Database.Persist.Quasi
import Database.Persist.TH
import Database.Persist.TH.Internal
import Models

main :: IO ()
main =
    defaultMain
        [ bgroup
            "mkPersist"
            []
        ]

-- bench "From File" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/models-slowly")
-- , bgroup "Non-Null Fields"
--    [ bgroup "Increasing model count"
--        [ bench "1x10" $ nfIO $ mkPersist' $( parseReferencesQ (mkModels 10 10))
--        , bench "10x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 10))
--        , bench "100x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 100 10))
--        -- , bench "1000x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 1000 10))
--        ]
--    , bgroup "Increasing field count"
--        [ bench "10x1" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 1))
--        , bench "10x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 10))
--        , bench "10x100" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 100))
--        -- , bench "10x1000" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 1000))
--        ]
--    ]
-- , bgroup "Nullable"
--    [ bgroup "Increasing model count"
--        [ bench "20x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 20 10))
--        , bench "40x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 40 10))
--        , bench "60x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 60 10))
--        , bench "80x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 80 10))
--        , bench "100x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 100 10))
--        -- , bench "1000x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 1000 10))
--        ]
--    , bgroup "Increasing field count"
--        [ bench "10x20" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 20))
--        , bench "10x40" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 40))
--        , bench "10x60" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 60))
--        , bench "10x80" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 80))
--        , bench "10x100" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 100))
--        -- , bench "10x1000" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 1000))
--        ]
--    ]

-- Orphan instances for NFData Template Haskell types
instance NFData Overlap

instance NFData AnnTarget

instance NFData RuleBndr

instance NFData Role

instance NFData Phases

instance NFData InjectivityAnn

instance NFData FamilyResultSig

instance NFData RuleMatch

instance NFData TypeFamilyHead

instance NFData TySynEqn

instance NFData Inline

instance NFData Pragma

instance NFData FixityDirection

instance NFData Safety

instance NFData Fixity

instance NFData Callconv

instance NFData Foreign

instance NFData SourceStrictness

instance NFData SourceUnpackedness

instance NFData FunDep

instance NFData Bang

#if MIN_VERSION_template_haskell(2,12,0)
instance NFData PatSynDir where


instance NFData PatSynArgs where


instance NFData DerivStrategy where


instance NFData DerivClause where
#endif

#if MIN_VERSION_template_haskell(2,21,0)
instance NFData BndrVis where
#endif

#if MIN_VERSION_template_haskell(2,22,0)
instance NFData NamespaceSpecifier where
#endif

instance NFData Con

instance NFData Range

instance NFData Clause

instance NFData PkgName

instance NFData Dec

instance NFData Stmt

instance NFData TyLit

instance NFData NameSpace

instance NFData Body

instance NFData Guard

instance NFData Match

instance NFData ModName

instance NFData Pat

#if MIN_VERSION_template_haskell(2,16,0)
instance NFData Bytes where
    rnf !_ = ()
#endif

#if MIN_VERSION_template_haskell(2,17,0)
instance NFData a => NFData (TyVarBndr a) where

instance NFData Specificity
#else
instance NFData TyVarBndr where

#endif

instance NFData NameFlavour

instance NFData Type

instance NFData Exp

instance NFData Lit

instance NFData OccName

instance NFData Name
