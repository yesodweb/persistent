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
import Models

main :: IO ()
main = defaultMain
    [ bgroup "mkPersist"
        [ -- bench "From File" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/models-slowly")
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
        ]
    ]

-- Orphan instances for NFData Template Haskell types
instance NFData Overlap where


instance NFData AnnTarget where

instance NFData RuleBndr where


instance NFData Role where


instance NFData Phases where


instance NFData InjectivityAnn where


instance NFData FamilyResultSig where


instance NFData RuleMatch where


instance NFData TypeFamilyHead where


instance NFData TySynEqn where


instance NFData Inline where


instance NFData Pragma where


instance NFData FixityDirection where


instance NFData Safety where


instance NFData Fixity where


instance NFData Callconv where


instance NFData Foreign where


instance NFData SourceStrictness where


instance NFData SourceUnpackedness where


instance NFData FunDep where


instance NFData Bang where


#if MIN_VERSION_template_haskell(2,12,0)
instance NFData PatSynDir where


instance NFData PatSynArgs where


instance NFData DerivStrategy where


instance NFData DerivClause where

#endif

instance NFData Con where


instance NFData Range where


instance NFData Clause where


instance NFData PkgName where


instance NFData Dec where


instance NFData Stmt where


instance NFData TyLit where


instance NFData NameSpace where


instance NFData Body where


instance NFData Guard where


instance NFData Match where


instance NFData ModName where


instance NFData Pat where

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

instance NFData NameFlavour where


instance NFData Type where


instance NFData Exp where


instance NFData Lit where

instance NFData OccName where


instance NFData Name where

