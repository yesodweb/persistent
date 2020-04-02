{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.DeepSeq
import Control.DeepSeq.Generics
import Criterion.Main
import Data.Text                  (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Database.Persist.Quasi
import Database.Persist.TH
import Models

main :: IO ()
main = defaultMain
    [ bgroup "mkPersist"
        [ bench "From File" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/models-slowly")
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
    rnf = genericRnf

instance NFData AnnTarget where
    rnf = genericRnf
instance NFData RuleBndr where
    rnf = genericRnf

instance NFData Role where
    rnf = genericRnf

instance NFData Phases where
    rnf = genericRnf

instance NFData InjectivityAnn where
    rnf = genericRnf

instance NFData FamilyResultSig where
    rnf = genericRnf

instance NFData RuleMatch where
    rnf = genericRnf

instance NFData TypeFamilyHead where
    rnf = genericRnf

instance NFData TySynEqn where
    rnf = genericRnf

instance NFData Inline where
    rnf = genericRnf

instance NFData Pragma where
    rnf = genericRnf

instance NFData FixityDirection where
    rnf = genericRnf

instance NFData Safety where
    rnf = genericRnf

instance NFData Fixity where
    rnf = genericRnf

instance NFData Callconv where
    rnf = genericRnf

instance NFData Foreign where
    rnf = genericRnf

instance NFData SourceStrictness where
    rnf = genericRnf

instance NFData SourceUnpackedness where
    rnf = genericRnf

instance NFData FunDep where
    rnf = genericRnf

instance NFData Bang where
    rnf = genericRnf

#if MIN_VERSION_template_haskell(2,12,0)
instance NFData PatSynDir where
    rnf = genericRnf

instance NFData PatSynArgs where
    rnf = genericRnf

instance NFData DerivStrategy where
    rnf = genericRnf

instance NFData DerivClause where
    rnf = genericRnf
#endif

instance NFData Con where
    rnf = genericRnf

instance NFData Range where
    rnf = genericRnf

instance NFData Clause where
    rnf = genericRnf

instance NFData PkgName where
    rnf = genericRnf

instance NFData Dec where
    rnf = genericRnf

instance NFData Stmt where
    rnf = genericRnf

instance NFData TyLit where
    rnf = genericRnf

instance NFData NameSpace where
    rnf = genericRnf

instance NFData Body where
    rnf = genericRnf

instance NFData Guard where
    rnf = genericRnf

instance NFData Match where
    rnf = genericRnf

instance NFData ModName where
    rnf = genericRnf

instance NFData Pat where
    rnf = genericRnf

instance NFData TyVarBndr where
    rnf = genericRnf

instance NFData NameFlavour where
    rnf = genericRnf

instance NFData Type where
    rnf = genericRnf

instance NFData Exp where
    rnf = genericRnf

instance NFData Lit where
    rnf = genericRnf

instance NFData OccName where
    rnf = genericRnf

instance NFData Name where
    rnf = genericRnf
