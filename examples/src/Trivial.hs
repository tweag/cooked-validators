{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | This is a contract that validates iff there's an output at some specified address
-- It does nothing besides providing a simple enough test case for us to understand
-- how would we handle sets of validators.
module Trivial where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import Ledger.Contexts (ScriptContext (..))
import qualified Ledger.Credential as Ledger
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value (geq)
import qualified PlutusCore.Pretty as P
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

-- | Lets add some salt as a parameter to be able to make things a little
--  more difficult, since we will be finding this in practice a lot.
data TrivialParams = TrivialParams
  { salt :: Integer,
    next :: Ledger.ValidatorHash
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''TrivialParams

type TrivialDatum = ()

type TrivialRedeemer = ()

{-# INLINEABLE validateTrivial #-}
validateTrivial :: TrivialParams -> TrivialDatum -> TrivialRedeemer -> ScriptContext -> Bool
validateTrivial (TrivialParams _ next) _ _ ctx =
  (== 1) $ length $ filter (xxx . Ledger.addressCredential . Ledger.txOutAddress) $ Ledger.txInfoOutputs (scriptContextTxInfo ctx)
  where
    -- Here we make sure that that thre is an output in the transaction
    -- which pays to a given script; All scripts that are related to us have to
    -- be passed around as 'ValidatorHash'es one way or another, so the pir file
    -- never really contains any hash.
    xxx :: Ledger.Credential -> Bool
    xxx (Ledger.ScriptCredential vh) = vh == next
    xxx _ = False

-- Plutus boilerplate

data Trivial

instance Scripts.ValidatorTypes Trivial where
  type RedeemerType Trivial = TrivialRedeemer
  type DatumType Trivial = TrivialDatum

trivialValidator :: TrivialParams -> Scripts.TypedValidator Trivial
trivialValidator =
  Scripts.mkTypedValidatorParam @Trivial
    $$(PlutusTx.compile [||validateTrivial||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @TrivialDatum @TrivialRedeemer

trivialValidatorAddress :: TrivialParams -> Ledger.Address
trivialValidatorAddress = Ledger.scriptAddress . Scripts.validatorScript . trivialValidator

-- In order to inspect the pir file, you can generate it with the function below,
-- but we'll sumarize the important bits at the end of the file.
savePirFile :: Haskell.IO ()
savePirFile = case PlutusTx.getPir $$(PlutusTx.compile [||validateTrivial||]) of
  Just res -> Haskell.writeFile "trivial.pir" (Haskell.show $ P.prettyClassicDebug res)
  Nothing -> Haskell.undefined

-- |
--
-- * Symbolic Execution and Hashes
--
-- There is no need to care about it!!
--
-- If we generate the 'trivial.pir' file above, by runnung the 'savePirFile'
-- function, we will get something that looks like:
--
-- > 01| (program
-- > 02|   (let
-- > 03|     ...
-- > 04|     (lam ds_1725 TrivialParams_1385
-- > 05|       (lam ds_1726 Unit_1137
-- > 06|         (lam ds_1727 Unit_1137
-- > 07|           (lam ctx_1728 ScriptContext_1394
-- > 08|             [{ [ TrivialParams_match_1387 ds_1725 ] Bool_1289 }
-- > 09|               (lam ds_1729 (con integer)
-- > 10|                 (lam next_1730 (con bytestring)
-- > 11|                    ...
-- > 12|                        [(builtin equalsByteString) vh_1791 next_1730]
-- > 13|                    ...
-- > 14| ))]))))))
--
-- The file is a program which consits in a large (nested) let-statement and
-- returns something of type @TrivialParams -> Unit -> Unit -> ScriptContext -> Bool@,
-- and we can see the lambdas corresponding to this function type in lines 4-7 in
-- the snippet above.
--
-- Note however, that the first thing we do is pattern match on the params and
-- extract the @next@ field. When running this on our symbolic execution engine,
-- I'd expect something like the following constraints to come out:
--
--      ctx ~ ScriptContext { ... , txInfoOutputs = x:xs , ... }
--  &&
--      y ~ addressCredential (txOutAddress x)
--  &&
--      ScriptCredential vh ~ y
-- -------------------------------------------------------------
--    validateTrivial (TrivialParams _ vh) _ _ ctx
--
-- We'd know that by accumulating that as a path formula which the SMT is happy to infer
-- the result is true, then simplifying the path formula to the best of our habilities.
--
-- ** How Should we Connect More Validators?
--
-- We'd somehow add another constraint, perhaps a meta constraint, conjunct to our
-- path execution:
--
--   vh @~ otherValidator
--
-- Now, the tool would know that whenever vh probably appears in @txInfoInputs@, it
-- submits the same symbolic @ctx@ with its discovered constraints to @otherValidator@,
-- concatenating the path formulas and further refining the @ctx@
