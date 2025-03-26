{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The set of parameters, like protocol parameters and slot configuration.
module Cardano.Node.Emulator.Internal.Node.Params
  ( Params (..),
    paramsFromConfig,
    C.mkLatestTransitionConfig,
    slotConfigL,
    networkIdL,
    emulatorPParamsL,
    emulatorPParams,
    ledgerProtocolParameters,
    increaseTransactionLimits,
    increaseTransactionLimits',
    emulatorEpochSize,
    emulatorShelleyGenesisDefaults,
    emulatorAlonzoGenesisDefaults,
    emulatorConwayGenesisDefaults,
    keptBlocks,

    -- * cardano-ledger specific types and conversion functions
    EmulatorEra,
    PParams,
    TransitionConfig,
    slotLength,
    testnet,
    emulatorGlobals,
    emulatorEraHistory,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Genesis qualified as C
import Cardano.Ledger.Alonzo.PParams qualified as C
import Cardano.Ledger.Api.PParams qualified as C
import Cardano.Ledger.Api.Transition qualified as C
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer), boundRational)
import Cardano.Ledger.Binary.Version (Version, natVersion)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (ExUnits), Prices (Prices))
import Cardano.Ledger.Shelley.API (Coin (Coin), Globals, mkShelleyGlobals)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Node.Emulator.Internal.Node.TimeSlot
  ( SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime),
    beginningOfTime,
    nominalDiffTimeToPOSIXTime,
    posixTimeToNominalDiffTime,
    posixTimeToUTCTime,
    utcTimeToPOSIXTime,
  )
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Control.Lens (makeLensesFor, over, (%~), (&), (.~), (^.))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Default (Default (def))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.SOP (K (K))
import Data.SOP.Counting qualified as Ouroboros
import Data.SOP.NonEmpty qualified as Ouroboros
import Data.SOP.Strict (NP (Nil, (:*)))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Word (Word32)
import Ledger.Test (testNetworkMagic, testnet)
import Ouroboros.Consensus.Block (GenesisWindow (GenesisWindow))
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import PlutusLedgerApi.V1 (POSIXTime (POSIXTime, getPOSIXTime))
import Prettyprinter (Pretty (pretty), viaShow, vsep, (<+>))

-- | The default era for the emulator
type EmulatorEra = ConwayEra StandardCrypto

type PParams = C.PParams EmulatorEra

type TransitionConfig = C.TransitionConfig EmulatorEra

data Params = Params
  { pSlotConfig :: !SlotConfig,
    pEmulatorPParams :: !PParams,
    pNetworkId :: !C.NetworkId,
    pEpochSize :: !EpochSize,
    pConfig :: !TransitionConfig
  }
  deriving (Eq, Show, Generic)

instance ToJSON C.NetworkId where
  toJSON C.Mainnet = JSON.String "Mainnet"
  toJSON (C.Testnet (C.NetworkMagic n)) = JSON.Number $ fromIntegral n

instance FromJSON C.NetworkId where
  parseJSON (JSON.String "Mainnet") = pure C.Mainnet
  parseJSON (JSON.Number n) = pure $ C.Testnet $ C.NetworkMagic $ truncate n
  parseJSON v =
    prependFailure "parsing NetworkId failed, " (typeMismatch "'Mainnet' or Number" v)

deriving newtype instance ToJSON C.NetworkMagic

deriving newtype instance FromJSON C.NetworkMagic

makeLensesFor
  [ ("pSlotConfig", "slotConfigL"),
    ("pEmulatorPParams", "emulatorPParamsL"),
    ("pNetworkId", "networkIdL")
  ]
  ''Params

instance Default Params where
  def = paramsFromConfig defaultConfig

instance Pretty Params where
  pretty Params {..} =
    vsep
      [ "Slot config:" <+> pretty pSlotConfig,
        "Network ID:" <+> viaShow pNetworkId,
        "Protocol Parameters:" <+> viaShow pEmulatorPParams
      ]

-- | Convert `Params` to cardano-ledger `PParams`
emulatorPParams :: Params -> PParams
emulatorPParams = pEmulatorPParams

ledgerProtocolParameters :: Params -> C.LedgerProtocolParameters C.ConwayEra
ledgerProtocolParameters = C.LedgerProtocolParameters . emulatorPParams

-- | Set higher limits on transaction size and execution units.
-- This can be used to work around @MaxTxSizeUTxO@ and @ExUnitsTooBigUTxO@ errors.
-- Note that if you need this your Plutus script will probably not validate on Mainnet.
increaseTransactionLimits :: Params -> Params
increaseTransactionLimits = increaseTransactionLimits' 2 10 10

increaseTransactionLimits' :: Word32 -> Natural -> Natural -> Params -> Params
increaseTransactionLimits' size steps mem =
  over emulatorPParamsL $
    (C.ppMaxTxSizeL %~ (size *)) . (C.ppMaxTxExUnitsL %~ f)
  where
    f :: ExUnits -> ExUnits
    f (ExUnits executionSteps executionMemory) =
      ExUnits (steps * executionSteps) (mem * executionMemory)

emulatorProtocolMajorVersion :: Version
emulatorProtocolMajorVersion = natVersion @9

defaultConfig :: TransitionConfig
defaultConfig =
  C.mkLatestTransitionConfig
    emulatorShelleyGenesisDefaults
    emulatorAlonzoGenesisDefaults
    emulatorConwayGenesisDefaults

emulatorShelleyGenesisDefaults :: C.ShelleyGenesis StandardCrypto
emulatorShelleyGenesisDefaults =
  C.shelleyGenesisDefaults
    { C.sgNetworkMagic = case testNetworkMagic of C.NetworkMagic nm -> nm,
      C.sgSystemStart = posixTimeToUTCTime $ POSIXTime beginningOfTime,
      C.sgProtocolParams =
        C.sgProtocolParams C.shelleyGenesisDefaults
          & C.ppProtocolVersionL .~ ProtVer emulatorProtocolMajorVersion 0
          & C.ppMinFeeBL .~ Coin 155_381
          & C.ppMinFeeAL .~ Coin 44
          & C.ppKeyDepositL .~ Coin 2_000_000
    }

instance MonadFail (Either String) where
  fail = Left

emulatorAlonzoGenesisDefaults :: C.AlonzoGenesis
emulatorAlonzoGenesisDefaults =
  (C.alonzoGenesisDefaults C.ConwayEra)
    { C.agPrices =
        Prices (fromJust $ boundRational (577 % 10_000)) (fromJust $ boundRational (721 % 10_000_000)),
      C.agMaxTxExUnits = ExUnits 14_000_000 10_000_000_000
    }

emulatorConwayGenesisDefaults :: C.ConwayGenesis StandardCrypto
emulatorConwayGenesisDefaults = C.conwayGenesisDefaults

paramsFromConfig :: TransitionConfig -> Params
paramsFromConfig tc =
  Params
    { pSlotConfig =
        SlotConfig
          { scSlotZeroTime = utcTimeToPOSIXTime $ C.sgSystemStart sg,
            scSlotLength =
              getPOSIXTime $ nominalDiffTimeToPOSIXTime $ C.Ledger.fromNominalDiffTimeMicro $ C.sgSlotLength sg
          },
      pEmulatorPParams = tc ^. C.tcInitialPParamsG,
      pNetworkId = C.Testnet (C.NetworkMagic $ C.sgNetworkMagic sg),
      pEpochSize = C.sgEpochLength sg,
      pConfig = tc
    }
  where
    sg = tc ^. C.tcShelleyGenesisL

-- | Calculate the cardano-ledger `SlotLength`
slotLength :: Params -> SlotLength
slotLength Params {pSlotConfig} = mkSlotLength $ posixTimeToNominalDiffTime $ POSIXTime $ scSlotLength pSlotConfig

keptBlocks :: Params -> Integer
keptBlocks Params {pConfig} = fromIntegral $ C.sgSecurityParam (pConfig ^. C.tcShelleyGenesisL)

-- | A sensible default 'EpochSize' value for the emulator
emulatorEpochSize :: EpochSize
emulatorEpochSize = EpochSize 432_000

-- | A sensible default 'Globals' value for the emulator
emulatorGlobals :: Params -> Globals
emulatorGlobals params@Params {pEpochSize, pConfig} =
  mkShelleyGlobals
    (pConfig ^. C.tcShelleyGenesisL)
    (fixedEpochInfo pEpochSize (slotLength params))

emulatorGenesisWindow :: GenesisWindow
emulatorGenesisWindow = GenesisWindow window
  where
    -- A good default value for eras that never fork is
    -- 3k/f, with k = 2160 and f = 20 (given by the Genesis team).
    window = (3 * 2160) `div` 20

-- | A sensible default 'EraHistory' value for the emulator
emulatorEraHistory :: Params -> C.EraHistory
emulatorEraHistory params = C.EraHistory (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list)
  where
    one =
      Ouroboros.nonEmptyHead $
        Ouroboros.getSummary $
          Ouroboros.neverForksSummary (pEpochSize params) (slotLength params) emulatorGenesisWindow
    list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil
