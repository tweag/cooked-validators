-- | Tests for 'Cooked.Attack.PeerTampering'.
module Spec.Attack.PeerTampering where

import Cooked
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.NonDet
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

-- | The public key hash of a known wallet.
pkh :: Integer -> Api.PubKeyHash
pkh = Script.toPubKeyHash . wallet

-- | A skeleton in which @wallet 1@ occurs twice as an allocated peer (once as
-- an output owner and once as a signatory) and @wallet 2@ occurs once. This
-- exercises both branches of 'txSkelAllocatedPeersT'.
baseSkel :: TxSkel
baseSkel =
  txSkelEmulatorTemplate
    { txSkelOutputs = [wallet 1 `receives` Value (Script.lovelace 3_000)],
      txSkelSignatories = txSkelSignatoriesFromList [wallet 1, wallet 2]
    }

-- | The list of allocated peers of a skeleton, in traversal order (output
-- owners first, then signatories).
allocatedPeers :: TxSkel -> [Api.PubKeyHash]
allocatedPeers = toListOf (txSkelAllocatedPeersT % userPubKeyHashI)

-- | Running a peer tampering attack purely, returning for every resulting
-- transaction its labels together with its allocated peers and the peers the
-- attack reports as modified.
runPeerTampering ::
  TxSkel ->
  PeerTamperingParams '[Tweak, NonDet] ->
  [(Set.Set TxSkelLabel, [Api.PubKeyHash], [Api.PubKeyHash])]
runPeerTampering skel params =
  fmap (\(s, modified) -> (view txSkelLabelsL s, allocatedPeers s, modified))
    . run
    . runNonDet
    $ runTweak skel (peerTamperingAttack params)

-- | Replacing every occurrence of a peer in a single transaction rewrites all
-- of its foci, records them in the label, and returns them.
replaceAllTest :: TestTree
replaceAllTest =
  testCase "replacing a peer in one transaction rewrites all its foci and records them" $
    [ ( Set.singleton (TxSkelLabel (PeerTamperingLabel [pkh 1, pkh 1])),
        [pkh 3, pkh 3, pkh 2],
        [pkh 1, pkh 1]
      )
    ]
      @=? runPeerTampering baseSkel (purePeerTamperingParams OneBranchForAllFoci [(pkh 1, [pkh 3])])

-- | Offering several replacements for a peer branches into every combination of
-- per-focus choices.
replaceBranchingTest :: TestTree
replaceBranchingTest =
  testCase "offering several replacements branches on every combination of per-focus choices" $
    assertSameSets
      [ [pkh 3, pkh 3, pkh 2],
        [pkh 3, pkh 4, pkh 2],
        [pkh 4, pkh 3, pkh 2],
        [pkh 4, pkh 4, pkh 2]
      ]
      (fmap (\(_, peers, _) -> peers) $ runPeerTampering baseSkel (purePeerTamperingParams OneBranchForAllFoci [(pkh 1, [pkh 3, pkh 4])]))

-- | 'singlePeerTamperingParams' branches per focus, producing one transaction
-- per occurrence of the replaced peer, each rewriting a single focus.
replacePerFocusTest :: TestTree
replacePerFocusTest =
  testCase "singlePeerTamperingParams rewrites one focus per transaction" $
    assertSameSets
      [ [pkh 3, pkh 1, pkh 2],
        [pkh 1, pkh 3, pkh 2]
      ]
      (fmap (\(_, peers, _) -> peers) $ runPeerTampering baseSkel (singlePeerTamperingParams (wallet 1) (wallet 3)))

-- | The attack yields no transaction when the targeted peer is absent.
replaceAbsentTest :: TestTree
replaceAbsentTest =
  testCase "no transaction is produced when the targeted peer is absent" $
    []
      @=? runPeerTampering baseSkel (purePeerTamperingParams OneBranchForAllFoci [(pkh 9, [pkh 3])])

tests :: TestTree
tests =
  testGroup
    "Peer tampering"
    [ replaceAllTest,
      replaceBranchingTest,
      replacePerFocusTest,
      replaceAbsentTest
    ]
