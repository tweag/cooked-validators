module Cooked.Attack
  ( module Cooked.Attack.Common,
    module Cooked.Attack.DatumHijacking,
    module Cooked.Attack.DoubleSat,
    module Cooked.Attack.DupToken,
    module Cooked.Attack.OutPermutations,
    module Cooked.Attack.TamperDatum,
  )
where

import Cooked.Attack.Common
import Cooked.Attack.DatumHijacking
import Cooked.Attack.DoubleSat
import Cooked.Attack.DupToken
import Cooked.Attack.OutPermutations
import Cooked.Attack.TamperDatum

-- The idea of this module: Turning optics into attacks
-------------------------------------------------------
--
-- In cooked-validators, a _single transaction attack_ on a smart contract is a
-- function that modifies one transaction: An attacker applies this function to
-- a transaction in an otherwise normal chain of transactions, somehow fools the
-- validator script(s), and profits. So, attacks in cooked-valiadators should
-- modify 'TxSkel's.
--
-- It would be nice to have a collection of parametric attacks, together with an
-- easy way to add new attacks to that collection. This module and its
-- submodules contain the beginnings of such a collection, and also a hopefully
-- useful mechanisms to extend it.
--
-- Since most attacks are of the form "look deep into the nested data types
-- within a 'TxSkel' and change something", The idea is to use the optics
-- defined in "Cooked.Tx.Constraints.Optics" and turn them into attacks.
--
-- Optic attack example: Token duplication
------------------------------------------
--
-- There's a 'dupTokenAttack' in "Cooked.Attack.DupToken", and its
-- implementation is quite readable; the purpose of the following is not to
-- explain that function in detail, but to motivate how one should write new
-- attacks using this module.
--
-- A _token duplication_ attack consists in trying to increase the amount of
-- tokens a transaction mints and paying the surplus to an attacker. A naive
-- idea to implement the token duplication attack would then be
--
-- > naiveDupTokenAttack :: Wallet -> (Value -> Value) -> TxSkel -> TxSkel
-- > naiveDupTokenAttack attacker increaseValue =
-- >   paySurplusTo attacker . over (mintsConstraintT % valueL) increaseValue
--
-- where @paySurplusTo :: Wallet -> TxSkel -> TxSkel@ is a suitable function
-- that modifies a 'TxSkel' to play any extra minted tokens to a given
-- wallet.
--
-- This is idea is almost right, save for the type of @naiveDupTokenAttack@: If
-- @increaseValue@ did not in fact _change_ any of the minted values, or if
-- there were no 'MintsConstraint's in the transaction under consideration, we
-- have no way to detect this failure. Also, depending on the current state of
-- the 'MockChain' there might be more than one possibly interesting way to
-- modify the initial transaction. That's why we set
--
-- > type Attack = MockChainSt -> TxSkel -> [TxSkel]
--
-- and the module "Cooked.Attack.Common" provides functions like
--
-- > mkAttack :: Is k A_Traversal => Optic' k is TxSkel a -> (a -> Maybe a) -> Attack
--
-- which is a kind of "'over' with failure": The list of modified 'TxSkel's
-- returned by @mkAttack optic f state skel@ will be nonempty if and only if
--
-- - the @optic@ being used in the attack has at least one focus on the input
--   @skel@, and
--
-- - the function @f@ returns @Just@ on at least one of the foci.
--
-- In that case, the returned list will contain exactly one modified 'TxSkel',
-- with all foci modified by @f@.
--
-- There are also other functions like 'mkAttack' that return more than one
-- modification in "Cooked.Attack.Common".
--
-- Note that we can use 'mkAttack' (or a similar fuction) to write
-- 'dupTokenAttack' in a contract-agnostic manner. Also note that, despite
-- (because?) of the generality of such attacks, they are relatively easy to
-- implement. Our growing collection of optics in "Cooked.Tx.Constraints.Optics"
-- and other helpers will hopefully mean that it will become easier and easier
-- to write attacks.
