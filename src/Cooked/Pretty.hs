-- | This module exposes off-chain pretty-printing functions for transaction
-- skeletons, utxo states, addresses, pubkey hashes, values, etc.
--
-- We provide the 'PrettyCooked' class and instances for common Plutus types.
-- We don't rely on 'PP.Pretty' from "Prettyprinter" in order to define better
-- printers for Plutus types which already have instances of 'PP.Pretty'. Also,
-- 'PrettyCooked' makes it possible to optionally modify pretty printing
-- settings 'PrettyCookedOpts' (e.g. length of printed hashes).
--
-- == Requirements on datum and redeemers
--
-- Datums and redeemers are required to have a 'PrettyCooked' instance.
--
-- For trivial datatypes, you can rely on Show by using 'PrettyPrinter.viaShow'
-- from "Prettyprinter": @prettyCooked = Prettyprinter.viaShow@.
--
-- For more complex datatypes, you can rely on existing 'PrettyCooked'
-- instances. Prefer implementing the 'prettyCookedOpt' function and relay the
-- 'PrettyCookedOpts' settings to other printers.
--
-- @ data Foo = Bar Api.Value | Baz Api.PubkeyHash Api.Value
--
--     instance PrettyCooked Foo where
--       prettyCookedOpt pcOpts (Bar value) =
--         "Bar" <+> prettyCookedOpt pcOpts value
--       prettyCookedOpt pcOpts (Baz pkh value) =
--         prettyItemize
--           "Baz"
--           "-"
--           [ "user:" <+> prettyCookedOpt pcOpts pkh,
--             "deposit:" <+> prettyCookedOpt pcOpts value ]
-- @
--
-- The 'prettyItemize' function is useful to nicely lay down nested lists of
-- elements. Since we manipulate regular 'PrettyPrinter.Doc' values, any
-- function from "Prettyprinter" can be used to implement your printers.
--
-- == How to pretty print?
--
-- Pretty printing of transaction skeletons and UTxO states is done
-- automatically by the end-user functions provided in
-- "Cooked.MockChain.Testing".
--
-- To do it manually, use instances of 'PrettyCooked', 'PrettyCookedL' or
-- 'PrettyCookedM' defined in 'Cooked.Pretty.Skeleton' or
-- 'Cooked.Pretty.MockChain' such as the one for @MockChainReturn a UtxoState@.
module Cooked.Pretty (module X) where

import Cooked.Pretty.Class as X
import Cooked.Pretty.Hashable as X
import Cooked.Pretty.MockChain as X ()
import Cooked.Pretty.Options as X
import Cooked.Pretty.Plutus as X ()
import Cooked.Pretty.Skeleton as X ()
