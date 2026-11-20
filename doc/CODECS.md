# Codecs and registries

A chain index can be saved to a JSON file and loaded back, typically to
persist state across blockchain runs. Outputs hide their datums and owners
behind existential types, so their types must be named in the file and
recovered when loading. This relies on:

1. **type keys** (`HasTypeKey`): the name under which a type is serialized,
2. **codecs** (`Codec`): type witnesses carrying the instances needed to decode,
3. **registries** (`Registry`, `Registries`): maps from type keys to codecs.

## Type keys

```haskell
class HasTypeKey t where
  typeKey :: String
  default typeKey :: (Typeable t) => String
```

Every datum type, and every pubkey or script type used through `UserPubKey`
or `UserScript`, needs an instance, as `DatumConstrs`, `PubKeyConstrs` and
`ScriptConstrs` require it. Three options:

```haskell
-- default key
instance HasTypeKey MyDatum

-- default key, derived (requires DeriveAnyClass)
data MyDatum = ...
  deriving stock (Show, Eq)
  deriving anyclass (HasTypeKey)

-- explicit key
instance HasTypeKey MyDatum where
  typeKey = "my-datum"
```

The default key is the fully qualified name of the type, e.g.
`"My.Module.MyDatum"`, so homonymous types do not collide. It changes if the
type moves to another module, which makes saved files unreadable: prefer an
explicit key for long-lived files.

Cooked provides instances for `()`, `Integer`, `Bool`, `BuiltinByteString`,
`BuiltinData`, pairs, `PubKeyHash`, `Wallet`, `TxSkelSignatory`,
`MultiPurposeScript`, `TypedValidator a` and `Versioned` `Script`,
`MintingPolicy`, `Validator` and `StakeValidator`.

## Codecs and registries

A `Codec c` witnesses a type satisfying the constraint `c`, and is built with
a type application: `Codec @MyDatum`. A `Registry c` maps type keys to codecs:

| Registry         | Constraint      | Decoding via |
| ---------------- | --------------- | ------------ |
| `DatumRegistry`  | `DatumConstrs`  | `FromData`   |
| `PubKeyRegistry` | `PubKeyConstrs` | `FromJSON`   |
| `ScriptRegistry` | `ScriptConstrs` | `FromJSON`   |

They are built with `mkRegistry :: [Codec c] -> Registry c` or
`registerType @T`. A codec registered under an existing key silently replaces
the previous one.

`Registries` gathers the three registries:

- `defaultRegistries` covers the types natively handled by cooked: datums
  `BuiltinData` and `()`, pubkeys `Wallet`, `TxSkelSignatory` and
  `PubKeyHash`, and the script types listed above (except `TypedValidator`).
- `emptyRegistries` (or `mempty`) contains nothing.
- `extendDatumRegistry`, `extendPubKeyRegistry` and `extendScriptRegistry` add
  codecs; `r1 <> r2` extends `r1` with `r2`.

In practice, users only extend the datum registry with their own types:

```haskell
myRegistries :: Registries
myRegistries = extendDatumRegistry (mkRegistry [Codec @MyDatum]) defaultRegistries
```

## Saving and loading

`Cooked.Runtime.State` provides:

- `saveChainIndex path chainIndex`, which also backs up the previous file to
  `path.bak`, and needs no registries;
- `loadChainIndex path registries allowMissing allowMalformed`, where the
  booleans make it fall back to an empty chain index when the file is missing
  or malformed (unknown type keys included);
- `chainIndexToJSON` and `chainIndexFromJSON`, the underlying conversions.

### In blockchain runs

`BlockChainConf` (in `Cooked.BlockChain.Config`) has two fields for this,
used by `runBlockChainFromConf`:

- `bccChainIndexSource :: ChainIndexSource`, the initial chain index: either
  `DirectChainIndex chainIndex`, or
  `ChainIndexFromFile path registries allowMissing allowMalformed`, which calls
  `loadChainIndex`;
- `bccChainIndexTarget :: Maybe FilePath`, where to save the final chain index
  with `saveChainIndex`, if anywhere.

Templates set both to the same file, accepting a missing file but not a
malformed one:

| Template                                      | Registries used               |
| --------------------------------------------- | ----------------------------- |
| `blockChainConfWithDefaultRegistriesTemplate` | `defaultRegistries`           |
| `blockChainConfWithExtraRegistryTemplate`     | `defaultRegistries <> extra`  |
| `blockChainConfWithGivenRegistryTemplate`     | exactly the given ones        |

```haskell
conf =
  blockChainConfWithExtraRegistryTemplate
    connectInfo
    "chain-index.json"
    (extendDatumRegistry (mkRegistry [Codec @MyDatum]) mempty)
```

`blockChainConfDefaultTemplate` neither loads nor saves any file.

## Pitfalls

- **Unregistered type**: loading fails with e.g. `deserializeDatum: unknown
  datum type: My.Module.MyDatum`. Note that `emptyRegistries` and
  `blockChainConfWithGivenRegistryTemplate` do not include the defaults.
- **Key collision**: only the last registered codec is kept, so data of the
  other type is decoded with the wrong instance. Default keys never collide;
  explicit ones might.
- **Changed key**: to read old files, give the type an explicit key equal to
  the old one, or rewrite the keys in the file.
- **Private keys**: `Wallet`s and `TxSkelSignatory`s are saved with their
  private keys in plain text, until proper wallet support exists. Handle files
  and their `.bak` backups with care.
