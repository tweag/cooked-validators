# Balancing transactions in `cooked-validators`

`cooked-validators` provides its own transaction abstraction, `TxSkel`. The
transaction skeleton contains both concrete elements that should appear in the
generated Cardano transaction, such as inputs and outputs, and various options
piloting this generation. In particular, this transaction generation contains a
pass of balancing which is heavily controlled by those options. In this
document, we explain what balancing entails, how it is currently implemented,
and what are the options impacting this mechanism.

## Balancing: Kesako?

### Balancing requirements

In Cardano, transactions need to be balanced when submitted for validation. This
means that the equation `input value + minted value = output value + burned
value + fee` must be satisfied to go into phase 2 of the validation process. In
addition, collaterals must be provided to account for transaction failing in
phase 1. They are related to the fee with the following inequation:
`totalCollateral >= fee * feeToCollateralRatio` and they must satisfy their own
preservation equation: `collateralInputs = totalCollaterals +
returnCollaterals`. Finally, the actual required fee for a given transaction
depends on the size of the transaction as well as the (not yet executed)
resources used by the scripts ran when validating the transaction.

### Balancing mechanism

Our balancing mechanism consists in transforming a given transaction skeleton
into a balanced version with sufficient fee to account for the size of the
resulting transaction, and sufficient collaterals to account for those fee. This
is a complicated process because balancing a transaction will usually results in
creating new inputs and outputs, thus resulting in higher fee and collaterals,
thus needing new inputs to account for those, and so on. In the process, we also
want to compute reasonable fee (if not optimal) that would resemble the actual
fee estimated on-chain.

Overall, our balancing function is signed as follows:

``` haskell
balanceTxSkel :: (MonadBlockChainBalancing m) => TxSkel -> m (TxSkel, Fee, Collaterals, Wallet)
```

It takes a skeleton as input living into a `MonadBlockChainBalancing`
environement, and returns:
- A balanced skeleton
- The associated fee accounted for in the skeleton
- The associated set of collateral inputs to account for the chosen fee
- The return collateral wallet to return excess collateral to

## Balancing options

There are various options piloting transaction generation that are related to
balancing. Here is an overview of those options with the semantics of their
various constructors, alongside their default value.

### Balancing policy

Whether to automatically balance a skeleton or not, and which wallet to use as a
balancing wallet.

``` haskell
data BalancingPolicy
  = BalanceWithFirstSigner -- default
  | BalanceWith Wallet
  | DoNotBalance
```

This options turns on or off the automated balancing feature. When the feature
is on, a balancing wallet needs to be provided. The balancing wallet is a
specific wallet that is required during balancing. The address of the balancing
wallet is used when extra value is generated through balancing (which is almost
always the case) and needs to be returned somewhere. When the option
`BalancingUtxos` requires it, the balancing wallet is also used to provide utxos
for missing inputs in the transaction.

Here is the semantics of the constructors:
* `BalanceWithFirstSigner`: Turns on auto-balancing, and will use the first
  wallet in the list of signers as the balancing wallet. If the list of signers
  is empty, this throws the following error:
  ``` haskell
  FailWith "Can't select balancing wallet from the signers lists because it is empty."
  ```
  Note that an empty list of signers would lead to a validation error anyway due
  to collaterals requirements.
* `BalanceWith Wallet`: Turns on auto-balancing, and uses the given wallet as
  balancing wallet. Note that if the balancing process requires additional utxos
  from that wallet to be consumed (which will most certainly happen), then this
  wallet will need to be a signer of the transaction for the generated
  transaction to be successfully validated.
* `DoNotBalance`: Turns off auto balancing. The transaction skeleton, and in
  particular its inputs and outputs, will remain unchanged through the
  process. Note that it will still be possible to balance those transactions by
  hand by providing a manual fee (see [`FeePolicy`](#fee-policy)).

### Balancing utxos

Which utxos to pick from to account for the missing value in the inputs of the
skeleton.

``` haskell
data BalancingUtxos
  = BalancingUtxosAutomatic -- default
    BalancingUtxosWith (Set Api.TxOutRef)
```

If auto-balancing is enabled, additional utxos will be added to the transaction
inputs to account for any missing value there. These utxos need to be found
somewhere. The options gives two choice as to where to look for them. Note that
the set of utxos *considered* for balancing is not necessarily equal to the
eventual set of balancing utxos. Instead, the latter is included in the former.

Here is the semantics of the constructors:
* `BalancingUtxosAutomatic`: The utxos that will be considered to be used for
  balancing purposes are those possessed by the balancing wallet, and which only
  contain a value. In particular, utxos with a reference script, a datum or a
  staking credential will not be considered for balancing.
* `BalancingUtxosWith (Set Api.TxOutRef)`: The given set of utxos will be
  considered for balancing purpose, minus the utxos belonging to a script. Those
  are not eligible as they would need to be consumed using an unknown
  redeemer. Note that any additional element in the provided utxos (staking
  credential, reference script or datum) will be lost in the balancing process
  if they get chosen for balancing. This option is thus inherently less safe
  than the former but offers more control in return.
  
If auto-balancing is disabled, this option will be ignored.

### Balance output policy

Whether to add up extra balancing value to existing output, or create a new one.

``` haskell
data BalanceOutputPolicy
  = AdjustExistingOutput -- default
  | DontAdjustExistingOutput
```

If auto-balancing is enabled, new utxos will be consumed to account for any
missing value in inputs. In the process, and since those utxos have a fixed
value, extra value might be fed into the transaction, which needs to be given
back to the balancing wallet in return. This option controls how this extra
payment is performed within the transaction.

Here is the semantics of the constructors:
* `AdjustExistingOutput`: Any extra value will be added to the first output of
  the transaction that goes to the balancing wallet. If this output contains
  other pieces of informations, they will be kept intact. Only the value will be
  changed. If no such output exists, the semantics will be the same as using
  `DontAdjustExistingOutput`. This option is to be preferred when one wants to
  limitate the number of utxos present in the index.
* `DontAdjustExistingOutput`: Any extra value will be given back to the
  balancing wallet using a new transaction output. This output will be placed at
  the end of the list of outputs of the transaction. This option is to be
  preferred when ones wants to clearly separate what originated from the initial
  unbalanced transaction, and what came from the balancing process.
  
Note that both these options lead to changes in the outputs of the transaction
that are *order-preserving*. This is very important since many smart contracts
require a certain outputs order, and we don't want the balancing process to
alter it somehow.

If auto-balancing is disabled, this option will be ignored.

### Fee policy

Whether to use manual fee or automatically assess a suitable fee.

``` haskell
data FeePolicy
  = AutoFeeComputation -- default
  | ManualFee Integer
```

Since fee are a part of the balancing equation that a transaction must satisfy,
their value is an inherent part of the balancing process. This option controls
whether these fee should be assessed automatically during the balancing process,
of if the balancing should occur around a given fee.

Here is the semantics of the constructors:
* `AutoFeeComputation`: The balancing process will perform a dichotomic search
  between minFee and maxFee (both depending on some protocol parameters) to
  search for an optimal fee around which the transaction can be balanced given
  the available balancing utxos (see [`BalancingUtxos`](#balancing-utxos)). The
  balancing process guarantees that this fee is optimal regarding Cardano's fee
  estimate. In other words, a lower fee will be below Cardano's estimate for the
  current transaction fee. Note that the actual required fee could be smaller
  because this estimate is conservative, but it bears guarantees that the actual
  fee will not be higher that this estimate, which is the most important. This
  option should be preferred when one wants to be paying as little fee as
  possible.
* `ManualFee Integer`: The balancing process will be performed around the given
  fee. No check will be done to assess whether this fee will be enough when
  validating the transaction. This option is to be preferred when one wants to
  prioritize performance at the cost of a non-optimal fee.
  
If auto-balancing is disabled, the first option will choose the maximum fee
authorized by the protocol parameters, while the second will keep the
user-provided fee.

### Collateral utxos

Which utxos to pick from as collateral inputs.

``` haskell
data CollateralUtxos
  = CollateralUtxosFromBalancingWallet -- default
  | CollateralUtxosFromWallet Wallet
  | CollateralUtxosFromSet (Set Api.TxOutRef) Wallet
```

In addition to the regular utxos consumed in a transaction, additional utxos
need to be provided in case the validation fails in phase 2. These utxos should
be sufficient to cover a certain total collateral, which is a multiple of the
given transaction fee (usually 1.5, depending on the protocol parameters). The
excess can be given back to a certain wallet through an output called a return
collateral. This option pilots which utxos should be *considered* by the
balancing mechanism to be provided in the transaction. Similarly to
[`BalancingUtxos`](#balancing-utxos), the final set of utxos will not
necessarilly be equal to the considered set, but will be included in it. This is
especially true for collaterals since protocol parameters also dictate a maximum
number of allowed collateral utxos.

Here is the semantics of the constructors:
* `CollateralUtxosFromBalancingWallet`: Use only-value utxos belonging to the
  balancing wallet. The return collateral will be sent to the balancing
  wallet. Synonym to `CollateralUtxosFromWallet (balancingWallet)`.
* `CollateralUtxosFromWallet Wallet`: Use only-value utxos belonging to the
  given wallet. For the transaction to be validated, this wallet will need to
  sign it. The return collateral will be sent to the same wallet.
* `CollateralUtxosFromSet (Set Api.TxOutRef) Wallet`: Use utxos from the given
  set, and send back return collaterals to the given wallet. Note that if some
  of those utxos belong to a script, and are chosen by the balancing mechanism,
  the validation will fail as only PK utxos are allowed as collaterals by the
  ledger rules. This option is inherently less safe than the others, as the
  additional information contained in the chosen utxo will be lost in the
  balancing process. To be used with caution.

If the auto-balancing is disabled, the computing for the collateral will still
happen around the fee chosen by the [`FeePolicy`](#fee-policy). In addition, it
is not currently possible for user to specify a specific amount of collaterals
to be used in a given transaction (as opposed to fee). This was a deliberate
choice to limitate the user interaction with the collateral mechanism as this is
very seldom used and `cooked-validators` currently does not support to keep
issuing transactions after a validation failure, thus rendering the collateral
mechanism almost irrelevant outside of transaction validation.

## Balancing algorithm

The balancing algorithm consists in taking as input a skeleton in a
`MonadBlockChain` environment, and given back this skeleton with associated
fees, collaterals and return collateral wallet. These four elements are piloted
by the [balancing options](#balancing-options). It is composed of several
parts. While not all of them will be subjected to a detailed description here
(the code is extensively commented), we will detailed the most important or
challenging ones.

### Reaching a given value with a set of utxos

When balancing a transaction, we take as input a set of candidate utxos, and
need to decide on a subset of those which is:
- sufficient to cover missing value in inputs of the skeleton.
- either providing exactly the right amount of funds, or enough extra to sustain
  an additional output in terms of minimal ada (as excess will be given back to
  the balancing wallet).
- minimal in a way, as we do not want to generate big transactions and to
  systematically take the whole candidate set for balancing. The definition we
  take as minimal is that the subset does not contain any utxos that does not
  contribute to reaching the final value. In other words, taking any utxo out of
  the chosen subset should not amount to the required value.
  
We have the same kind of requirement to compute a suitable subset of
collaterals, with the additional one of having a maximum number of elements in
the subsets.
  
Function `reachValue` performs the candidates subset computation. Here is its
signature:

``` haskell
reachValue :: [(Api.TxOutRef, Api.TxOut)] -> Api.Value -> Integer -> [[(Api.TxOutRef, Api.TxOut)], Api.Value)]
```

This function takes a list of utxos coupled with their associated outputs, a
value (the target to be reached) and an integer, which represents the maximum
number of elements candidate subsets should contain. The function is recursive
and computes in 2^n, where n is the number of input utxos, as all `find all
subsets` do. The idea is to go through the input list and decide to either pick
or drop the first element. It is optimized four ways compared to a regular `find
all subsets` algorithm:
- at each step, we check whether taking the whole remaining list would be
  sufficient to reach the value. If not, we stop the computation.
- we limit our search to the number of elements stipulated in the parameters of
  the function. For the limitation on collaterals inputs (usually at 3) this
  makes the search much faster.
- we only consider picking the current element if it contributes to reaching the
  target. In other words, if the intersection between the current element value
  and the target value is empty, we skip it.
- once the target is reached, we directly stop the search and do not attempt to
  add more elements to the subset.
  
The function returns a list of lists of candidates with their associated surplus
value beyond the target.

We can then sort the candidate sets by the minimal ada required to pay back the
surplus value to the balancing wallet. The first element of the sorted list is
our current best candidate set of utxos to either balancing the transaction or
the collaterals, which satisfy all our criteria.
