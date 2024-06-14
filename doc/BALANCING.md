# Balancing transactions in `cooked-validators`

The `cooked-validators` library introduces its own transaction abstraction,
TxSkel. This transaction skeleton includes both concrete elements, such as
inputs and outputs, that will appear in the generated Cardano transaction, as
well as various options that guide the generation process. Notably, this
generation process includes a balancing phase that is significantly influenced
by these options. In this document, we explain what balancing involves, how it
is currently implemented, and which options affect this mechanism.

## Balancing: Kesako?

### Balancing requirements
In Cardano, transactions must be balanced before they can be submitted for
validation. This means the equation `input value + minted value = output value +
burned value + fee` must be satisfied to proceed to phase 2 of the validation
process. Additionally, collaterals must be provided to account for transaction
failures in phase 2. These collaterals are related to the fee by the following
inequation: `totalCollateral >= fee * feeToCollateralRatio`, and they must
satisfy their own preservation equation: `collateralInputs = totalCollaterals +
returnCollaterals`. Lastly, the actual required fee for a given transaction
depends on the size of the transaction and the (not yet executed) resources used
by the scripts during validation.

### Balancing mechanism

Our balancing mechanism transforms a given transaction skeleton into a balanced
version, ensuring it has sufficient fees to cover the resulting transaction size
and adequate collaterals to cover those fees. This process is complex because
balancing a transaction often leads to the creation of new inputs and outputs,
which increases fees and collaterals. This, in turn, necessitates additional
inputs to cover these costs, creating a cyclical challenge. Additionally, we aim
to compute reasonable (if not optimal) fees that approximate the actual fees
estimated on-chain.

Our balancing function is signed as follows:

``` haskell
balanceTxSkel :: (MonadBlockChainBalancing m) => TxSkel -> m (TxSkel, Fee, Collaterals, Wallet)
```

This function takes a skeleton within a `MonadBlockChainBalancing` environement,
and returns:
- A balanced skeleton
- The associated fee accounted for by the skeleton
- The set of collateral inputs to cover the chosen fee
- The return collateral wallet to return excess collateral

## Balancing options

Various options control transaction generation, many of them related to
balancing. Here's an overview of these options along with the semantics of their
constructors and default values.

### Balancing policy

This policy determines whether a skeleton should be automatically balanced and
specifies which wallet to use as the balancing wallet.

``` haskell
data BalancingPolicy
  = BalanceWithFirstSigner -- default
  | BalanceWith Wallet
  | DoNotBalance
```

The balancing wallet is a critical component needed during the balancing
process. Its address serves multiple purposes: it is utilized when surplus value
is generated through balancing, which typically occurs, and this excess value
requires a destination. Additionally, when the `BalancingUtxos` option
necessitates it, the balancing wallet also supplies UTXOs to cover any missing
inputs in the transaction.

Here are the options available:
* `BalanceWithFirstSigner`: Enables auto-balancing and uses the first wallet in
  the list of signers as the balancing wallet. If the list of signers is empty,
  an error is thrown:
  
  ``` haskell
  FailWith "Can't select balancing wallet from the signers lists because it is empty."
  ```
  
  Note that an empty list of signers would lead to a validation error anyway due
  to collateral requirements.
* `BalanceWith Wallet`: Enables auto-balancing and uses the specified wallet as
  the balancing wallet. If the balancing process requires additional UTXOs from
  this wallet (which is highly likely), the wallet must be a signer of the
  transaction for successful validation.
  
* `DoNotBalance`: Disables auto-balancing. The transaction skeleton, including
  its inputs and outputs, remains unchanged throughout the process. However,
  it's still possible to manually balance these transactions by providing a
  custom fee (see [`FeePolicy`](#fee-policy)).

### Balancing utxos

Which utxos to pick from to account for the missing value in the inputs of the
skeleton.

``` haskell
data BalancingUtxos
  = BalancingUtxosAutomatic -- default
    BalancingUtxosWith (Set Api.TxOutRef)
```

When auto-balancing is enabled, extra UTXOs will be included in the transaction
inputs to cover any shortfall. These UTXOs must be sourced from somewhere, and
there are two options for where to find them. It's important to note that the
set of UTXOs considered for balancing is not necessarily the same as the final
set used; rather, the final set is a subset of the initially considered UTXOs.

Here is the semantics of the constructors:
* `BalancingUtxosAutomatic`: The UTXOs considered for balancing will be those
  owned by the balancing wallet and containing only a value. UTXOs with a
  reference script, a datum, or a staking credential will not be considered for
  balancing.
* `BalancingUtxosWith (Set Api.TxOutRef)`: The specified set of UTXOs will be
  considered for balancing, excluding those associated with a script, as they
  would require an unknown redeemer to be consumed. Any additional elements in
  the provided UTXOs (staking credential, reference script, or datum) will be
  lost if chosen for balancing. This option is less safe than the former but
  provides more control.
  
If auto-balancing is disabled, this option will be ignored.

### Balance output policy

Whether to add up extra balancing value to existing output, or create a new one.

``` haskell
data BalanceOutputPolicy
  = AdjustExistingOutput -- default
  | DontAdjustExistingOutput
```

If auto-balancing is enabled, new unspent utxos will be used to cover any
shortfall in input values. During this process, because these utxos have a fixed
value, the transaction may end up with excess value, which must be returned to
the balancing wallet. This setting determines how this excess payment is handled
within the transaction.

Here are the options for handling the extra payment:
* `AdjustExistingOutput`: Any excess value will be added to the first output of
  the transaction that is destined for the balancing wallet. If this output
  already contains other information, that information will remain unchanged;
  only the value will be adjusted. If no such output exists, this option behaves
  the same as `DontAdjustExistingOutput`. This option is preferable when
  minimizing the number of UTXOs in the index is desired.
* `DontAdjustExistingOutput`: Any excess value will be returned to the balancing
  wallet through a new transaction output. This new output will be appended to
  the end of the list of transaction outputs. This option is preferable when
  maintaining a clear separation between the original unbalanced transaction and
  the balancing process is important.
  
It's crucial to note that both options ensure the transaction outputs maintain
their original order. This preservation is significant for smart contracts that
rely on specific output ordering, ensuring the balancing process does not
unintentionally alter this order.

If auto-balancing is disabled, this setting becomes irrelevant and will be
disregarded.

### Fee policy

Whether to use manual fee or automatically assess a suitable fee.

``` haskell
data FeePolicy
  = AutoFeeComputation -- default
  | ManualFee Integer
```

Since fees are integral to ensuring a transaction's balance, their determination
is crucial in the balancing process. This setting governs whether these fees
should be automatically computed during balancing or if a specific fee should be
used.

Here's how each option operates:

* `AutoFeeComputation`: The balancing process will conduct a dichotomous search
  between minFee and maxFee (both dependent on protocol parameters) to find an
  optimal fee around which the transaction can be balanced, considering
  available balancing UTXOs (see [`BalancingUtxos`](#balancing-utxos)). This
  approach ensures the fee aligns with Cardano's fee estimate. While this
  estimate may be conservative, it guarantees that the actual fee will not
  exceed this estimate, which is crucial. This option is recommended for
  minimizing fees.
  
* `ManualFee`: The balancing process will proceed with the transaction
  using the specified fee. No check will verify if this fee is adequate while
  allowing a successful validation. This option is suitable when prioritizing
  transaction generation performance over optimal fee calculation.
  
If auto-balancing is disabled, the behavior is as follows:

* For the first option (`AutoFeeComputation`), the maximum fee permitted by
  protocol parameters will be chosen and associated to the unbalanced
  transaction.
  
* For the second option (`ManualFee`), the fee provided by the user will
  be associated to the unbalanced transaction without adjustment.

These choices ensure that fees are managed appropriately according to the needs
of the transaction and the desired balance between cost and performance.

### Collateral utxos

Which utxos to pick from as collateral inputs.

``` haskell
data CollateralUtxos
  = CollateralUtxosFromBalancingWallet -- default
  | CollateralUtxosFromWallet Wallet
  | CollateralUtxosFromSet (Set Api.TxOutRef) Wallet
```

In addition to the regular UTXOs consumed in a transaction, additional UTXOs
must be provided to cover potential phase 2 validation failures. These UTXOs
need to be sufficient to meet a specified total collateral requirement,
typically 1.5 times the transaction fee based on protocol parameters. Any
surplus can be returned to a designated wallet through an output known as return
collateral. This setting determines which UTXOs the balancing mechanism should
consider for inclusion in the transaction. Similar to
[`BalancingUtxos`](#balancing-utxos), the final set of UTXOs included may not
necessarily match the considered set, especially for collaterals, as protocol
parameters also impose limits on the number of allowable collateral UTXOs
(typically 3).

Here are the options available:

* `CollateralUtxosFromBalancingWallet`: Use UTXOs containing only value from the
  balancing wallet. The return collateral will be directed back to the balancing
  wallet. This option is synonymous with `CollateralUtxosFromWallet
  (balancingWallet)`.
* `CollateralUtxosFromWallet Wallet`: Use UTXOs containing only value from the
  specified wallet. Transactions using these UTXOs will require the signing of
  the wallet owner for validation. The return collateral will also be sent to
  this same wallet.
* `CollateralUtxosFromSet (Set Api.TxOutRef) Wallet`: Utilize UTXOs from the
  provided set and direct return collaterals to the designated wallet. Note that
  if any of these UTXOs belong to a script and are selected by the balancing
  mechanism, validation will fail because only UTXOs controlled by public keys
  are permissible as collaterals according to ledger rules. Exercise caution
  with this option as it may involve losing additional information contained
  within the chosen UTXO during the balancing process.

If auto-balancing is disabled, the collateral calculation will still be based on
the fee determined by the [`FeePolicy`](#fee-policy). Currently, users cannot
specify the exact amount of collateral to be used in a transaction, unlike
fees. This limitation is intentional to minimize user interaction with the
collateral mechanism, which is rarely needed. Additionally, `cooked-validators`
does not support issuing new transactions after a validation failure, making the
collateral mechanism less relevant outside of transaction validation
requirements.

## Balancing algorithm

The balancing algorithm operates by taking a transaction skeleton as input
within a `MonadBlockChain` environment and returning this skeleton with
associated fees, collaterals, and a return collateral wallet. These four
elements are governed by the [balancing options](#balancing-options). The
algorithm comprises several components. While not all components will be
described in detail here (the code is thoroughly commented), we will focus on
the most important and challenging aspects.

### Reaching a given value with a set of utxos

When balancing a transaction, we start with a set of candidate UTXOs and need to
decide on a subset that:
- Is sufficient to cover the missing value in the inputs of the skeleton.
- Either provides exactly the right amount of funds or enough extra to sustain
  an additional output in terms of minimal ADA (as the excess will be returned
  to the balancing wallet).
- Is minimal, meaning it does not include any UTXOs that do not contribute to
  reaching the required value. In other words, removing any UTXO from the chosen
  subset should prevent achieving the required value.
  
We also have similar requirements for computing a suitable subset of
collaterals, with the additional constraint of having a maximum number of
elements in the subsets.
  
The function `reachValue` performs this subset computation. Here is its
signature:

``` haskell
reachValue :: [(Api.TxOutRef, Api.TxOut)] -> Api.Value -> Integer -> [[(Api.TxOutRef, Api.TxOut)], Api.Value)]
```

This function takes a list of UTXOs coupled with their associated outputs, a
target value to be reached, and an integer representing the maximum number of
elements candidate subsets should contain. The function is recursive and
operates in 2^n time complexity, where n is the number of input UTXOs, as all
"find all subsets" algorithms do. The idea is to go through the input list and
decide to either pick or drop the first element. The function is optimized in
four ways compared to a regular "find all subsets" algorithm:
- At each step, we check whether taking the whole remaining list would be
  sufficient to reach the value. If not, we stop the computation.
- We limit our search to the number of elements specified in the function
  parameters. In particular, the limitation on the number of collateral inputs
  (usually at 3) makes the search much faster.
- We only consider picking the current element if it contributes to reaching the
  target. In other words, if the intersection between the current element value
  and the (positive part of the) target value is empty, we skip it.
- Once the target is reached, we directly stop the search and do not attempt to
  add more elements to the subset.
  
The function returns a list of lists of candidates with their associated surplus
value beyond the target.

The function returns a list of candidate sets along with their associated
surplus values beyond the target. We then sort these candidate sets based on the
minimal ADA required to return the surplus value to the balancing wallet. The
first element in this sorted list represents the optimal candidate set of UTXOs
for balancing the transaction or providing collaterals, meeting all our criteria
for efficiency and sufficiency.

### Computing optimal fee

Computing a reasonable fee for a given transaction skeleton is challenging in
Cardano because, while the fee depends on the transaction itself, it also relies
on aspects that are difficult to assess before submitting the
transaction. However, the fee must be included in the transaction itself, making
this a challenging issue. Users need a reliable way to set a fee that is both
high enough to meet validation requirements and low enough to minimize overall
payment.

Here are the requirements for our fee computation mechanism:
* The fee must be high enough to account for the estimated transaction fee
  computed through the Cardano API, which is guaranteed to be an overestimate of
  the actual transaction fee at validation time.
* The fee must be minimal, meaning any smaller fee would result in a transaction
  that no longer meets the previous requirement.
* The transaction must be balanceable around the chosen fee.
* Collateral must be computable around the chose fee.

Fortunately, fees are bounded within a specific interval that can be deduced
from protocol parameters. Our implementation relies on a dichotomic search
within this interval. The function that performs this computation is
`computeFeeAndBalance`, with the following signature:

``` haskell
(MonadBlockChainBalancing m) => Wallet -> Fee -> Fee -> Collaterals -> [(Api.TxOutRef, Api.TxOut)] -> Wallet -> TxSkel -> m (TxSkel, Fee, Set Api.TxOutRef)
```

This function takes as input the balancing wallet, the two boundaries of the
search interval, the candidate balancing UTXOs, the return collateral wallet,
and the skeleton to adjust. It returns the adjusted skeleton with the computed
fee and the associated set of collateral inputs.

The complexity of this function arises from the fact that coupling a balancing
and a fee computation attempt can fail in three different ways:
- The balancing itself can fail in two ways, either due to a lack of candidate
  inputs or collateral inputs.
- The estimated fee for the generated skeleton can be higher than what the
  skeleton accounts for.

The function is a slightly optimized dichotomic search that unfolds as follows. 

- The input interval should never be empty.
- If it is reduced to a single point, we attempt to balance with the only
possible fee value, which may or may not succeed.
- When the interval is larger, the process becomes more nuanced and interesting,
  as follows.

We attempt to balance the skeleton around the middle of the interval. Based on
the estimated fee resulting from this balancing, we adjust our search interval:
- If the balancing succeeds, but the estimated fee is higher than the current
  applied fee, we search higher up, excluding the current fee attempt.
- If the balancing fails and the remaining smaller left interval is empty, we
  propagate the error, indicating that balancing around the minimum possible fee
  for this transaction is impossible.
- If the balancing fails but the smaller left interval is not empty, we try to
  balance within this smaller interval, hoping to find a candidate set that
  yields a suitable smaller fee.
- If the balancing succeeds with a smaller estimated fee than attempted, and the
  output value of the new skeleton matches the input skeleton (indicating no
  surplus value to return to the balancing wallet), we continue the search
  within the smaller interval.
- If the balancing succeeds with a smaller estimated fee than attempted and
  already includes returning some value to the balancing wallet, we can optimize
  further. Experience shows that with the same UTXO configuration, the estimated
  fee remains constant if we reduce the fee in the transaction and increase the
  amount returned to the balancing wallet. Thus, we can safely search in the
  smaller interval that ends at the estimated fee instead of the currently
  attempted fee.
  
The recursion continues until an error is propagated or the interval is reduced
to a single point.

