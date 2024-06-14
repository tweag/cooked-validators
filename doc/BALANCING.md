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

It takes a skeleton as input living into a `MonadBlockChainBalancing` environement, and returns:
- A balanced skeleton
- The associated fee accounted for in the skeleton
- The associated set of collateral inputs to account for the chosen fee
- The return collateral wallet to return excess collateral to

## Options piloting balancing

### Balancing policy -- Whether to balance or not, and with which wallet.

``` haskell
data BalancingPolicy
  = BalanceWithFirstSigner -- default
  | BalanceWith Wallet
  | DoNotBalance
```

### Fee policy -- Whether to use manual fee or assess a suitable fee.

``` haskell
data FeePolicy
  = AutoFeeComputation -- default
  | ManualFee Integer
```

### Balance output policy -- Whether to add up return value to existing output, or create a new one.

``` haskell
data BalanceOutputPolicy
  = AdjustExistingOutput -- default
  | DontAdjustExistingOutput
```

### Balancing utxos -- Which utxos to pick from to account for the missing value in the inputs of the skeleton.

``` haskell
data BalancingUtxos
  = BalancingUtxosAutomatic -- default
    BalancingUtxosWith (Set Api.TxOutRef)
```

### Collateral utxos -- Which utxos to pick from as collateral inputs.

``` haskell
data CollateralUtxos
  = CollateralUtxosFromBalancingWallet -- default
  | CollateralUtxosFromWallet Wallet
  | CollateralUtxosFromSet (Set Api.TxOutRef) Wallet
```
