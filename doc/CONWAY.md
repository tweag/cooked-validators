# Supported Conway features

With the arrival of the Conway era, the Cardano blockchain is being enriched
with a significant set of features revolving around governance. Governance can
be defined as the ability for ada holders to take part in various on-chain
decisions as described in
[CIP-1694](https://github.com/cardano-foundation/CIPs/tree/master/CIP-1694). These
new features have brought a substantial set of changes, such as new script
purposes, new centralized data like committee, and new transaction
features. This documents describes which of those features are currently being
supported by cooked-validators, and to which extent. Each of the following items
describes a feature that is currently supported. The reader can assume that
everything that is not directly mentioned here about Conway is not yet
supported.

## Proposal procedures

It is currently possible to describe proposal procedures and attach an arbitrary
number of those in transaction skeletons. The balancing mechanism will take into
account the required deposit for each of these procedures. If those proposal
procedure involve scripts (i.e. they proposal a withdrawal or a parameter
change) those script will be ran during the validation process. However, the
proposal will not be enacted.

## Multipurpose scripts

Multipurpose scripts, from
[CIP-0069](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0069) are
fully supported by cooked-validators.

## Withdrawals

It is currently possible to register a staking credential with a certain deposit
and reward (either a script or a wallet) and to later on withdraw this
reward. If the reward is associated with a script, this script will be ran
during the validation process.
