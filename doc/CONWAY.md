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
number of those in transaction skeleton. The balancing mechanism will take into
account the required deposit for each of these procedures.

### Parameter changes

It is possible to issue proposal procedures with a request for changes in
parameters. If a script witness is attached to this proposal (typically the
constitution script), it will be ran against the proposal. All kinds of
parameter changes are supported, except for the cost models, which contain too
many values and are not even yet taken into account by the current constitution.

### Treasury withdrawals

It is possible to issue proposal procedures with a request for treasury
withdrawals. If a script witness is attached to this proposal (typically the
constitution script), it will be ran against the proposal.
