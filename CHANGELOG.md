# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# v2.2.0

## Added

- `utxosAtWithPageLimit` query, which allows setting an upper limit on the number of pages requested ([#4](https://github.com/mlabs-haskell/purescript-cardano-blockfrost-provider/pull/4))

# v2.1.0

## Changed

- Context: Blockfrost now allows to specify the version of Ogmios to use when forwarding transaction evaluation
  requests. ([#3](https://github.com/mlabs-haskell/purescript-cardano-blockfrost-provider/pull/3))
  - Always specify Ogmios v6 for `evaluateTx`.
  - If no additional utxos are provided, use the default evaluation endpoint. This change is motivated by the
    observation that only the default endpoint (no additional utxo set support) with Ogmios v6 correctly returns script
    evaluation traces on failure.
  - Reuse Ogmios utilities from [purescript-cardano-provider](https://github.com/mlabs-haskell/purescript-cardano-provider)
    to decode tx evaluation responses. Remove the obsolete Ogmios v5.6 evaluation response decoding and conversion
    logic.

# v2.0.0

## Changed

- Replaced cardano-serialization-lib (CSL) with [cardano-data-lite (CDL)](https://github.com/mlabs-haskell/purescript-cardano-data-lite) ([#2](https://github.com/mlabs-haskell/purescript-cardano-blockfrost-provider/pull/2))
