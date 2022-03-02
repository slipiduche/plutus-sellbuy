### plutus-sellbuy

This directory contains a simple "Hello World" script using an integer literal (needed because the Plutus interpreter doesn't currently accept byte string literals) and a smart contract for nft marketplace

``plutus-sellbuy`` -- very simple numeric version
``plutus-sellbuycontract`` -- nft marketplace contract

to compile serialezed script:

cabal run plutus-sellbuycontract -- 42 sellbuycontract.plutus
