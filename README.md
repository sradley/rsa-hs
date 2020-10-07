# RSA in Haskell
An implementation of the RSA cryptosystem in Haskell.

## Benchmarks

### Generating 1024-bit Keys
```
$ time cabal exec rsa-hs
cabal exec rsa-hs  0.18s user 0.05s system 96% cpu 0.247 total
```

### Generating 2048-bit Keys
```
$ time cabal exec rsa-hs
cabal exec rsa-hs  1.80s user 0.33s system 99% cpu 2.136 total
```

### Generating 4096-bit Keys
```
$ time cabal exec rsa-hs
cabal exec rsa-hs  6.12s user 0.29s system 99% cpu 6.416 total
```

## Usage

### Generating Keys
```
-- |Generates an n-bit RSA key.
genKey :: Int -> IO Key
```

### Extracting Public Keys
```
-- |Extracts a public key from a private key.
extKey :: Key -> Key
```

### Encryption
```
-- |Encrypt under RSA given a public or private key.
encrypt :: Key -> Integer -> Integer
```

### Decryption
```
-- |Decrypt under RSA given a private key.
decrypt :: Key -> Integer -> Maybe Integer
```
