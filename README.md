# RSA in Haskell
An implementation of the RSA cryptosystem in Haskell.

## Benchmarks

### Generating 1024-bit Keys
```
$ time cabal exec rsa-hs
cabal exec rsa-hs  0.39s user 0.10s system 99% cpu 0.496 total
```

### Generating 2048-bit Keys
```
$ time cabal exec rsa-hs
cabal exec rsa-hs  2.06s user 0.13s system 99% cpu 2.193 total
```

### Generating 4096-bit Keys
```
$ time cabal exec rsa-hs
cabal exec rsa-hs  47.23s user 0.48s system 99% cpu 47.786 total
```

## Usage

### Generating Keys
```
-- |Generates an n-bit RSA key.
generate :: Int -> IO Key
```

### Extracting Public Keys
```
-- |Extracts a public key from a private key.
extract :: Key -> Key
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
