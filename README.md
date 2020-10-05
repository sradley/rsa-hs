# RSA in Haskell

## Generating Keys
```
-- |Generates an n-bit RSA key.
generate :: Int -> IO Key
```

## Extracting Public Keys
```
-- |Extracts a public key from a private key.
extract :: Key -> Key
```

## Encryption
```
-- |Encrypt under RSA given a public or private key.
encrypt :: Key -> Integer -> Integer
```

## Decryption
```
-- |Decrypt under RSA given a private key.
decrypt :: Key -> Integer -> Maybe Integer
```
