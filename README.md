# extensible-stream-fmt
Library to assist with writing compact data streams from Haskell values using the extensible package https://hackage.haskell.org/package/extensible.

Features:
  - Serialization of extensible Record and Variant types using the Cereal package
  - Automatic generation of schemas to describe common primitive types and arbitrary Records and Variants

Intended use case is saving large data streams to file and saving a schema of the data type alongside the compressed raw binary. The generated schemas are describe types in valid Haskell code, and can be used for type-checking before deserializing a binary file.
