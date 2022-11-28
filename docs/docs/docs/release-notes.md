---
layout: docs
title: Release notes
---

# Release notes

## 0.3.0
- Dependency updates (ZIO 2.0.4, Cats Effect 3.4.1, etc.)
- ZIO Schema based derivation
- Scala 3 support
- Shardcake module
- Many new built-in codecs
- Redesigned package structure
- Lower level codec API in place of the ZPure one for higher performance 

## 0.2.3
- Dependency updates, support for ZIO 2.0.0-RC6

## 0.2.1, 0.2.2 
- Dependency updates

## 0.2.0
- Core migrated to `ZPure`
- Codecs for cats data types moved to `desert-cats` module

## 0.1.5
- Performance improvements

## 0.1.4
- Fixed the akka serialization base class

## 0.1.3
- Fixed stack safety

## 0.1.2
- Disabled automatic string deduplication
- ZIO 1.0.0

## 0.1.1
- Support for making fields and constructors *transient*
- Ability to pass _offset_ and _length_ to `writeBytes` in custom serializers
- General purpose serialization failure type (`SerializationFailure`) for custom serializers
- Ability to read/write byte arrays using ZIP compression
- `UUID` codec
- Helper functions to lift `Try[T]` into `Ser[T]` and `Deser[T]` for custom serializers

## 0.1.0
The initial release of `desert`