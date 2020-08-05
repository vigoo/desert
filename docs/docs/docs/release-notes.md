---
layout: docs
title: Release notes
permalink: docs/release-notes/
---

# Release notes

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