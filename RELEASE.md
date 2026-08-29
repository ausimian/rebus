# Release Notes

## Unreleased

### Fixed

- Correct declared byte lengths for arrays requiring position-dependent padding,
  including variants and nested arrays of 8-byte-aligned values (`av`, `aav`,
  `aax`, and `aad`), which could otherwise produce malformed messages and make
  self-decoding fail.
