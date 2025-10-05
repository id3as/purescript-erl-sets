# purescript-erl-sets

A PureScript library for Erlang sets, using an FFI to Erlang's `sets` module for efficiency.

## Installation

Add `erl-sets` to your `spago.yaml` file and `spago install`.

```yaml
dependencies:
  - erl-sets
```

## Usage

```purescript
import Prelude
import Erl.Data.Set as Set

mySet :: Set.Set Int
mySet = Set.fromFoldable [1, 2, 3, 3, 4]

-- Set.member 3 mySet == true
-- Set.size mySet == 4
```

## API Documentation

The module `Erl.Data.Set` provides an API that mirrors the standard `Data.Set` module from `purescript-ordered-collections`.

This implementation uses a Foreign Function Interface (FFI) to Erlang's `sets` module for performance. The module uses Erlang's term equality as opposed to the `Eq` class.

## Development

To build the project and run the tests, use the following commands:

```bash
spago build
spago test
```
