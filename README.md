# Infinitable (Rust)

Infinitable is a simple library for introducing the notion of "infinity" and "negative infinity" to numeric types, such as integers, that do not have infinite values.

A representation of infinity is useful for graph algorithms such as Dijkstra's algorithm, as well as for representing a graph with an adjacency matrix.

## Usage

Simply install the `infinitable` crate, available on Crates.io.

```rust
extern crate infinitable;
use infinitable::Infinitable;

fn main() {
	let finite = Infinitable::Finite(5);
	let infinity = Infinitable::Infinity;
	let negative_infinity = Infinitable::NegativeInfinity;

	assert!(finite < infinity);
	assert!(finite > negative_infinity);
}
```

## Tests

Tests are in the `tests` directory. Simply run `cargo test`.

## Note About Floating-Point Numbers

Infinitable does not account for existing infinite values in floating-point numeric types. The infinity provided by Infinitable compares greater than *all* existing values, and the negative infinity provided by Infinitable compares less than *all* existing values.

To illustrate, here are some values listed from least to greatest:

* Infinitable `NegativeInfinity`
* Floating-point negative infinity
* Floating-point zero
* Floating-point infinity
* Infinitable `Infinity`

## License

Infinitable is available under the 2-clause BSD license. Refer to `LICENSE.txt` for details.