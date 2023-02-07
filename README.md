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

The infinite values provided by Infinitable are different from the existing infinite values in floating-point numeric types. Therefore, simply using `Finite` or `Infinitable::from` to convert from a floating-point value to an `Infinitable` may lead to unintuitive results:

```
extern crate infinitable;
use infinitable::Infinitable;

fn main() {
	let infinitable_infinity = Infinitable::Infinity;
	let fp_infinity = Infinitable::Finite(f64::INFINITY);

	assert!(infinitable_infinity > fp_infinity);
}
```

The library provides the `from_f32` and `from_f64` to convert from floating-point values while taking into account infinite values and NaN:

```
extern crate infinitable;
use infinitable::Infinitable;

fn main() {
	let infinitable_infinity = Infinitable::Infinity;
	let fp_infinity = infinitable::from_f64(f64::INFINITY).unwrap();

	assert!(infinitable_infinity == fp_infinity);
}
```

## License

Infinitable is available under the 2-clause BSD license. Refer to `LICENSE.txt` for details.