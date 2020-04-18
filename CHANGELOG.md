# Changelog

## 1.3.0

* The variants of `Infinitable` are now re-exported, so you can type `Finite(x)` instead of `Infinitable::Finite(x)`.
* The `finite_or_infinity()` and `finite_or_negative_infinity()` methods are added to convert from an optional finite value to an infinitable.
* The `Infinitable` type now implements the `Hash` trait.

## 1.2.0

* Negation operator is now supported.
* The `Infinitable` type now implements the `From` trait, as well as the `Display`, `Copy`, and `Clone` traits as appropriate (that is, when the underlying type implements them).

## 1.1.0

* The `finite()` method is added to extract an optional finite value.
* The `Infinitable` type now implements the `Debug` trait.
* The public API is fully documented.

## 1.0.1

* The URL in the license is fixed (previously it pointed to the original C++ implementation of the library).

## 1.0.0

* Initial version supports equality and comparison operators.