//! Infinity for types without infinite values
//!
//! Infinitable introduces the notion of "infinity" and "negative infinity"
//! to numeric types, such as integers, that do not have infinite values.
//!
//! A representation of infinity is useful for graph algorithms such as
//! Dijkstra's algorithm, as well as for representing a graph with an
//! adjacency matrix.
//!
//! # Basic Usage
//!
//! ```
//! use infinitable::*;
//!
//! let finite = Finite(5);
//! let infinity = Infinity;
//! let negative_infinity = NegativeInfinity;
//!
//! assert!(finite < infinity);
//! assert!(finite > negative_infinity);
//! ```

#![cfg_attr(not(test), no_std)]

#[cfg(test)]
extern crate core;

use core::cmp::Ordering;
use core::fmt;
use core::fmt::{Display, Formatter};
use core::ops::Neg;

/// An "infinitable" value, one that can be either finite or infinite
///
/// # Versioning
///
/// Available since 1.0.0. Variants are re-exported since 1.3.0.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Infinitable<T> {
    /// A finite value `T`
    Finite(T),
    /// Positive infinity, which compares greater than all other values
    Infinity,
    /// Negative infinity, which compares less than all other values
    NegativeInfinity,
}

pub use Infinitable::{Finite, Infinity, NegativeInfinity};

impl<T> Infinitable<T> {
    /// Returns `true` if the value is `Finite`.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// let finite = Finite(5);
    /// assert!(finite.is_finite());
    /// let infinite: Infinitable<i32> = Infinity;
    /// assert!(!infinite.is_finite());
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.0.0.
    pub fn is_finite(&self) -> bool {
        match self {
            Finite(_) => true,
            _ => false,
        }
    }

    /// Converts from an `Infinitable<T>` to an `Option<T>`.
    ///
    /// Converts `self` into an `Option<T>` possibly containing a finite value,
    /// consuming `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// let finite = Finite(5);
    /// assert_eq!(Some(5), finite.finite());
    /// let infinite: Infinitable<i32> = Infinity;
    /// assert_eq!(None, infinite.finite());
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.1.0.
    pub fn finite(self) -> Option<T> {
        match self {
            Finite(x) => Some(x),
            _ => None,
        }
    }

    /// Converts from an `Option<T>` to either `Finite` or `Infinity`.
    ///
    /// Converts an `Option<T>` to an `Infinitable<T>`. `Some(T)` is converted
    /// to `Finite(T)`, and `None` is converted to `Infinity`.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// let finite = Finite(5);
    /// assert_eq!(finite, Infinitable::finite_or_infinity(Some(5)));
    /// let infinite: Infinitable<i32> = Infinity;
    /// assert_eq!(infinite, Infinitable::finite_or_infinity(None));
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.3.0.
    pub fn finite_or_infinity(option: Option<T>) -> Infinitable<T> {
        match option {
            Some(x) => Finite(x),
            None => Infinity,
        }
    }

    /// Converts from an `Option<T>` to either `Finite` or `NegativeInfinity`.
    ///
    /// Converts an `Option<T>` to an `Infinitable<T>`. `Some(T)` is converted
    /// to `Finite(T)`, and `None` is converted to `NegativeInfinity`.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// let finite = Finite(5);
    /// assert_eq!(finite, Infinitable::finite_or_negative_infinity(Some(5)));
    /// let infinite: Infinitable<i32> = NegativeInfinity;
    /// assert_eq!(infinite, Infinitable::finite_or_negative_infinity(None));
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.3.0.
    pub fn finite_or_negative_infinity(option: Option<T>) -> Infinitable<T> {
        match option {
            Some(x) => Finite(x),
            None => NegativeInfinity,
        }
    }
}

impl<T> From<T> for Infinitable<T> {
    /// Converts from a value `T` to `Finite` containing the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// let finite = Infinitable::from(5);
    /// assert_eq!(Finite(5), finite);
    ///
    /// // Warning: There is no special handling for pre-existing infinite values
    /// let fp_infinity = Infinitable::from(f32::INFINITY);
    /// assert_eq!(Finite(f32::INFINITY), fp_infinity);
    /// assert_ne!(Infinity, fp_infinity);
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.2.0.
    fn from(value: T) -> Infinitable<T> {
        Finite(value)
    }
}

/// Partial order, where the underlying type `T` implements a partial order.
///
/// `NegativeInfinity` compares less than all other values,
/// and `Infinity` compares greater than all other values.
///
/// # Examples
///
/// ```
/// use infinitable::*;
/// use std::cmp::Ordering;
///
/// let finite = Finite(5);
/// let infinity = Infinity;
/// let negative_infinity = NegativeInfinity;
///
/// assert_eq!(Some(Ordering::Less), finite.partial_cmp(&infinity));
/// assert_eq!(Some(Ordering::Greater), finite.partial_cmp(&negative_infinity));
/// ```
///
/// # Versioning
///
/// Available since 1.0.0.
impl<T> PartialOrd for Infinitable<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match cmp_initial(self, other) {
            CmpInitialResult::Infinite(o) => Some(o),
            CmpInitialResult::Finite(x, y) => x.partial_cmp(y),
        }
    }
}

/// Total order, where the underlying type `T` implements a total order.
///
/// `NegativeInfinity` compares less than all other values,
/// and `Infinity` compares greater than all other values.
///
/// # Examples
///
/// ```
/// use infinitable::*;
/// use std::cmp::Ordering;
///
/// let finite = Finite(5);
/// let infinity = Infinity;
/// let negative_infinity = NegativeInfinity;
///
/// assert_eq!(Ordering::Less, finite.cmp(&infinity));
/// assert_eq!(Ordering::Greater, finite.cmp(&negative_infinity));
/// ```
///
/// # Versioning
///
/// Available since 1.0.0.
impl<T> Ord for Infinitable<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match cmp_initial(self, other) {
            CmpInitialResult::Infinite(o) => o,
            CmpInitialResult::Finite(x, y) => x.cmp(y),
        }
    }
}

enum CmpInitialResult<'a, T> {
    Infinite(Ordering),
    Finite(&'a T, &'a T),
}

fn cmp_initial<'a, T>(x: &'a Infinitable<T>, y: &'a Infinitable<T>) -> CmpInitialResult<'a, T> {
    match (x, y) {
        (Infinity, Infinity) | (NegativeInfinity, NegativeInfinity) => {
            CmpInitialResult::Infinite(Ordering::Equal)
        }
        (Infinity, _) | (_, NegativeInfinity) => CmpInitialResult::Infinite(Ordering::Greater),
        (NegativeInfinity, _) | (_, Infinity) => CmpInitialResult::Infinite(Ordering::Less),
        (Finite(xf), Finite(yf)) => CmpInitialResult::Finite(xf, yf),
    }
}

impl<T> Neg for Infinitable<T>
where
    T: Neg,
{
    type Output = Infinitable<T::Output>;

    /// Negates the value, when the underlying type `T` supports negation.
    ///
    /// `Infinity` is negated to `NegativeInfinity` (and vice versa),
    /// and `Finite` is negated based on the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// let finite = Finite(5);
    /// assert_eq!(Finite(-5), -finite);
    /// let infinity: Infinitable<i32> = Infinity;
    /// assert_eq!(NegativeInfinity, -infinity);
    /// let negative_infinity: Infinitable<i32> = NegativeInfinity;
    /// assert_eq!(Infinity, -negative_infinity);
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.3.0.
    fn neg(self) -> Infinitable<T::Output> {
        match self {
            Finite(x) => Finite(-x),
            Infinity => NegativeInfinity,
            NegativeInfinity => Infinity,
        }
    }
}

impl<T> Display for Infinitable<T>
where
    T: Display,
{
    /// Formats the value, where the underlying type `T` supports formatting.
    ///
    /// `Infinity` is formatted to `"inf"`, `NegativeInfinity` is formatted to
    /// `"-inf"`, and `Finite` is formatted based on the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// let finite = Finite(5);
    /// assert_eq!("5", format!("{}", finite));
    /// let infinity: Infinitable<i32> = Infinity;
    /// assert_eq!("inf", format!("{}", infinity));
    /// let negative_infinity: Infinitable<i32> = NegativeInfinity;
    /// assert_eq!("-inf", format!("{}", negative_infinity));
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.2.0.
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Finite(x) => write!(f, "{}", x),
            Infinity => write!(f, "inf"),
            NegativeInfinity => write!(f, "-inf"),
        }
    }
}
