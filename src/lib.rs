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

use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::Neg;

/// An "infinitable" value, one that can be either finite or infinite
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Infinitable<T> {
    /// A finite value `T`
    Finite(T),
    /// Positive infinity, which compares greater than all finite values
    Infinity,
    /// Negative infinity, which compares less than all finite values
    NegativeInfinity,
}

pub use Infinitable::{Finite, Infinity, NegativeInfinity};

impl<T> Infinitable<T> {
    /// Returns `true` if the value is `Finite`.
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::Finite;
    ///
    /// let finite = Finite(5);
    /// assert!(finite.is_finite());
    /// ```
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
    pub fn finite_or_negative_infinity(option: Option<T>) -> Infinitable<T> {
        match option {
            Some(x) => Finite(x),
            None => NegativeInfinity,
        }
    }
}

impl<T> From<T> for Infinitable<T> {
    fn from(value: T) -> Infinitable<T> {
        Finite(value)
    }
}

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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Finite(x) => write!(f, "{}", x),
            Infinity => write!(f, "inf"),
            NegativeInfinity => write!(f, "-inf"),
        }
    }
}
