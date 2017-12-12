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
//! use infinitable::Infinitable;
//!
//! let finite = Infinitable::Finite(5);
//! let infinity = Infinitable::Infinity;
//! let negative_infinity = Infinitable::NegativeInfinity;
//!
//! assert!(finite < infinity);
//! assert!(finite > negative_infinity);
//! ```

use std::cmp::Ordering;
use std::ops::Neg;
use std::fmt;
use std::fmt::{Display,Formatter};

/// An "infinitable" value, one that can be either finite or infinite
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Infinitable<T> {
	/// A finite value `T`
	Finite(T),
	/// Positive infinity, which compares greater than all finite values
	Infinity,
	/// Negative infinity, which compares less than all finite values
	NegativeInfinity,
}

impl<T> Infinitable<T> {
	/// Returns `true` if the value is `Finite`.
	///
	/// # Examples
	///
	/// ```
	/// use infinitable::Infinitable;
	///
	/// let finite = Infinitable::Finite(5);
	/// assert!(finite.is_finite());
	/// ```
	pub fn is_finite(&self) -> bool {
		match self {
			&Infinitable::Finite(_) => true,
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
	/// use infinitable::Infinitable;
	///
	/// let finite = Infinitable::Finite(5);
	/// assert_eq!(Some(5), finite.finite());
	/// let infinite: Infinitable<i32> = Infinitable::Infinity;
	/// assert_eq!(None, infinite.finite());
	/// ```
	pub fn finite(self) -> Option<T> {
		match self {
			Infinitable::Finite(x) => Some(x),
			_ => None,
		}
	}
}

impl<T> From<T> for Infinitable<T> {
	fn from(value: T) -> Infinitable<T> {
		Infinitable::Finite(value)
	}
}

impl<T> PartialOrd for Infinitable<T> where T: PartialOrd {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		match (self, other) {
			(&Infinitable::Infinity, &Infinitable::Infinity)
			| (&Infinitable::NegativeInfinity, &Infinitable::NegativeInfinity)
				=> Some(Ordering::Equal),
			(&Infinitable::Infinity, _)
			| (_, &Infinitable::NegativeInfinity)
				=> Some(Ordering::Greater),
			(&Infinitable::NegativeInfinity, _)
			| (_, &Infinitable::Infinity)
				=> Some(Ordering::Less),
			(&Infinitable::Finite(ref x), &Infinitable::Finite(ref y))
				=> x.partial_cmp(y),
		}
	}
}

impl<T> Ord for Infinitable<T> where T: Ord {
	fn cmp(&self, other: &Self) -> Ordering {
		match (self, other) {
			(&Infinitable::Finite(ref x), &Infinitable::Finite(ref y))
				=> x.cmp(y),
			(..)
				=> self.partial_cmp(other).unwrap(),
		}
	}
}

impl<T> Neg for Infinitable<T> where T: Neg {
	type Output = Infinitable<T::Output>;

	fn neg(self) -> Infinitable<T::Output> {
		match self {
			Infinitable::Finite(x) => Infinitable::Finite(-x),
			Infinitable::Infinity => Infinitable::NegativeInfinity,
			Infinitable::NegativeInfinity => Infinitable::Infinity,
		}
	}
}

impl<T> Display for Infinitable<T> where T: Display {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			&Infinitable::Finite(ref x) => write!(f, "{}", x),
			&Infinitable::Infinity => write!(f, "inf"),
			&Infinitable::NegativeInfinity => write!(f, "-inf"),
		}
	}
}
