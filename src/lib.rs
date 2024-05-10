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

use core::cmp::Ordering;
use core::fmt;
use core::fmt::{Display, Formatter};
use core::ops::{Add, Div, Mul, Neg, Sub};
use num_traits::Zero;

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
    /// Returns `true` if the value is [`Finite`].
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
    #[must_use]
    pub fn is_finite(&self) -> bool {
        match self {
            Finite(_) => true,
            _ => false,
        }
    }

    /// Converts from an `Infinitable<T>` to an [`Option<T>`].
    ///
    /// Converts `self` into an [`Option<T>`] possibly containing
    /// a finite value, consuming `self`.
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
    #[must_use]
    pub fn finite(self) -> Option<T> {
        match self {
            Finite(x) => Some(x),
            _ => None,
        }
    }

    /// Converts from [`Option<T>`] to [`Finite`] or [`Infinity`].
    ///
    /// <code>[Some]\(T)</code> is converted to <code>[Finite]\(T)</code>,
    /// and [`None`] is converted to [`Infinity`].
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
    #[must_use]
    pub fn finite_or_infinity(option: Option<T>) -> Infinitable<T> {
        match option {
            Some(x) => Finite(x),
            None => Infinity,
        }
    }

    /// Converts from [`Option<T>`] to [`Finite`] or [`NegativeInfinity`].
    ///
    /// <code>[Some]\(T)</code> is converted to <code>[Finite]\(T)</code>,
    /// and [`None`] is converted to [`NegativeInfinity`].
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
    #[must_use]
    pub fn finite_or_negative_infinity(option: Option<T>) -> Infinitable<T> {
        match option {
            Some(x) => Finite(x),
            None => NegativeInfinity,
        }
    }

    /// Converts the value into a different type, based on the conversion of the
    /// underlying type `T`.
    ///
    /// Infinite values are preserved as is, while finite values are converted
    /// using [`into`] on the underlying value.
    ///
    /// [`into`]: Into::into
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    /// let finite = Finite(5);
    /// let finite64: Infinitable<i64> = finite.convert_into();
    /// assert_eq!(Finite(5i64), finite64);
    /// let infinite: Infinitable<i32> = Infinity;
    /// let infinite64: Infinitable<i64> = infinite.convert_into();
    /// assert_eq!(Infinity, infinite64);
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.6.0.
    #[must_use]
    pub fn convert_into<U>(self) -> Infinitable<U>
    where
        T: Into<U>,
    {
        match self {
            Finite(x) => Finite(x.into()),
            Infinity => Infinity,
            NegativeInfinity => NegativeInfinity,
        }
    }

    /// Converts the value into a different type, based on the conversion of the
    /// underlying type `T`.
    ///
    /// Infinite values are preserved as is, while finite values are converted
    /// using [`try_into`] on the underlying value. Conversion of infinite
    /// values always succeeds, while conversion of finite values may fail.
    ///
    /// [`try_into`]: TryInto::try_into
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    /// use std::num::TryFromIntError;
    /// let finite = Finite(1000);
    /// let finite8: Result<Infinitable<i8>, TryFromIntError>
    ///     = finite.try_convert_into();
    /// assert!(finite8.is_err());
    /// let infinite: Infinitable<i32> = Infinity;
    /// let infinite8: Result<Infinitable<i8>, TryFromIntError>
    ///     = infinite.try_convert_into();
    /// assert_eq!(Ok(Infinity), infinite8);
    /// ```
    ///
    /// # Versioning
    ///
    /// Available since 1.6.0.
    #[must_use]
    pub fn try_convert_into<U>(self) -> Result<Infinitable<U>, T::Error>
    where
        T: TryInto<U>,
    {
        match self {
            Finite(x) => x.try_into().map(|y| Finite(y)),
            Infinity => Ok(Infinity),
            NegativeInfinity => Ok(NegativeInfinity),
        }
    }
}

impl<T> From<T> for Infinitable<T> {
    /// Converts from a value `T` to [`Finite`] containing the underlying value.
    ///
    /// Note that there is no special handling for pre-existing infinite values.
    /// Consider using [`from_f32`] or [`from_f64`] for floating-point numbers.
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

/// Partial order, when the underlying type `T` implements a partial order.
///
/// [`NegativeInfinity`] compares less than all other values,
/// and [`Infinity`] compares greater than all other values.
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

/// Total order, when the underlying type `T` implements a total order.
///
/// [`NegativeInfinity`] compares less than all other values,
/// and [`Infinity`] compares greater than all other values.
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

impl<T> Add for Infinitable<T>
where
    T: Add,
{
    type Output = Infinitable<T::Output>;

    /// Adds two values.
    ///
    /// The addition operation follows these rules:
    ///
    /// | self               | rhs                | result                |
    /// |--------------------|--------------------|-----------------------|
    /// | `Finite`           | `Finite`           | `Finite` (add values) |
    /// | `Finite`           | `Infinity`         | `Infinity`            |
    /// | `Finite`           | `NegativeInfinity` | `NegativeInfinity`    |
    /// | `Infinity`         | `Finite`           | `Infinity`            |
    /// | `Infinity`         | `Infinity`         | `Infinity`            |
    /// | `Infinity`         | `NegativeInfinity` | Undefined (panic)     |
    /// | `NegativeInfinity` | `Finite`           | `NegativeInfinity`    |
    /// | `NegativeInfinity` | `Infinity`         | Undefined (panic)     |
    /// | `NegativeInfinity` | `NegativeInfinity` | `NegativeInfinity`    |
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// assert_eq!(Finite(5), Finite(2) + Finite(3));
    /// assert_eq!(Infinity, Finite(1) + Infinity);
    /// assert_eq!(NegativeInfinity, NegativeInfinity + Finite(1));
    /// ```
    ///
    /// The addition operation panics with `Infinity` and `NegativeInfinity`:
    ///
    /// ```should_panic
    /// use infinitable::*;
    ///
    /// let infinity: Infinitable<i32> = Infinity;
    /// let negative_infinity: Infinitable<i32> = NegativeInfinity;
    /// let _ = infinity + negative_infinity;
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if the operands consist of `Infinity` and `NegativeInfinity`.
    ///
    /// # Versioning
    ///
    /// Available since 1.5.0.
    fn add(self, rhs: Infinitable<T>) -> Infinitable<T::Output> {
        match (self, rhs) {
            (Infinity, NegativeInfinity) | (NegativeInfinity, Infinity) => {
                panic!("Cannot add infinity and negative infinity")
            }
            (Finite(lf), Finite(rf)) => Finite(lf.add(rf)),
            (Infinity, _) | (_, Infinity) => Infinity,
            (NegativeInfinity, _) | (_, NegativeInfinity) => NegativeInfinity,
        }
    }
}

impl<T> Sub for Infinitable<T>
where
    T: Sub,
{
    type Output = Infinitable<T::Output>;

    /// Subtracts two values.
    ///
    /// The subtraction operation follows these rules:
    ///
    /// | self               | rhs                | result                     |
    /// |--------------------|--------------------|----------------------------|
    /// | `Finite`           | `Finite`           | `Finite` (subtract values) |
    /// | `Finite`           | `Infinity`         | `NegativeInfinity`         |
    /// | `Finite`           | `NegativeInfinity` | `Infinity`                 |
    /// | `Infinity`         | `Finite`           | `Infinity`                 |
    /// | `Infinity`         | `Infinity`         | Undefined (panic)          |
    /// | `Infinity`         | `NegativeInfinity` | `Infinity`                 |
    /// | `NegativeInfinity` | `Finite`           | `NegativeInfinity`         |
    /// | `NegativeInfinity` | `Infinity`         | `NegativeInfinity`         |
    /// | `NegativeInfinity` | `NegativeInfinity` | Undefined (panic)          |
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// assert_eq!(Finite(3), Finite(5) - Finite(2));
    /// assert_eq!(Infinity, Infinity - Finite(1));
    /// assert_eq!(Infinity, Finite(1) - NegativeInfinity);
    /// assert_eq!(NegativeInfinity, NegativeInfinity - Finite(1));
    /// assert_eq!(NegativeInfinity, Finite(1) - Infinity);
    /// ```
    ///
    /// The subraction operation panics when an infinite value is subtracted
    /// from itself:
    ///
    /// ```should_panic
    /// use infinitable::*;
    ///
    /// let infinity: Infinitable<i32> = Infinity;
    /// let _ = infinity - infinity;
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if the operands are both `Infinity` or both `NegativeInfinity`.
    ///
    /// # Versioning
    ///
    /// Available since 1.5.0.
    fn sub(self, rhs: Infinitable<T>) -> Infinitable<T::Output> {
        match (self, rhs) {
            (Infinity, Infinity) | (NegativeInfinity, NegativeInfinity) => {
                panic!("Cannot subtract infinite value from itself")
            }
            (Finite(lf), Finite(rf)) => Finite(lf.sub(rf)),
            (Infinity, _) | (_, NegativeInfinity) => Infinity,
            (NegativeInfinity, _) | (_, Infinity) => NegativeInfinity,
        }
    }
}

impl<T> Mul for Infinitable<T>
where
    T: Mul + Zero + PartialOrd,
{
    type Output = Infinitable<<T as Mul>::Output>;

    /// Multiplies two values.
    ///
    /// The multiplication operation follows these rules:
    ///
    /// | self               | rhs                | result                     |
    /// |--------------------|--------------------|----------------------------|
    /// | `Finite`           | `Finite`           | `Finite` (multiply values) |
    /// | `Finite` (> 0)     | `Infinity`         | `Infinity`                 |
    /// | `Finite` (~ 0)     | `Infinity`         | Undefined (panic)          |
    /// | `Finite` (< 0)     | `Infinity`         | `NegativeInfinity`         |
    /// | `Finite` (> 0)     | `NegativeInfinity` | `NegativeInfinity`         |
    /// | `Finite` (~ 0)     | `NegativeInfinity` | Undefined (panic)          |
    /// | `Finite` (< 0)     | `NegativeInfinity` | `Infinity`                 |
    /// | `Infinity`         | `Finite` (> 0)     | `Infinity`                 |
    /// | `Infinity`         | `Finite` (~ 0)     | Undefined (panic)          |
    /// | `Infinity`         | `Finite` (< 0)     | `NegativeInfinity`         |
    /// | `Infinity`         | `Infinity`         | `Infinity`                 |
    /// | `Infinity`         | `NegativeInfinity` | `NegativeInfinity`         |
    /// | `NegativeInfinity` | `Finite` (> 0)     | `NegativeInfinity`         |
    /// | `NegativeInfinity` | `Finite` (~ 0)     | Undefined (panic)          |
    /// | `NegativeInfinity` | `Finite` (< 0)     | `Infinity`                 |
    /// | `NegativeInfinity` | `Infinity`         | `NegativeInfinity`         |
    /// | `NegativeInfinity` | `NegativeInfinity` | `Infinity`                 |
    ///
    /// (In the table, "~ 0" refers to a value that is either equal to or
    /// unordered with zero.)
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// assert_eq!(Finite(6), Finite(2) * Finite(3));
    /// assert_eq!(Infinity, Infinity * Finite(2));
    /// assert_eq!(Infinity, Finite(-1) * NegativeInfinity);
    /// assert_eq!(NegativeInfinity, NegativeInfinity * Finite(2));
    /// assert_eq!(NegativeInfinity, Finite(-1) * Infinity);
    /// ```
    ///
    /// The multiplication operation panics when an infinite value is multiplied
    /// with zero:
    ///
    /// ```should_panic
    /// use infinitable::*;
    ///
    /// let infinity: Infinitable<i32> = Infinity;
    /// let _ = infinity * Finite(0);
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if one of the operands is `Infinity` or `NegativeInfinity` and
    /// the other is a `Finite` value with an underlying value equal to or
    /// unordered with zero.
    ///
    /// # Versioning
    ///
    /// Available since 1.5.0.
    fn mul(self, rhs: Infinitable<T>) -> Infinitable<<T as Mul>::Output> {
        match (self, rhs) {
            (Infinity, Infinity) | (NegativeInfinity, NegativeInfinity) => Infinity,
            (Infinity, NegativeInfinity) | (NegativeInfinity, Infinity) => NegativeInfinity,
            (Infinity, Finite(x)) | (Finite(x), Infinity) => match x.partial_cmp(&T::zero()) {
                Some(Ordering::Greater) => Infinity,
                Some(Ordering::Less) => NegativeInfinity,
                _ => panic!("Cannot multiply infinite value and zero or unordered value"),
            },
            (NegativeInfinity, Finite(x)) | (Finite(x), NegativeInfinity) => {
                match x.partial_cmp(&T::zero()) {
                    Some(Ordering::Greater) => NegativeInfinity,
                    Some(Ordering::Less) => Infinity,
                    _ => panic!("Cannot multiply infinite value and zero or unordered value"),
                }
            }
            (Finite(lf), Finite(rf)) => Finite(lf.mul(rf)),
        }
    }
}

impl<T> Div for Infinitable<T>
where
    T: Div + Zero + PartialOrd,
    <T as Div>::Output: Zero,
{
    type Output = Infinitable<<T as Div>::Output>;

    /// Divides two values.
    ///
    /// The division operation follows these rules:
    ///
    /// | self               | rhs                | result                     |
    /// |--------------------|--------------------|----------------------------|
    /// | `Finite` (> 0)     | `Finite` (~ 0)     | `Infinity`                 |
    /// | `Finite` (~ 0)     | `Finite` (~ 0)     | Undefined (panic)          |
    /// | `Finite` (< 0)     | `Finite` (~ 0)     | `NegativeInfinity`         |
    /// | `Finite`           | `Finite` (<> 0)    | `Finite` (divide values)   |
    /// | `Finite`           | `Infinity`         | Zero                       |
    /// | `Finite`           | `NegativeInfinity` | Zero                       |
    /// | `Infinity`         | `Finite` (> 0)     | `Infinity`                 |
    /// | `Infinity`         | `Finite` (~ 0)     | `Infinity`                 |
    /// | `Infinity`         | `Finite` (< 0)     | `NegativeInfinity`         |
    /// | `Infinity`         | `Infinity`         | Undefined (panic)          |
    /// | `Infinity`         | `NegativeInfinity` | Undefined (panic)          |
    /// | `NegativeInfinity` | `Finite` (> 0)     | `NegativeInfinity`         |
    /// | `NegativeInfinity` | `Finite` (~ 0)     | `NegativeInfinity`         |
    /// | `NegativeInfinity` | `Finite` (< 0)     | `Infinity`                 |
    /// | `NegativeInfinity` | `Infinity`         | Undefined (panic)          |
    /// | `NegativeInfinity` | `NegativeInfinity` | Undefined (panic)          |
    ///
    /// (In the table, "~ 0" refers to a value that is either equal to or
    /// unordered with zero, and "<> 0" refers to a value that is either
    /// greater than or less than zero.)
    ///
    /// # Examples
    ///
    /// ```
    /// use infinitable::*;
    ///
    /// assert_eq!(Finite(3), Finite(6) / Finite(2));
    /// assert_eq!(Infinity, Infinity / Finite(1));
    /// assert_eq!(Infinity, NegativeInfinity / Finite(-2));
    /// assert_eq!(NegativeInfinity, NegativeInfinity / Finite(1));
    /// assert_eq!(NegativeInfinity, Infinity / Finite(-2));
    /// assert_eq!(Finite(0), Finite(1) / Infinity);
    /// assert_eq!(Finite(0), Finite(1) / NegativeInfinity);
    /// ```
    ///
    /// The division operation panics when an infinite value is divided by
    /// another infinite value, or when zero is divided by itself:
    ///
    /// ```should_panic
    /// use infinitable::*;
    ///
    /// let infinity: Infinitable<i32> = Infinity;
    /// let _ = infinity / infinity;
    /// ```
    ///
    /// ```should_panic
    /// use infinitable::*;
    ///
    /// let _ = Finite(0) / Finite(0);
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if both operands are either `Infinity` or `NegativeInfinity`, or
    /// both operands are `Finite` with an underlying value equal to or
    /// unordered with zero.
    ///
    /// # Versioning
    ///
    /// Available since 1.5.0.
    fn div(self, rhs: Infinitable<T>) -> Infinitable<<T as Div>::Output> {
        match (self, rhs) {
            (Infinity, Infinity)
            | (NegativeInfinity, NegativeInfinity)
            | (Infinity, NegativeInfinity)
            | (NegativeInfinity, Infinity) => panic!("Cannot divide two infinite values"),
            (Infinity, Finite(x)) => match x.partial_cmp(&T::zero()) {
                Some(Ordering::Less) => NegativeInfinity,
                _ => Infinity,
            },
            (NegativeInfinity, Finite(x)) => match x.partial_cmp(&T::zero()) {
                Some(Ordering::Less) => Infinity,
                _ => NegativeInfinity,
            },
            (Finite(_), Infinity) | (Finite(_), NegativeInfinity) => {
                Finite(<T as Div>::Output::zero())
            }
            (Finite(lf), Finite(rf)) => match rf.partial_cmp(&T::zero()) {
                Some(Ordering::Greater) | Some(Ordering::Less) => Finite(lf.div(rf)),
                _ => match lf.partial_cmp(&T::zero()) {
                    Some(Ordering::Greater) => Infinity,
                    Some(Ordering::Less) => NegativeInfinity,
                    _ => panic!("Cannot divide two zeros or unordered values"),
                },
            },
        }
    }
}

impl<T> Neg for Infinitable<T>
where
    T: Neg,
{
    type Output = Infinitable<T::Output>;

    /// Negates the value, when the underlying type `T` supports negation.
    ///
    /// [`Infinity`] is negated to [`NegativeInfinity`] (and vice versa),
    /// and [`Finite`] is negated based on the underlying value.
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
    /// Formats the value, when the underlying type `T` supports formatting.
    ///
    /// [`Infinity`] is formatted to `"inf"`, [`NegativeInfinity`] is formatted
    /// to `"-inf"`, and [`Finite`] is formatted based on the underlying value.
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

/// Converts from [`f32`] value to an optional [`Infinitable<f32>`],
/// accounting for floating-point infinities and NaN.
///
/// The value is converted as follows:
///
/// | value             | result                                   |
/// |-------------------|------------------------------------------|
/// | Finite value `x`  | <code>[Some]\([Finite]\(x))</code>       |
/// | Positive infinity | <code>[Some]\([Infinity])</code>         |
/// | Negative infinity | <code>[Some]\([NegativeInfinity])</code> |
/// | NaN               | [`None`]                                 |
///
/// # Examples
///
/// ```
/// use infinitable::*;
///
/// let finite = from_f32(5.0);
/// assert_eq!(Some(Finite(5.0)), finite);
/// let infinity = from_f32(f32::INFINITY);
/// assert_eq!(Some(Infinity), infinity);
/// let nan = from_f32(f32::NAN);
/// assert_eq!(None, nan);
/// ```
///
/// # Versioning
///
/// Available since 1.5.0.
pub fn from_f32(value: f32) -> Option<Infinitable<f32>> {
    if value.is_finite() {
        Some(Finite(value))
    } else if value.is_nan() {
        None
    } else if value.is_sign_positive() {
        Some(Infinity)
    } else {
        Some(NegativeInfinity)
    }
}

/// Converts from [`f64`] value to an optional [`Infinitable<f64>`],
/// accounting for floating-point infinities and NaN.
///
/// The value is converted as follows:
///
/// | value             | result                                   |
/// |-------------------|------------------------------------------|
/// | Finite value `x`  | <code>[Some]\([Finite]\(x))</code>       |
/// | Positive infinity | <code>[Some]\([Infinity])</code>         |
/// | Negative infinity | <code>[Some]\([NegativeInfinity])</code> |
/// | NaN               | [`None`]                                 |
///
/// # Examples
///
/// ```
/// use infinitable::*;
///
/// let finite = from_f64(5.0);
/// assert_eq!(Some(Finite(5.0)), finite);
/// let infinity = from_f64(f64::INFINITY);
/// assert_eq!(Some(Infinity), infinity);
/// let nan = from_f64(f64::NAN);
/// assert_eq!(None, nan);
/// ```
///
/// # Versioning
///
/// Available since 1.5.0.
pub fn from_f64(value: f64) -> Option<Infinitable<f64>> {
    if value.is_finite() {
        Some(Finite(value))
    } else if value.is_nan() {
        None
    } else if value.is_sign_positive() {
        Some(Infinity)
    } else {
        Some(NegativeInfinity)
    }
}
