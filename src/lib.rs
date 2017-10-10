use std::cmp::Ordering;

/// An "infinitable" value, one that can be either finite or infinite
#[derive(Debug)]
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
}

impl<T> PartialEq for Infinitable<T> where T: PartialEq {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(&Infinitable::Infinity, &Infinitable::Infinity)
			| (&Infinitable::NegativeInfinity, &Infinitable::NegativeInfinity)
				=> true,
			(&Infinitable::Finite(ref x), &Infinitable::Finite(ref y))
				=> x == y,
			(..)
				=> false,
		}
	}
}

impl<T> Eq for Infinitable<T> where T: Eq {
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
