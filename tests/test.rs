extern crate infinitable;
use infinitable::*;

const NEGINF: Infinitable<i32> = NegativeInfinity;
const INF: Infinitable<i32> = Infinity;
const ZERO: Infinitable<i32> = Finite(0);
const ONE: Infinitable<i32> = Finite(1);

#[test]
fn can_extract_finite_value() {
	if let Finite(x) = ZERO {
		assert_eq!(0, x);
	} else {
		assert!(false, "Not finite?");
	}

	if let Finite(x) = ONE {
		assert_eq!(1, x);
	} else {
		assert!(false, "Not finite?");
	}
}

#[test]
fn can_test_finiteness() {
	assert!(ZERO.is_finite());
	assert!(ONE.is_finite());
	assert!(!NEGINF.is_finite());
	assert!(!INF.is_finite());
}

#[test]
fn finite_values_compare_normally() {
	assert!(ZERO < ONE);
	assert!(ZERO <= ONE);
	assert!(ONE > ZERO);
	assert!(ONE >= ZERO);
	assert!(ONE == ONE);
	assert!(ONE <= ONE);
	assert!(ONE >= ONE);

	assert!(!(ZERO > ONE));
	assert!(!(ZERO >= ONE));
	assert!(!(ONE < ZERO));
	assert!(!(ONE <= ZERO));
}

#[test]
fn infinities_equality() {
	assert!(INF == INF);
	assert!(NEGINF == NEGINF);
	assert!(!(INF != INF));
	assert!(!(NEGINF != NEGINF));

	assert!(!(INF == NEGINF));
	assert!(INF != NEGINF);
	assert!(!(NEGINF == INF));
	assert!(NEGINF != INF);

	assert!(!(INF == ZERO));
	assert!(INF != ZERO);
	assert!(!(ZERO == INF));
	assert!(ZERO != INF);
}

#[test]
fn infinity_greater_than_all() {
	assert!(INF > ONE);
	assert!(INF >= ONE);
	assert!(INF > NEGINF);
	assert!(INF >= NEGINF);
	assert!(ONE < INF);
	assert!(ONE <= INF);
	assert!(NEGINF < INF);
	assert!(NEGINF <= INF);
}

#[test]
fn negative_infinity_less_than_all() {
	assert!(NEGINF < ZERO);
	assert!(NEGINF <= ZERO);
	assert!(NEGINF < INF);
	assert!(NEGINF <= INF);
	assert!(ZERO > NEGINF);
	assert!(ZERO >= NEGINF);
	assert!(INF > NEGINF);
	assert!(INF >= NEGINF);
}

#[test]
fn can_convert_to_finite() {
	assert_eq!(None, NEGINF.finite());
	assert_eq!(None, INF.finite());
	assert_eq!(Some(0), ZERO.finite());
	assert_eq!(Some(1), ONE.finite());
}

#[test]
fn can_format_display() {
	assert_eq!("-inf", format!("{}", NEGINF));
	assert_eq!("inf", format!("{}", INF));
	assert_eq!("0", format!("{}", ZERO));
	assert_eq!("1", format!("{}", ONE));
}

#[test]
fn can_negate() {
	assert_eq!(ZERO, -ZERO);
	assert_eq!(Finite(-1), -ONE);
	assert_eq!(NEGINF, -INF);
	assert_eq!(INF, -NEGINF);
}

#[test]
fn can_convert_from_value() {
	assert_eq!(ZERO, Infinitable::from(0));
	assert_eq!(ONE, Infinitable::from(1));
	assert_eq!(ZERO, From::from(0));
	assert_eq!(ONE, From::from(1));
}

#[test]
fn can_convert_from_option() {
	assert_eq!(ZERO, Infinitable::finite_or_infinity(Some(0)));
	assert_eq!(ONE, Infinitable::finite_or_infinity(Some(1)));
	assert_eq!(INF, Infinitable::finite_or_infinity(None));
	assert_eq!(ZERO, Infinitable::finite_or_negative_infinity(Some(0)));
	assert_eq!(ONE, Infinitable::finite_or_negative_infinity(Some(1)));
	assert_eq!(NEGINF, Infinitable::finite_or_negative_infinity(None));
}