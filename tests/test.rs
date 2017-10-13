extern crate infinitable;
use infinitable::Infinitable;

const NEGINF: Infinitable<i32> = Infinitable::NegativeInfinity;
const INF: Infinitable<i32> = Infinitable::Infinity;
const ZERO: Infinitable<i32> = Infinitable::Finite(0);
const ONE: Infinitable<i32> = Infinitable::Finite(1);

#[test]
fn can_extract_finite_value() {
	if let Infinitable::Finite(x) = ZERO {
		assert_eq!(0, x);
	} else {
		assert!(false, "Not finite?");
	}

	if let Infinitable::Finite(x) = ONE {
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