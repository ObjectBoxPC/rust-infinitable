use infinitable::*;

const NEGINF: Infinitable<i32> = NegativeInfinity;
const INF: Infinitable<i32> = Infinity;
const ZERO: Infinitable<i32> = Finite(0);
const ONE: Infinitable<i32> = Finite(1);
const NEGONE: Infinitable<i32> = Finite(-1);
const TWO: Infinitable<i32> = Finite(2);

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
    assert!(INF <= INF);
    assert!(NEGINF <= NEGINF);
    assert!(INF >= INF);
    assert!(NEGINF >= NEGINF);
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

    assert!(!(INF < ONE));
    assert!(!(INF <= ONE));
    assert!(!(INF < NEGINF));
    assert!(!(INF <= NEGINF));
    assert!(!(ONE > INF));
    assert!(!(ONE >= INF));
    assert!(!(NEGINF > INF));
    assert!(!(NEGINF >= INF));
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

    assert!(!(NEGINF > ZERO));
    assert!(!(NEGINF >= ZERO));
    assert!(!(NEGINF > INF));
    assert!(!(NEGINF >= INF));
    assert!(!(ZERO < NEGINF));
    assert!(!(ZERO <= NEGINF));
    assert!(!(INF < NEGINF));
    assert!(!(INF <= NEGINF));
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
fn can_add() {
    assert_eq!(ONE, ONE + ZERO);
    assert_eq!(ONE, ZERO + ONE);
    assert_eq!(TWO, ONE + ONE);
    assert_eq!(INF, ONE + INF);
    assert_eq!(INF, INF + ONE);
    assert_eq!(INF, INF + INF);
    assert_eq!(NEGINF, ONE + NEGINF);
    assert_eq!(NEGINF, NEGINF + ONE);
    assert_eq!(NEGINF, NEGINF + NEGINF);
}

#[test]
#[should_panic(expected = "Cannot add infinity and negative infinity")]
fn cannot_add_inf_and_neginf() {
    let _ = INF + NEGINF;
}

#[test]
#[should_panic(expected = "Cannot add infinity and negative infinity")]
fn cannot_add_neginf_and_inf() {
    let _ = NEGINF + INF;
}

#[test]
fn can_subtract() {
    assert_eq!(ONE, ONE - ZERO);
    assert_eq!(ZERO, ONE - ONE);
    assert_eq!(INF, INF - ONE);
    assert_eq!(INF, ONE - NEGINF);
    assert_eq!(INF, INF - NEGINF);
    assert_eq!(NEGINF, NEGINF - ONE);
    assert_eq!(NEGINF, ONE - INF);
    assert_eq!(NEGINF, NEGINF - INF);
}

#[test]
#[should_panic(expected = "Cannot subtract infinite value from itself")]
fn cannot_subtract_inf_from_self() {
    let _ = INF - INF;
}

#[test]
#[should_panic(expected = "Cannot subtract infinite value from itself")]
fn cannot_subtract_neginf_from_self() {
    let _ = NEGINF - NEGINF;
}

#[test]
fn can_multiply() {
    assert_eq!(ONE, ONE * ONE);
    assert_eq!(ZERO, ZERO * ONE);
    assert_eq!(INF, ONE * INF);
    assert_eq!(INF, INF * ONE);
    assert_eq!(INF, INF * INF);
    assert_eq!(INF, NEGINF * NEGINF);
    assert_eq!(INF, NEGONE * NEGINF);
    assert_eq!(NEGINF, ONE * NEGINF);
    assert_eq!(NEGINF, NEGINF * ONE);
    assert_eq!(NEGINF, NEGONE * INF);
    assert_eq!(NEGINF, NEGINF * INF);
}

#[test]
#[should_panic(expected = "Cannot multiply infinite value and zero or unordered value")]
fn cannot_multiply_inf_and_zero() {
    let _ = INF * ZERO;
}

#[test]
#[should_panic(expected = "Cannot multiply infinite value and zero or unordered value")]
fn cannot_multiply_neginf_and_zero() {
    let _ = NEGINF * ZERO;
}

#[test]
fn can_divide() {
    assert_eq!(ONE, ONE / ONE);
    assert_eq!(ONE, TWO / TWO);
    assert_eq!(NEGONE, ONE / NEGONE);
    assert_eq!(NEGONE, NEGONE / ONE);
    assert_eq!(ZERO, ZERO / ONE);
    assert_eq!(ZERO, ZERO / NEGONE);
    assert_eq!(ZERO, ONE / INF);
    assert_eq!(ZERO, ONE / NEGINF);
    assert_eq!(ZERO, NEGONE / NEGINF);
    assert_eq!(ZERO, NEGONE / INF);
    assert_eq!(INF, INF / ONE);
    assert_eq!(INF, INF / ZERO);
    assert_eq!(INF, NEGINF / NEGONE);
    assert_eq!(NEGINF, NEGINF / ONE);
    assert_eq!(NEGINF, NEGINF / ZERO);
    assert_eq!(NEGINF, INF / NEGONE);
}

#[test]
#[should_panic(expected = "Cannot divide two infinite values")]
fn cannot_divide_inf_by_itself() {
    let _ = INF / INF;
}

#[test]
#[should_panic(expected = "Cannot divide two infinite values")]
fn cannot_divide_neginf_by_itself() {
    let _ = INF / INF;
}

#[test]
#[should_panic(expected = "Cannot divide two zeros or unordered values")]
fn cannot_divide_zero_by_itself() {
    let _ = ZERO / ZERO;
}

#[test]
fn can_negate() {
    assert_eq!(ZERO, -ZERO);
    assert_eq!(NEGONE, -ONE);
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

#[test]
fn can_convert_into() {
    assert_eq!(Finite(1i64), ONE.convert_into());
    assert_eq!(Ok(Finite(1i16)), ONE.try_convert_into());
    assert!(NEGONE.try_convert_into::<u32>().is_err());
}

#[test]
fn can_convert_from_floating_point() {
    assert_eq!(Some(Finite(1.0)), from_f32(1.0));
    assert_eq!(Some(Infinity), from_f32(f32::INFINITY));
    assert_eq!(Some(NegativeInfinity), from_f32(f32::NEG_INFINITY));
    assert_eq!(None, from_f32(f32::NAN));
    assert_eq!(Some(Finite(1.0)), from_f64(1.0));
    assert_eq!(Some(Infinity), from_f64(f64::INFINITY));
    assert_eq!(Some(NegativeInfinity), from_f64(f64::NEG_INFINITY));
    assert_eq!(None, from_f64(f64::NAN));
}
