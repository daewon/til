fn can_divid(m: i32, n: i32) -> bool {
    m % n == 0
}

pub fn is_leap_year(year: i32) -> bool {
    if year % 4 == 0 {
        if year % 400 == 0 { true }
        else if year % 100 == 0 { false }
        else { true }
    } else {
        false
    }
}
