use std::fmt;
use std::ops::Add;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
pub struct Ratio<T = i64>(T, T);

fn lcm<T>(a: T, b: T) -> T {

}

impl<T: Display> fmt::Display for Ratio<T> {
    fn fmt(&self, w: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(w, "{}/{}", self.0, self.1)
    }
}

impl<T: Add> Add for Ratio<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self {

        let lcm = self.1 * other.1;

        Self(
            self.0 + other.0,

        )
    }
}











#[cfg(test)]
mod tests {
    use super::Ratio;
    #[test]
    fn formats_correctly() {
        let result = format!("{}", Ratio(1, 2));
        assert_eq!(result, "1/2");
    }

    #[test]
    fn adds_correctly() {
        let a = Ratio(1, 4);
        let b = Ratio(1, 4);

        let res = a + b;

        assert_eq!(res, Ratio(2, 4));
    } 
}
