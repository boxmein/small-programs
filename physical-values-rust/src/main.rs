use std::ops::{Add, Sub, Mul};
use std::fmt;

/// Represents a value with an uncertainty, eg (2.5 +/- 0.5)
/// Rules from https://sciencing.com/how-to-calculate-uncertainty-13710219.html
/// First element is the value, second is the uncertainty 
#[derive(Debug, Clone, Copy)]
struct Physical(f64, f64); 

impl Add for Physical {
    type Output = Self;

    fn add(self, _rhs: Self) -> Self {
        Physical(
            self.0 + _rhs.0,
            self.1 + _rhs.1
        )
    }
}

impl Sub for Physical {
    type Output = Self;
    fn sub(self, _rhs: Self) -> Self {
        Physical(
            self.0 - _rhs.0,
            self.1 + _rhs.1
        )
    }
}

impl Mul<Physical> for Physical {
    type Output = Self;
    fn mul(self, _rhs: Self) -> Self {
        Physical(
            self.0 * _rhs.0,
            self.1 + _rhs.1
        )
    }
}

impl Mul<f64> for Physical {
    type Output = Physical;
    fn mul(self, _rhs: f64) -> Physical {
        Physical(
            self.0 * _rhs,
            self.1 * _rhs
        )
    }
}

impl fmt::Display for Physical {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} +/- {}", self.0, self.1)
    }
}


fn main() {
    let x = Physical(2.5, 0.5);
    let y = Physical(3.0, 0.1);
    let z: f64 = 200.0;
    println!("Add {}", x + y);
    println!("Mul {}", x * y);
    println!("Sub {}", x - y);
    println!("Sub with float: {}", x * z);
}
