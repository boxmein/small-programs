// --- Day 16: Packet Decoder ---

// As you leave the cave and reach open waters, you receive a transmission from the Elves back on the ship.

// The transmission was sent using the Buoyancy Interchange Transmission System (BITS), a method of packing numeric expressions into a binary sequence. Your submarine's computer has saved the transmission in hexadecimal (your puzzle input).

// The first step of decoding the message is to convert the hexadecimal representation into binary. Each character of hexadecimal corresponds to four bits of binary data:

// 0 = 0000
// 1 = 0001
// 2 = 0010
// 3 = 0011
// 4 = 0100
// 5 = 0101
// 6 = 0110
// 7 = 0111
// 8 = 1000
// 9 = 1001
// A = 1010
// B = 1011
// C = 1100
// D = 1101
// E = 1110
// F = 1111

// The BITS transmission contains a single packet at its outermost layer which itself contains many other packets. The hexadecimal representation of this packet might encode a few extra 0 bits at the end; these are not part of the transmission and should be ignored.

// Every packet begins with a standard header: the first three bits encode the packet version, and the next three bits encode the packet type ID. These two values are numbers; all numbers encoded in any packet are represented as binary with the most significant bit first. For example, a version encoded as the binary sequence 100 represents the number 4.

// Packets with type ID 4 represent a literal value. Literal value packets encode a single binary number. To do this, the binary number is padded with leading zeroes until its length is a multiple of four bits, and then it is broken into groups of four bits. Each group is prefixed by a 1 bit except the last group, which is prefixed by a 0 bit. These groups of five bits immediately follow the packet header. For example, the hexadecimal string D2FE28 becomes:

// 110100101111111000101000
// VVVTTTAAAAABBBBBCCCCC

// Below each bit is a label indicating its purpose:

//     The three bits labeled V (110) are the packet version, 6.
//     The three bits labeled T (100) are the packet type ID, 4, which means the packet is a literal value.
//     The five bits labeled A (10111) start with a 1 (not the last group, keep reading) and contain the first four bits of the number, 0111.
//     The five bits labeled B (11110) start with a 1 (not the last group, keep reading) and contain four more bits of the number, 1110.
//     The five bits labeled C (00101) start with a 0 (last group, end of packet) and contain the last four bits of the number, 0101.
//     The three unlabeled 0 bits at the end are extra due to the hexadecimal representation and should be ignored.

// So, this packet represents a literal value with binary representation 011111100101, which is 2021 in decimal.

// Every other type of packet (any packet with a type ID other than 4) represent an operator that performs some calculation on one or more sub-packets contained within. Right now, the specific operations aren't important; focus on parsing the hierarchy of sub-packets.

// An operator packet contains one or more packets. To indicate which subsequent binary data represents its sub-packets, an operator packet can use one of two modes indicated by the bit immediately after the packet header; this is called the length type ID:

//     If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
//     If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.

// Finally, after the length type ID bit and the 15-bit or 11-bit field, the sub-packets appear.

// For example, here is an operator packet (hexadecimal string 38006F45291200) with length type ID 0 that contains two sub-packets:

// 00111000000000000110111101000101001010010001001000000000
// VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB

//     The three bits labeled V (001) are the packet version, 1.
//     The three bits labeled T (110) are the packet type ID, 6, which means the packet is an operator.
//     The bit labeled I (0) is the length type ID, which indicates that the length is a 15-bit number representing the number of bits in the sub-packets.
//     The 15 bits labeled L (000000000011011) contain the length of the sub-packets in bits, 27.
//     The 11 bits labeled A contain the first sub-packet, a literal value representing the number 10.
//     The 16 bits labeled B contain the second sub-packet, a literal value representing the number 20.

// After reading 11 and 16 bits of sub-packet data, the total length indicated in L (27) is reached, and so parsing of this packet stops.

// As another example, here is an operator packet (hexadecimal string EE00D40C823060) with length type ID 1 that contains three sub-packets:

// 11101110000000001101010000001100100000100011000001100000
// VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC

//     The three bits labeled V (111) are the packet version, 7.
//     The three bits labeled T (011) are the packet type ID, 3, which means the packet is an operator.
//     The bit labeled I (1) is the length type ID, which indicates that the length is a 11-bit number representing the number of sub-packets.
//     The 11 bits labeled L (00000000011) contain the number of sub-packets, 3.
//     The 11 bits labeled A contain the first sub-packet, a literal value representing the number 1.
//     The 11 bits labeled B contain the second sub-packet, a literal value representing the number 2.
//     The 11 bits labeled C contain the third sub-packet, a literal value representing the number 3.

// After reading 3 complete sub-packets, the number of sub-packets indicated in L (3) is reached, and so parsing of this packet stops.

// For now, parse the hierarchy of the packets throughout the transmission and add up all of the version numbers.

// Here are a few more examples of hexadecimal-encoded transmissions:

//     8A004A801A8002F478 represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.
//     620080001611562C8802118E34 represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet that contains two literal values. This packet has a version sum of 12.
//     C0015000016115A2E0802F182340 has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.
//     A0016C880162017C3686B18A3D4780 is an operator packet that contains an operator packet that contains an operator packet that contains five literal values; it has a version sum of 31.

// Decode the structure of your hexadecimal-encoded BITS transmission; what do you get if you add up the version numbers in all packets?

// To begin, get your puzzle input.

// --- Part Two ---

// Now that you have the structure of your transmission decoded, you can calculate the value of the expression it represents.

// Literal values (type ID 4) represent a single number as described above. The remaining type IDs are more interesting:

// Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
// Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
// Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
// Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
// Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
// Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
// Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
// Using these rules, you can now work out the value of the outermost packet in your BITS transmission.

// For example:

// C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
// 04005AC33890 finds the product of 6 and 9, resulting in the value 54.
// 880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
// CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
// D8005AC2A8F0 produces 1, because 5 is less than 15.
// F600BC2D8F produces 0, because 5 is not greater than 15.
// 9C005AC2F8F0 produces 0, because 5 is not equal to 15.
// 9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
// What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?

use nom::{
  IResult,
  bits,
  error::Error,
};

use hex::decode;

use std::io::{stdin, BufRead};

#[derive(Debug)]
enum Packet {
  Sum {
    version: u8,
    type_id: u8,
    subpackets: Vec<Packet>,
  },
  Product {
    version: u8,
    type_id: u8,
    subpackets: Vec<Packet>,
  },
  Min {
    version: u8,
    type_id: u8,
    subpackets: Vec<Packet>,
  },
  Max {
    version: u8,
    type_id: u8,
    subpackets: Vec<Packet>,
  },
  Gt {
    version: u8,
    type_id: u8,
    subpackets: Vec<Packet>,
  },
  Lt {
    version: u8,
    type_id: u8,
    subpackets: Vec<Packet>,
  },
  Eq {
    version: u8,
    type_id: u8,
    subpackets: Vec<Packet>,
  },
  Literal {
    version: u8,
    type_id: u8,
    binary_parts: Vec<u8>,
  }
}

impl Packet {
  pub fn get_version(&self) -> u64 {
    match self {
      Packet::Sum { version, .. } => *version as u64,
      Packet::Product { version, .. } => *version as u64,
      Packet::Min { version, .. } => *version as u64,
      Packet::Max { version, .. } => *version as u64,
      Packet::Gt { version, .. } => *version as u64,
      Packet::Lt { version, .. } => *version as u64,
      Packet::Eq { version, .. } => *version as u64,
      Packet::Literal { version, .. } => *version as u64
    }
  }
  pub fn get_subpackets(&self) -> Option<&[Packet]> {
    match self {
      Packet::Sum { subpackets, .. } => Some(subpackets),
      Packet::Product { subpackets, .. } => Some(subpackets),
      Packet::Min { subpackets, .. } => Some(subpackets),
      Packet::Max { subpackets, .. } => Some(subpackets),
      Packet::Gt { subpackets, .. } => Some(subpackets),
      Packet::Lt { subpackets, .. } => Some(subpackets),
      Packet::Eq { subpackets, .. } => Some(subpackets),
      Packet::Literal { .. } => None
    }
  }
  pub fn get_value(&self) -> u64 {
    match self {
      Packet::Literal { binary_parts, .. } => {
        assert!(binary_parts.len() > 0);
        assert!(binary_parts.len() <= 16); // 16 x 4 bits == 64 bits

        let mut value: u64 = 0;
        let max_offset = binary_parts.len() - 1;
        for i in 0..binary_parts.len() {
          let offset = (max_offset - i) * 4;
          let part_bit: u64 = binary_parts[i] as u64 & 0xf; // slice to a nibble
          value |= part_bit << offset;
        }

        value
      },
      Packet::Sum { subpackets, .. } => {
        assert!(subpackets.len() > 0);
        subpackets.iter()
          .map(|pkt| pkt.get_value())
          .reduce(|acc, i| acc + i)
          .expect("Packet had 0 subpackets")
      },
      Packet::Product { subpackets, .. } => {
        assert!(subpackets.len() > 0);
        subpackets.iter()
          .map(|pkt| pkt.get_value())
          .reduce(|acc, i| acc * i)
          .expect("Packet had 0 subpackets")
      },
      Packet::Max { subpackets, .. } => {
        assert!(subpackets.len() > 0);
        subpackets.iter()
          .map(|pkt| pkt.get_value())
          .max()
          .expect("Packet had 0 subpackets")
      },
      Packet::Min { subpackets, .. } => {
        assert!(subpackets.len() > 0);
        subpackets.iter()
          .map(|pkt| pkt.get_value())
          .min()
          .expect("Packet had 0 subpackets")
      },
      Packet::Gt { subpackets, .. } => {
        assert_eq!(subpackets.len(), 2);
        if subpackets[0].get_value() > subpackets[1].get_value() {
          1
        } else {
          0
        }
      },
      Packet::Lt { subpackets, .. } => {
        assert_eq!(subpackets.len(), 2);

        if subpackets[0].get_value() < subpackets[1].get_value() {
          1
        } else {
          0
        }
      },
      Packet::Eq { subpackets, .. } => {
        assert_eq!(subpackets.len(), 2);
        if subpackets[0].get_value() == subpackets[1].get_value() {
          1
        } else {
          0
        }
      },
    }
  }
}

fn from_hex(input: &str) -> Result<Vec<u8>, hex::FromHexError> {
  decode(input)
}

fn extract_literal_packet_contents(
  (data, cursor): (&[u8], usize),
) -> IResult<(&[u8], usize), Vec<u8>, Error<(&[u8], usize)>> {
  let extract_binary_part = bits::complete::take::<_, u8, _, _>(5usize);

  let mut data = data;
  let mut cursor = cursor;
  let mut binparts: Vec<u8> = vec![];

  loop {
    let ((d, cur), binary_part) = extract_binary_part((data, cursor))?;
    println!("binary part: {:>05b}", binary_part);
    data = d;
    cursor = cur;
    binparts.push(binary_part);

    let is_end = (binary_part >> 4 & 1) == 0;
    if is_end {
      println!("end of binary parts");
      break;
    }
  }

  Ok(((data, cursor), binparts))
}

fn extract_subpackets_lti_1(
  (data, cursor): (&[u8], usize)
) -> IResult<(&[u8], usize), Vec<Packet>, Error<(&[u8], usize)>> {
  let extract_length_1 = bits::complete::take::<_, u16, _, _>(11usize);

  let mut subpackets: Vec<Packet> = vec![];
  let ((data, cursor), length) = extract_length_1((data, cursor))?;

  println!("length data 1: {} ({:>011b})", length, length);
  let mut data = data;
  let mut cursor = cursor;
  for i in 0..length {
    println!("recursively parsing packet {}", i);
    let ((d, cur), pkt) = extract_packet((data, cursor))?;
    data = d;
    cursor = cur;

    subpackets.push(pkt);
  }
  
  Ok(((data, cursor), subpackets))
}

fn extract_subpackets_lti_0(
  (data, cursor): (&[u8], usize)
) -> IResult<(&[u8], usize), Vec<Packet>, Error<(&[u8], usize)>> {
  let mut subpackets: Vec<Packet> = vec![];
  let extract_length_0 = bits::complete::take::<_, u16, _, _>(15usize);

  let ((data, cursor), length) = extract_length_0((data, cursor))?;
  
  let unconsumed: usize = data.len() * 8 - cursor;
  let target_unconsumed: usize = unconsumed - length as usize;
  
  println!("length data 0: {} ({:>08b})", length, length);
  println!("{} bits unconsumed, until target {}", unconsumed, target_unconsumed);
  
  let mut cursor = cursor;
  let mut data = data;
  loop {
    println!("parsing a packet to eat more bits");
  
    let ((d, cur), pkt) = extract_packet((data, cursor))?;
    data = d;
    cursor = cur;
  
    subpackets.push(pkt);
  
    let new_unconsumed = data.len() * 8 - cursor;
    
    println!("{} bits unconsumed", new_unconsumed);
    
    if new_unconsumed <= target_unconsumed {
      println!("reached target, breaking");
      break;
    }
  }

  Ok(((data, cursor), subpackets))
}

fn extract_operator_packet_contents(
  (data, cursor): (&[u8], usize),
) -> IResult<(&[u8], usize), Vec<Packet>, Error<(&[u8], usize)>> {
  let extract_length_type_id = bits::complete::take::<_, u8, _, _>(1usize);

  let ((data, cursor), lti) = extract_length_type_id((data, cursor))?;
  println!("length type identifier: {}", lti);
  let ((data, cursor), subpackets) = 
    if lti == 1 {
      extract_subpackets_lti_1((data, cursor))?
    } else if lti == 0 {
      extract_subpackets_lti_0((data, cursor))?
    } else {
      panic!("unsupported length type ID");
    };

  Ok(((data, cursor), subpackets))
}

fn extract_packet((data, cursor): (&[u8], usize)) -> IResult<(&[u8], usize), Packet, Error<(&[u8], usize)>> {
  let extract_version = bits::complete::take::<_, u8, _, _>(3usize);
  let extract_type_id = bits::complete::take::<_, u8, _, _>(3usize);

  println!("received {} bytes ({} bits with cursor)", data.len(), data.len() * 8 - cursor);

  let ((data, cursor), version) = extract_version((data, cursor))?;
  println!("version: {}", version);
  let ((mut data, cursor), type_id) = extract_type_id((data, cursor))?;
  println!("type_id: {}", type_id);

  if type_id == 4 {
    let ((data, cursor), binary_parts) = extract_literal_packet_contents((data, cursor))?;
    Ok(((data, cursor), Packet::Literal {
      version,
      type_id,
      binary_parts,
    }))
  } else {
    let ((data, cursor), subpackets) = extract_operator_packet_contents((data, cursor))?;
    let pkt = match type_id {
      0 => Packet::Sum {
        version,
        type_id,
        subpackets,
      },
      1 => Packet::Product {
        version,
        type_id,
        subpackets,
      },
      2 => Packet::Min {
        version,
        type_id,
        subpackets,
      },
      3 => Packet::Max {
        version,
        type_id,
        subpackets,
      },
      5 => Packet::Gt {
        version,
        type_id,
        subpackets,
      },
      6 => Packet::Lt {
        version,
        type_id,
        subpackets,
      },
      7 => Packet::Eq {
        version,
        type_id,
        subpackets,
      },
      _ => panic!("Invalid type ID!")
    };
    Ok(((data, cursor), pkt))
  }
}

fn sum_version_numbers(existing_sum: u64, packet: &Packet) -> u64 {
  let mut sum = existing_sum;

  sum += packet.get_version();

  if let Some(subpackets) = packet.get_subpackets() {
    for subpk in subpackets {
      sum = sum_version_numbers(sum, subpk);
    }
  }

  sum
}

fn main() {
  let mut s: String = String::default();
  for line in stdin()
      .lock()
      .lines()
      .filter(|value| value.is_ok())
      .map(|value| value.expect("stdin failure")) {
      s += &line;
  }

  let data = from_hex(&s).expect("invalid hex");
  let ((data, cursor), packet) = extract_packet((&data, 0)).expect("failed to parse input!");
  let result = sum_version_numbers(0, &packet);
  println!("Sum of all version numbers: {}", result);
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn from_hex_works() {
    assert_eq!(
      &from_hex("01020304").unwrap(),
      &[1, 2, 3, 4]
    );
  }

  #[test]
  fn typeid_4_parser_works() {
    let data = from_hex("D2FE28").unwrap();
    let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();

    match pkt {
      Packet::Literal { version, type_id, binary_parts } => {
        assert_eq!(version, 6);
        assert_eq!(type_id, 4);
        assert_eq!(binary_parts.len(), 3);
        assert_eq!(binary_parts[0], 0b10111);
        assert_eq!(binary_parts[1], 0b11110);
        assert_eq!(binary_parts[2], 0b00101);
      }
      _ => panic!("Wrong packet type!")
    }
  }

  #[test]
  fn operator_packet_example_lti_0_works() {
    let data = from_hex("38006F45291200").unwrap();
    let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();

    match pkt {
      Packet::Lt { version, type_id, subpackets } => {    
        assert_eq!(version, 1);
        assert_eq!(type_id, 6);
        assert_eq!(subpackets.len(), 2);

        match subpackets[0] {
          Packet::Literal { type_id, .. } => {
            assert_eq!(type_id, 4);
          }
          _ => panic!("Wrong packet type!")
        }
        match subpackets[1] {
          Packet::Literal { type_id, .. } => {
            assert_eq!(type_id, 4);
          }
          _ => panic!("Wrong packet type!")
        }
      }
      _ => panic!("Wrong packet type!")
    }
  }

  #[test]
  fn operator_packet_example_lti_1_works() {
    let data = from_hex("EE00D40C823060").unwrap();
    let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();

    println!("{:?}", pkt);
    match pkt {
      Packet::Max { version, type_id, subpackets } => {    
        assert_eq!(version, 7);
        assert_eq!(type_id, 3);
        assert_eq!(subpackets.len(), 3);

        match subpackets[0] {
          Packet::Literal { type_id, .. } => {
            assert_eq!(type_id, 4);
          }
          _ => panic!("Wrong packet type!")
        }
        match subpackets[1] {
          Packet::Literal { type_id, .. } => {
            assert_eq!(type_id, 4);
          }
          _ => panic!("Wrong packet type!")
        }
      }
      _ => panic!("Wrong packet type!")
    }
  }

  #[test]
  fn part_1_example_1_works() {
    let data = from_hex("8A004A801A8002F478").unwrap();
    let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();
    let sum = sum_version_numbers(0, &pkt);
    assert_eq!(sum, 16);
  }

  #[test]
  fn part_1_example_2_works() {
    let data = from_hex("620080001611562C8802118E34").unwrap();
    let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();
    let sum = sum_version_numbers(0, &pkt);
    assert_eq!(sum, 12);
  }

  #[test]
  fn part_1_example_3_works() {
    let data = from_hex("C0015000016115A2E0802F182340").unwrap();
    let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();
    let sum = sum_version_numbers(0, &pkt);
    assert_eq!(sum, 23);
  }

  #[test]
  fn part_1_example_4_works() {
    let data = from_hex("A0016C880162017C3686B18A3D4780").unwrap();
    let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();
    let sum = sum_version_numbers(0, &pkt);
    assert_eq!(sum, 31);
  }

  #[test]
  fn literal_packing_works() {
    let packet = Packet::Literal {
      version: 1,
      type_id: 4,
      binary_parts: vec![
        0b1_0000,
        0b1_0001,
        0b1_1111,
        0b1_1110,
        0b1_0011,
        0b0_1010,
      ],
    };

    assert_eq!(
      packet.get_value(),
      0b0000_0001_1111_1110_0011_1010,
    )
  }

  macro_rules! part_2_test {
    ($name:ident, $hex:expr, $value:expr) => {
      #[test]
      fn $name() {
        let data = from_hex($hex).unwrap();
        let ((data, cursor), pkt) = extract_packet((&data, 0)).unwrap();
        assert_eq!(pkt.get_value(), $value);
      }
    }
  }

  part_2_test!(sum_works, "C200B40A82", 3);
  part_2_test!(product_works, "04005AC33890", 54);
  part_2_test!(min_works, "880086C3E88112", 7);
  part_2_test!(max_works, "CE00C43D881120", 9);
  part_2_test!(lt_works, "D8005AC2A8F0", 1);
  part_2_test!(gt_works, "F600BC2D8F", 0);
  part_2_test!(eq_works, "9C005AC2F8F0", 0);
  part_2_test!(nested_works, "9C0141080250320F1802104A08", 1);
}