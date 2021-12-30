# Kaitai Struct is cool but it is still very raw and not suitable for
# rust usage.
meta:
  id: adventofcode_bits
  endian: le
  bit-endian: be
doc: |
  The transmission was sent using the Buoyancy Interchange Transmission System (BITS), a method of packing numeric expressions into a binary sequence. Your submarine's computer has saved the transmission in hexadecimal (your puzzle input).
seq:
  - id: version
    type: b3
  - id: type_id
    type: b3
  - id: data
    type:
      switch-on: type_id
      cases:
        4: literal_value
        _: operation_value
types:
  literal_value:
    seq:
      - id: literal_value_item
        type: b5
        repeat: eos
  operation_value:
    seq:
      - id: operation_type_bit
        type: b1