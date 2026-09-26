#![allow(non_upper_case_globals)]

bitflags! {
    #[derive(Copy, Clone, Default)]
    pub struct Support: u8 {
        const None            = 0b000_0000;
        const Type            = 0b000_0001;
        const Lifetime        = 0b000_0010;
        const Const           = 0b000_0100;
        const AllGeneric      = 0b000_0111;
        const TupleStruct     = 0b000_1000;
        const NamedStruct     = 0b001_0000;
        const Struct          = 0b001_1000;
        const Enum            = 0b010_0000;
        const Union           = 0b100_0000;
        const AllData         = 0b111_1000;
        const All             = 0b111_1111;
    }
}
