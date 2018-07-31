bitflags! {
    pub struct GenericSupport: u32 {
        const None     = 0b000;
        const Type     = 0b001;
        const Lifetime = 0b010;
        const Const    = 0b100;
        const All      = 0b111;
    }
}

// TODO: Differentiate between structs with named fields and tuple structs?
// maybe const Struct = 0b11;
// maybe const NamedStruct = 0b01;
// maybe const TupleStruct = 0b10;
bitflags! {
    pub struct DataSupport: u32 {
        const None   = 0b000;
        const Struct = 0b001;
        const Enum   = 0b010;
        const Union  = 0b100;
        const All    = 0b111;
    }
}
