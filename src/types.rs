#[derive(Clone, Debug)]
pub enum EfiType {
    Status,
    Bool,
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    IntN,
    UIntN,
    Char8,
    Char16,
    Id(String),
    Ptr(Box<EfiType>),
}

#[derive(Copy, Clone, Debug)]
pub enum EfiArgDir {
    In,
    Out,
}

#[derive(Clone, Debug)]
pub struct EfiArg {
    pub name: String,
    pub ty: EfiType,
    pub dir: EfiArgDir,
    pub optional: bool,
}

#[derive(Clone, Debug)]
pub struct EfiMethod {
    pub name: String,
    pub ty: EfiType,
    pub args: Vec<EfiArg>,
}

#[derive(Clone, Debug)]
pub struct EfiField {
    pub name: String,
    pub ty: EfiType,
}

#[derive(Clone, Debug)]
pub enum EfiRecordKind {
    Union,
    Struct,
}

#[derive(Clone, Debug)]
pub struct EfiRecord {
    pub name: String,
    pub fields: Vec<EfiField>,
    pub kind: EfiRecordKind,
}

#[derive(Clone, Debug)]
pub struct EfiVariant {
    pub name: String,
    pub value: Option<u64>,
}

#[derive(Clone, Debug)]
pub struct EfiEnum {
    pub name: String,
    pub fields: Vec<EfiVariant>,
}

#[derive(Clone, Debug)]
pub struct EfiProtocol {
    pub name: String,
    pub methods: Vec<EfiMethod>,
    pub fields: Vec<EfiField>,
}

#[derive(Clone, Debug)]
pub struct EfiModule {
    pub protocols: Vec<EfiProtocol>,
    pub records: Vec<EfiRecord>,
    pub enums: Vec<EfiEnum>,
}
