#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum EfiArgDir {
    In,
    Out,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EfiArg {
    pub name: String,
    pub ty: EfiType,
    pub dir: EfiArgDir,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EfiMethod {
    pub name: String,
    pub ty: EfiType,
    pub args: Vec<EfiArg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EfiField {
    pub name: String,
    pub ty: EfiType,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EfiRecord {
    Struct { name: String, fields: Vec<EfiField> },
    Union { name: String, fields: Vec<EfiField> },
}

impl EfiRecord {
    pub fn get_name(&self) -> &str {
        match *self {
            EfiRecord::Struct { ref name, .. } => &name[..],
            EfiRecord::Union { ref name, .. } => &name[..],
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EfiProtocol {
    pub name: String,
    pub methods: Vec<EfiMethod>,
    pub fields: Vec<EfiField>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EfiModule {
    pub protocols: Vec<EfiProtocol>,
    pub records: Vec<EfiRecord>,
}
