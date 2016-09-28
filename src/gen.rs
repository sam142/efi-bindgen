use types::*;
use syntax::ast::*;
use syntax::parse::token;
use syntax::codemap::DUMMY_SP;
use syntax::ptr::P;
use syntax::print::pprust;
use syntax::print::pp::eof;

fn gen_ident<S: Into<String>>(name: S) -> Ident {
    Ident::with_empty_ctxt(token::intern(&name.into()))
}

fn mk_type<S: Into<String>>(global: bool, segment: S) -> P<Ty> {
    let segments = PathSegment {
        identifier: gen_ident(segment),
        parameters: PathParameters::AngleBracketed(AngleBracketedParameterData {
            lifetimes: Vec::new(),
            types: P::new(),
            bindings: P::new(),
        }),
    };

    let node = TyKind::Path(None,
                            Path {
                                span: DUMMY_SP,
                                global: global,
                                segments: vec![segments],
                            });

    P(Ty {
        id: DUMMY_NODE_ID,
        node: node,
        span: DUMMY_SP,
    })
}

fn gen_type(ty: &EfiType) -> P<Ty> {
    match *ty {
        EfiType::Status => mk_type(false, "u64"),
        EfiType::UIntN => mk_type(false, "u64"),
        EfiType::IntN => mk_type(false, "i64"),
        EfiType::Bool => mk_type(false, "bool"),
        EfiType::Int8 => mk_type(false, "i8"),
        EfiType::UInt8 => mk_type(false, "u8"),
        EfiType::Int16 => mk_type(false, "i16"),
        EfiType::UInt16 => mk_type(false, "u16"),
        EfiType::Int32 => mk_type(false, "i32"),
        EfiType::UInt32 => mk_type(false, "u32"),
        EfiType::Int64 => mk_type(false, "i64"),
        EfiType::UInt64 => mk_type(false, "u64"),
        EfiType::Char8 => mk_type(false, "u8"),
        EfiType::Char16 => mk_type(false, "u16"),
        EfiType::Id(ref name) => mk_type(false, &name[..]),
        EfiType::Ptr(ref ty) => {
            P(Ty {
                id: DUMMY_NODE_ID,
                node: TyKind::Ptr(MutTy {
                    ty: gen_type(&*ty),
                    mutbl: Mutability::Immutable,
                }),
                span: DUMMY_SP,
            })
        }
    }
}

fn gen_struct_fields(fields: &Vec<EfiField>) -> Vec<StructField> {
    fields.iter()
        .map(|field| {
            StructField {
                span: DUMMY_SP,
                ident: Some(gen_ident(&field.name[..])),
                vis: Visibility::Public,
                id: DUMMY_NODE_ID,
                ty: gen_type(&field.ty),
                attrs: Vec::new(),
            }
        })
        .collect()
}

fn gen_record(record: &EfiRecord) -> VariantData {
    match *record {
        EfiRecord::Struct { ref fields, .. } => {
            VariantData::Struct(gen_struct_fields(fields), DUMMY_NODE_ID)
        }
        EfiRecord::Union { .. } => panic!("unions not yet supported"),
    }
}

pub fn gen_module(module: &EfiModule) {
    println!("{:?}", module);

    let records = module.records
        .iter()
        .map(|r| {
            P(Item {
                ident: gen_ident(r.get_name()),
                attrs: Vec::new(),
                id: DUMMY_NODE_ID,
                node: ItemKind::Struct(gen_record(r), Generics::default()),
                vis: Visibility::Public,
                span: DUMMY_SP,
            })
        })
        .collect();

    let module = Mod {
        inner: DUMMY_SP,
        items: records,
    };

    let mut ps = pprust::rust_printer(Box::new(::std::io::stdout()));
    ps.print_mod(&module, &[]);
    ps.print_remaining_comments();
    eof(&mut ps.s);
    ps.s.out.flush();
}
