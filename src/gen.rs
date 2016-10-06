use types::*;
use std::io;
use syntax::ast::*;
use syntax::ast::DUMMY_NODE_ID as DID;
use syntax::abi::Abi;
use syntax::codemap::dummy_spanned;
use syntax::codemap::DUMMY_SP as DSP;
use syntax::ptr::P;
use syntax::print::pprust;
use syntax::print::pp;
use syntax::ext::base::{ExtCtxt, DummyResolver};
use syntax::ext::build::AstBuilder;
use syntax::ext::expand::ExpansionConfig;
use syntax::parse::ParseSess;
use syntax::parse::token::{InternedString, fresh_name};

fn gen_cc_name<P: Fn(&str) -> bool>(name: &str, p: P) -> String {
    fn capitalize(s: &str) -> String {
        let mut res = String::with_capacity(s.len());
        let mut chars = s.chars();

        res.extend(chars.by_ref().take(1).flat_map(|c| c.to_uppercase()));
        res.extend(chars.flat_map(|c| c.to_lowercase()));
        res
    }

    name.split('_').filter(|s| p(s)).map(|s| capitalize(s)).collect()
}

fn gen_eficc_name(name: &str) -> String {
    gen_cc_name(name, |s| s != "EFI" && s != "PROTOCOL")
}

fn gen_trait_name(name: &str) -> String {
    gen_cc_name(name, |s| s != "EFI")
}

fn gen_efisc_name(name: &str) -> String {
    let mut sc_name = String::with_capacity(name.len());
    let mut chars = name.chars();
    sc_name.extend(chars.by_ref().take(1).flat_map(|c| c.to_lowercase()));

    for c in chars {
        if c.is_uppercase() {
            sc_name.push('_');
            sc_name.extend(c.to_lowercase());
        } else {
            sc_name.push(c);
        }
    }

    sc_name
}

fn gen_repr_c(cx: &ExtCtxt) -> Attribute {
    cx.attribute(DSP,
                 cx.meta_list(DSP,
                              InternedString::new("repr"),
                              vec![cx.meta_list_item_word(DSP, InternedString::new("C"))]))
}

fn gen_derive(cx: &ExtCtxt, traits: &[&'static str]) -> Attribute {
    cx.attribute(DSP,
                 cx.meta_list(DSP,
                              InternedString::new("derive"),
                              traits.iter()
                                  .map(|t| cx.meta_list_item_word(DSP, InternedString::new(t)))
                                  .collect()))
}

fn gen_type(cx: &ExtCtxt, ty: &EfiType, dir: Option<EfiArgDir>) -> P<Ty> {
    match *ty {
        EfiType::Status => cx.ty_ident(DSP, cx.ident_of("u64")),
        EfiType::UIntN => cx.ty_ident(DSP, cx.ident_of("u64")),
        EfiType::IntN => cx.ty_ident(DSP, cx.ident_of("i64")),
        EfiType::Bool => cx.ty_ident(DSP, cx.ident_of("bool")),
        EfiType::Int8 => cx.ty_ident(DSP, cx.ident_of("i8")),
        EfiType::UInt8 => cx.ty_ident(DSP, cx.ident_of("u8")),
        EfiType::Int16 => cx.ty_ident(DSP, cx.ident_of("i16")),
        EfiType::UInt16 => cx.ty_ident(DSP, cx.ident_of("u16")),
        EfiType::Int32 => cx.ty_ident(DSP, cx.ident_of("i32")),
        EfiType::UInt32 => cx.ty_ident(DSP, cx.ident_of("u32")),
        EfiType::Int64 => cx.ty_ident(DSP, cx.ident_of("i64")),
        EfiType::UInt64 => cx.ty_ident(DSP, cx.ident_of("u64")),
        EfiType::Char8 => cx.ty_ident(DSP, cx.ident_of("u8")),
        EfiType::Char16 => cx.ty_ident(DSP, cx.ident_of("u16")),
        EfiType::Id(ref name) => cx.ty_ident(DSP, cx.ident_of(&gen_eficc_name(&name))),
        EfiType::Ptr(ref ty) => {
            let mutbl = match dir {
                Some(EfiArgDir::In) => Mutability::Immutable,
                Some(EfiArgDir::Out) |
                None => Mutability::Mutable,
            };
            cx.ty_ptr(DSP, gen_type(cx, &*ty, dir), mutbl)
        }
    }
}

fn gen_field(cx: &ExtCtxt, field: &EfiField) -> StructField {
    StructField {
        span: DSP,
        ident: Some(cx.ident_of(&gen_efisc_name(&field.name))),
        vis: Visibility::Public,
        id: DID,
        ty: gen_type(cx, &field.ty, None),
        attrs: Vec::new(),
    }
}

fn gen_record(cx: &ExtCtxt, record: &EfiRecord) -> P<Item> {
    let fields = VariantData::Struct(record.fields
                                         .iter()
                                         .map(|f| gen_field(cx, f))
                                         .collect(),
                                     DID);

    P(Item {
        ident: cx.ident_of(&gen_eficc_name(&record.name)),
        attrs: vec![gen_repr_c(cx),
                    match record.kind {
                        EfiRecordKind::Struct => gen_derive(cx, &["Copy", "Clone", "Debug"]),
                        EfiRecordKind::Union => gen_derive(cx, &["Copy", "Clone"]),
                    }],
        id: DID,
        node: match record.kind {
            EfiRecordKind::Struct => ItemKind::Struct(fields, Generics::default()),
            EfiRecordKind::Union => ItemKind::Union(fields, Generics::default()),
        },
        vis: Visibility::Public,
        span: DSP,
    })
}

fn gen_enum(cx: &ExtCtxt, enm: &EfiEnum) -> P<Item> {
    let gen_lit = |v| cx.expr_lit(DSP, LitKind::Int(v, LitIntType::Unsuffixed));

    let vs = enm.fields
        .iter()
        .map(|f| {
            let mut v = cx.variant(DSP, cx.ident_of(&f.name), vec![]);
            v.node.disr_expr = f.value.map(&gen_lit);
            v
        })
        .collect();

    cx.item_enum(DSP, cx.ident_of(&enm.name), EnumDef { variants: vs })
        .map(|mut e| {
            e.attrs = vec![gen_repr_c(cx), gen_derive(cx, &["Copy", "Clone", "Debug"])];
            e.vis = Visibility::Public;
            e
        })
}

fn gen_bare_method(cx: &ExtCtxt, method: &EfiMethod) -> StructField {
    let args = method.args
        .iter()
        .map(|arg| {
            cx.arg(DSP,
                   cx.ident_of(&gen_efisc_name(&arg.name)),
                   gen_type(cx, &arg.ty, Some(arg.dir)))
        })
        .collect();

    let ty = Ty {
        id: DID,
        node: TyKind::BareFn(P(BareFnTy {
            unsafety: Unsafety::Unsafe,
            abi: Abi::Win64,
            lifetimes: vec![],
            decl: cx.fn_decl(args, gen_type(cx, &method.ty, None)),
        })),
        span: DSP,
    };

    StructField {
        span: DSP,
        ident: Some(cx.ident_of(&gen_efisc_name(&method.name))),
        vis: Visibility::Public,
        id: DID,
        ty: P(ty),
        attrs: Vec::new(),
    }
}

// FIXME: method/field order
fn gen_bare_protocol(cx: &ExtCtxt, proto: &EfiProtocol) -> P<Item> {
    let methods = proto.methods.iter().map(|m| gen_bare_method(cx, m));
    let fields = proto.fields.iter().map(|f| gen_field(cx, f));
    let entries = methods.chain(fields).map(|mut f| {
        f.vis = Visibility::Inherited;
        f
    });

    cx.item_struct(DSP,
                     cx.ident_of(&gen_eficc_name(&proto.name)),
                     VariantData::Struct(entries.collect(), DID))
        .map(|mut s| {
            s.attrs = vec![gen_repr_c(cx)];
            s.vis = Visibility::Public;
            s
        })
}

fn gen_method_sig(cx: &ExtCtxt, method: &EfiMethod) -> MethodSig {
    let args = method.args
        .iter()
        .map(|a| {
            if a.name == "This" {
                cx.arg(DSP, cx.ident_of("self"), cx.ty(DSP, TyKind::ImplicitSelf))

            } else {
                cx.arg(DSP,
                       cx.ident_of(&gen_efisc_name(&a.name)),
                       gen_type(cx, &a.ty, Some(a.dir)))
            }
        })
        .collect();

    MethodSig {
        unsafety: Unsafety::Normal,
        constness: dummy_spanned(Constness::NotConst),
        abi: Abi::Rust,
        decl: cx.fn_decl(args, gen_type(cx, &method.ty, None)),
        generics: Generics::default(),
    }
}

fn gen_getter_sig(cx: &ExtCtxt, field: &EfiField) -> MethodSig {
    let name = fresh_name(cx.ident_of("'a"));
    let mut lifetime = Generics::default();
    lifetime.lifetimes = vec![cx.lifetime_def(DSP, name, vec![])];

    let ty = match field.ty {
        EfiType::Ptr(ref ty) => {
            cx.ty_rptr(DSP,
                       gen_type(cx, ty, Some(EfiArgDir::In)),
                       Some(cx.lifetime(DSP, name)),
                       Mutability::Immutable)
        }
        ref ty => gen_type(cx, ty, Some(EfiArgDir::In)),
    };

    MethodSig {
        unsafety: Unsafety::Normal,
        constness: dummy_spanned(Constness::NotConst),
        abi: Abi::Rust,
        decl: cx.fn_decl(vec![cx.arg(DSP, cx.ident_of("self"), cx.ty(DSP, TyKind::ImplicitSelf))],
                         ty),
        generics: lifetime,
    }
}

fn gen_protocol_trait(cx: &ExtCtxt, proto: &EfiProtocol) -> P<Item> {
    let methods = proto.methods
        .iter()
        .map(|m| {
            TraitItem {
                id: DID,
                ident: cx.ident_of(&gen_efisc_name(&m.name)),
                attrs: vec![],
                node: TraitItemKind::Method(gen_method_sig(cx, m), None),
                span: DSP,
            }
        });

    let accessors = proto.fields
        .iter()
        .map(|f| {
            TraitItem {
                id: DID,
                ident: cx.ident_of(&gen_efisc_name(&f.name)),
                attrs: vec![],
                node: TraitItemKind::Method(gen_getter_sig(cx, f), None),
                span: DSP,
            }
        });

    let trt = ItemKind::Trait(Unsafety::Normal,
                              Generics::default(),
                              P::new(),
                              methods.chain(accessors).collect());

    cx.item(DSP, cx.ident_of(&gen_trait_name(&proto.name)), vec![], trt)
        .map(|mut i| {
            i.vis = Visibility::Public;
            i
        })
}

fn gen_method_block(cx: &ExtCtxt, method: &EfiMethod) -> P<Block> {
    let paren = |expr| cx.expr(DSP, ExprKind::Paren(expr));

    let args = method.args
        .iter()
        .map(|a| if a.name == "This" {
            cx.expr_self(DSP)
        } else {
            cx.expr_ident(DSP, cx.ident_of(&gen_efisc_name(&a.name)))
        })
        .collect();

    let field = cx.expr_field_access(DSP,
                                     paren(cx.expr_deref(DSP, cx.expr_self(DSP))),
                                     cx.ident_of(&gen_efisc_name(&method.name)));

    let call = cx.expr_call(DSP, paren(field), args);

    cx.block_expr(cx.expr_block(P(Block {
        stmts: vec![cx.stmt_expr(call)],
        id: DID,
        rules: BlockCheckMode::Unsafe(UnsafeSource::CompilerGenerated),
        span: DSP,
    })))
}

fn gen_getter_block(cx: &ExtCtxt, field: &EfiField) -> P<Block> {
    let paren = |expr| cx.expr(DSP, ExprKind::Paren(expr));

    let field = cx.expr_field_access(DSP,
                                     paren(cx.expr_deref(DSP, cx.expr_self(DSP))),
                                     cx.ident_of(&gen_efisc_name(&field.name)));

    let deref = cx.expr_addr_of(DSP, cx.expr_deref(DSP, field));


    cx.block_expr(cx.expr_block(P(Block {
        stmts: vec![cx.stmt_expr(deref)],
        id: DID,
        rules: BlockCheckMode::Unsafe(UnsafeSource::CompilerGenerated),
        span: DSP,
    })))
}

fn gen_protocol_impl(cx: &ExtCtxt, proto: &EfiProtocol) -> P<Item> {
    let methods = proto.methods
        .iter()
        .map(|m| {
            ImplItem {
                id: DID,
                ident: cx.ident_of(&gen_efisc_name(&m.name)),
                vis: Visibility::Inherited,
                defaultness: Defaultness::Final,
                attrs: vec![],
                node: ImplItemKind::Method(gen_method_sig(cx, m), gen_method_block(cx, m)),
                span: DSP,
            }
        });

    let accessors = proto.fields
        .iter()
        .map(|f| {
            ImplItem {
                id: DID,
                ident: cx.ident_of(&gen_efisc_name(&f.name)),
                vis: Visibility::Inherited,
                defaultness: Defaultness::Final,
                attrs: vec![],
                node: ImplItemKind::Method(gen_getter_sig(cx, f), gen_getter_block(cx, f)),
                span: DSP,
            }
        });

    let trait_ident = cx.ident_of(&gen_trait_name(&proto.name));
    let proto_ident = cx.ident_of(&gen_eficc_name(&proto.name));
    let ty = cx.ty_ptr(DSP, cx.ty_ident(DSP, proto_ident), Mutability::Immutable);
    let imp = ItemKind::Impl(Unsafety::Normal,
                             ImplPolarity::Positive,
                             Generics::default(),
                             Some(cx.trait_ref(cx.path_ident(DSP, trait_ident))),
                             ty,
                             methods.chain(accessors).collect());

    cx.item(DSP, trait_ident, vec![], imp)
}

pub fn gen_module(module: &EfiModule) -> io::Result<()> {
    let sess = &ParseSess::new();
    let mut ml = DummyResolver;
    let cx = ExtCtxt::new(sess,
                          vec![],
                          ExpansionConfig::default("efi".into()),
                          &mut ml);

    let records = module.records.iter().map(|r| gen_record(&cx, r));
    let enums = module.enums.iter().map(|r| gen_enum(&cx, r));
    let protos = module.protocols.iter().map(|p| gen_bare_protocol(&cx, p));
    let traits = module.protocols.iter().map(|p| gen_protocol_trait(&cx, p));
    let impls = module.protocols.iter().map(|p| gen_protocol_impl(&cx, p));
    let module = Mod {
        inner: DSP,
        items: records.chain(enums).chain(protos).chain(traits).chain(impls).collect(),
    };

    let mut ps = pprust::rust_printer(Box::from(io::stdout()));
    ps.s = pp::mk_printer(Box::from(io::stdout()), 100);
    try!(ps.print_mod(&module, &[]));
    try!(ps.print_remaining_comments());
    try!(pp::eof(&mut ps.s));
    try!(ps.s.out.flush());
    Ok(())
}
