use proc_macro::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Attribute, Data, DataEnum, DeriveInput, Expr, ExprLit, Fields, Ident, Lit, Variant,
    parse_macro_input,
};

#[proc_macro_derive(PrettyFmt, attributes(pretty_fmt, skip))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match generate_from_input(&input) {
        Ok(s) => s.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn generate_from_input(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    match &input.data {
        Data::Enum(data) => {
            let arms = generate_for_enum(data)?;
            let body = quote! {
                match self {
                    #(#arms),*
                }
            };
            Ok(generate_impl(struct_ident, body))
        }
        Data::Struct(data) => {
            let body = quote! {};
            Ok(generate_impl(struct_ident, body))
        }
        _ => Err(syn::Error::new_spanned(
            struct_ident,
            "`PrettyFmt` can only be used on enums for now",
        )),
    }
}

fn generate_impl(type_ident: &Ident, body: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! {
        impl pretty_fmt::PrettyFmt for #type_ident {
            fn pretty_fmt_with_ctx(
                &self,
                ctx: &mut pretty_fmt::PrettyContext,
                f: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                #body
            }
        }
    }
}

fn generate_for_enum(data: &DataEnum) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let v: Result<Vec<_>, _> = data
        .variants
        .iter()
        .map(|v| generate_match_arm(v))
        .collect();
    let v = v?;
    Ok(v)
}

fn generate_match_arm(v: &Variant) -> syn::Result<proc_macro2::TokenStream> {
    let ident = &v.ident;
    let fields = &v.fields;
    let fmt_attr = get_attr(&v.attrs, "pretty_fmt");

    match fields {
        Fields::Unnamed(f) => {
            // fields like `Some(T)`
            let arg_idents = f
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    if let Some(_) = get_attr(&f.attrs, "skip") {
                        Ident::new("_", proc_macro2::Span::call_site())
                    } else {
                        Ident::new(&format!("arg{}", i), proc_macro2::Span::call_site())
                    }
                })
                .collect::<Vec<_>>();

            let arm = match fmt_attr {
                Some(fmt_attr) => {
                    quote! {
                        Self::#ident(#(#arg_idents),*) => todo!()
                    }
                }
                None => {
                    let fields: Vec<_> = arg_idents
                        .iter()
                        .enumerate()
                        .filter(|(_, ident)| ident.to_string() != "_")
                        .map(|(i, ident)| {
                            let i = i.to_string();
                            quote! {
                                .field(#i, #ident)?
                            }
                        })
                        .collect();
                    let ident_str = ident.to_string();
                    quote! {
                        Self::#ident(#(#arg_idents),*) => pretty_fmt::NodeFormatter::new(ctx, f)
                            .header(#ident_str)?
                            #(#fields)*
                            .finish()
                    }
                }
            };
            Ok(arm)
        }
        Fields::Named(f) => {
            // fields like `Point { x: f64, y: f64 }`

            let arg_idents_with_skip = f
                .named
                .iter()
                .map(|f| {
                    (
                        f.ident.clone().unwrap(),
                        get_attr(&f.attrs, "skip").is_some(),
                    )
                })
                .collect::<Vec<_>>();

            let field_defs = arg_idents_with_skip
                .iter()
                .map(|(ident, skip)| {
                    if *skip {
                        quote! { #ident: _ }
                    } else {
                        quote! { #ident }
                    }
                })
                .collect::<Vec<_>>();

            let arm = match fmt_attr {
                Some(fmt_attr) => {
                    quote! {
                        Self::#ident { #(#field_defs),* } => todo!()
                    }
                }
                None => {
                    let fields: Vec<_> = arg_idents_with_skip
                        .iter()
                        .filter(|(_, skip)| !*skip)
                        .map(|(ident, _)| {
                            let ident_name = ident.to_string();
                            quote! {
                                .field(#ident_name, #ident)?
                            }
                        })
                        .collect();
                    let ident_str = ident.to_string();
                    quote! {
                        Self::#ident { #(#field_defs),* } => pretty_fmt::NodeFormatter::new(ctx, f)
                            .header(#ident_str)?
                            #(#fields)*
                            .finish()
                    }
                }
            };
            Ok(arm)
        }
        Fields::Unit => {
            // fields like `None`
            let fmt_str = match fmt_attr {
                Some(fmt_attr) => generate_fmt_str(fmt_attr)?,
                None => ident.to_string(),
            };
            let arm = quote! {
                Self::#ident => write!(f, #fmt_str)
            };
            Ok(arm)
        }
    }
}

fn generate_fmt_str(fmt_attr: &Attribute) -> syn::Result<String> {
    let fmt_args = fmt_attr.parse_args()?;
    let fmt_str = if let Expr::Lit(ExprLit {
        lit: Lit::Str(s), ..
    }) = fmt_args
    {
        s.value()
    } else {
        return Err(syn::Error::new_spanned(
            fmt_args.to_token_stream(),
            "Only string literal is supported",
        ));
    };
    Ok(fmt_str)
}

fn get_attr<'a, 'b>(attrs: &'a Vec<Attribute>, name: &'b str) -> Option<&'a Attribute> {
    attrs.iter().find(|attr| attr.path().is_ident(name))
}
