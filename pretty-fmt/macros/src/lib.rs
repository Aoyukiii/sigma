use proc_macro::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Attribute, Data, DataEnum, DeriveInput, Expr, ExprLit, Fields, Ident, Lit, Variant,
    parse_macro_input,
};

#[proc_macro_derive(PrettyFmt, attributes(pretty_fmt))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_ident = &ast.ident;

    let data = match &ast.data {
        Data::Enum(data) => data,
        _ => {
            return syn::Error::new_spanned(
                &struct_ident,
                "`PrettyFmt` can only be used on enums for now",
            )
            .to_compile_error()
            .into();
        }
    };

    match generate_for_enum(data) {
        Ok(arms) => quote! {
            impl pretty_fmt::PrettyFmt for #struct_ident {
                fn pretty_fmt_with_ctx(
                    &self,
                    ctx: &mut pretty_fmt::PrettyContext,
                    f: &mut std::fmt::Formatter,
                ) -> std::fmt::Result {
                    match &self {
                        #(#arms),*
                    }
                }
            }
        }
        .into(),
        Err(e) => e.to_compile_error().into(),
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
    let fmt_attr = v
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("pretty_fmt"));

    match fields {
        Fields::Unnamed(f) => {
            let len = f.unnamed.len();
            let arg_idents = (0..len)
                .map(|i| Ident::new(&format!("arg{}", i), proc_macro2::Span::call_site()))
                .collect::<Vec<_>>();

            let arm = match fmt_attr {
                Some(fmt_attr) => quote! {
                    Self::#ident(#(#arg_idents),*) => todo!()
                },
                None => {
                    let fields: Vec<_> = arg_idents
                        .iter()
                        .enumerate()
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
        Fields::Named(_) => todo!(),
        Fields::Unit => {
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
