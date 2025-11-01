use proc_macro::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Ident,
    Variant, parse_macro_input,
};

#[proc_macro_derive(PrettyFmt, attributes(pretty_fmt, skip, header))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match generate_from_input(&input) {
        Ok(s) => s.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

struct FmtConfig {
    skip: bool,
    custom_fmt: Option<proc_macro2::TokenStream>,
    header_fmt: Option<proc_macro2::TokenStream>,
}

impl FmtConfig {
    fn from_attrs(attrs: &Vec<Attribute>) -> syn::Result<Self> {
        let skip = find_attr(attrs, "skip").is_some();
        let fmt_attr = find_attr(attrs, "pretty_fmt");
        let custom_fmt = match fmt_attr {
            Some(attr) => Some(attr.parse_args::<proc_macro2::TokenStream>()?),
            None => None,
        };
        let header_attr = find_attr(attrs, "header");
        let header_fmt = match header_attr {
            Some(attr) => Some(attr.parse_args::<proc_macro2::TokenStream>()?),
            None => None,
        };
        let custom_fmt = custom_fmt.map(|f| quote! { format!(#f) });
        let header_fmt = header_fmt.map(|f| quote! { format!(#f) });
        Ok(Self {
            skip,
            custom_fmt,
            header_fmt,
        })
    }
}

fn find_attr<'a, 'b>(attrs: &'a Vec<Attribute>, name: &'b str) -> Option<&'a Attribute> {
    attrs.iter().find(|attr| attr.path().is_ident(name))
}

enum FieldConfigs {
    Unit,
    Named(Vec<(Ident, FmtConfig)>),
    Unnamed(Vec<(usize, FmtConfig)>),
}

impl FieldConfigs {
    fn from_fields(fields: &Fields) -> syn::Result<Self> {
        match fields {
            Fields::Unit => Ok(Self::Unit),
            Fields::Named(FieldsNamed { named, .. }) => {
                let mut result = Vec::with_capacity(named.len());
                for field in named {
                    let config = FmtConfig::from_attrs(&field.attrs)?;
                    result.push((field.ident.clone().unwrap(), config));
                }
                Ok(Self::Named(result))
            }
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                let mut result = Vec::with_capacity(unnamed.len());
                for (i, field) in unnamed.iter().enumerate() {
                    let config = FmtConfig::from_attrs(&field.attrs)?;
                    result.push((i, config));
                }
                Ok(Self::Unnamed(result))
            }
        }
    }
}

fn make_ident(name: &str) -> Ident {
    Ident::new(name, proc_macro2::Span::call_site())
}

fn generate_from_input(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let attrs = &input.attrs;
    match &input.data {
        Data::Enum(data) => {
            let body = generate_body_for_enum(data)?;
            Ok(generate_impl(struct_ident, body))
        }
        Data::Struct(data) => {
            let body = generate_body_for_struct(struct_ident, data, attrs)?;
            Ok(generate_impl(struct_ident, body))
        }
        _ => Err(syn::Error::new_spanned(
            struct_ident,
            "`PrettyFmt` can only be used on enums or structs",
        )),
    }
}

fn generate_impl(type_ident: &Ident, body: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! {
        impl pretty_fmt::PrettyFmt for #type_ident {
            fn pretty_fmt_with_ctx(
                &self,
                ctx: &pretty_fmt::PrettyContext,
                f: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                #body
            }
        }
    }
}

fn generate_body_for_enum(data: &DataEnum) -> syn::Result<proc_macro2::TokenStream> {
    let arms = data
        .variants
        .iter()
        .map(|v| generate_arm(v))
        .collect::<Result<Vec<_>, _>>()?;
    let body = quote! {
        match self {
            #(#arms),*
        }
    };
    Ok(body)
}

fn generate_body_for_struct(
    struct_ident: &Ident,
    data: &DataStruct,
    attrs: &Vec<Attribute>,
) -> syn::Result<proc_macro2::TokenStream> {
    use FieldConfigs::*;
    let configs = FieldConfigs::from_fields(&data.fields)?;
    let config = FmtConfig::from_attrs(attrs)?;

    let custom_fmt = config.custom_fmt;
    let header_fmt = config.header_fmt;

    if custom_fmt.is_some() && header_fmt.is_some() {
        return Err(syn::Error::new_spanned(
            struct_ident.to_token_stream(),
            "Attributes `pretty_fmt` and `header` cannot be used simultaneously",
        ));
    }

    let struct_name = struct_ident.to_string();

    if let Unit = configs {
        if header_fmt.is_some() {
            return Err(syn::Error::new_spanned(
                struct_ident.to_token_stream(),
                "Attribute `header` cannot be used on the top of structs",
            ));
        }
        let custom_fmt = custom_fmt.unwrap_or(quote! { #struct_name });
        let body = quote! {
            write!(f, #custom_fmt)
        };
        return Ok(body);
    }

    let let_stmts: Vec<proc_macro2::TokenStream> = match &configs {
        Unit => unreachable!(),
        Named(it) => it
            .iter()
            .map(|(ident, _)| {
                quote! { let #ident = &self.#ident; }
            })
            .collect(),
        Unnamed(it) => it
            .iter()
            .map(|(idx, _)| {
                let ident = make_ident(&format!("arg{}", idx));
                quote! { let #ident = &self.#idx; }
            })
            .collect(),
    };

    let field_chains: Vec<proc_macro2::TokenStream> = match &configs {
        Unit => unreachable!(),
        Named(it) => it
            .iter()
            .filter_map(|(ident, config)| {
                if config.skip {
                    None
                } else {
                    let ident_name = ident.to_string();
                    Some(quote! { .field(#ident_name, #ident)? })
                }
            })
            .collect(),
        Unnamed(it) => it
            .iter()
            .filter_map(|(idx, config)| {
                if config.skip {
                    None
                } else {
                    let idx_name = idx.to_string();
                    Some(quote! { .field(#idx_name, #idx)? })
                }
            })
            .collect(),
    };

    let header_fmt = header_fmt.unwrap_or(quote! { #struct_name });
    let ret = quote! {
        pretty_fmt::NodeFormatter::new(ctx, f)
            .header(&#header_fmt)?
            #(#field_chains)*
            .finish()
    };
    let ret = custom_fmt
        .map(|custom_fmt| quote! { write!(f, "{}", #custom_fmt) })
        .unwrap_or(ret);
    let body = quote! {
        #(#let_stmts)*
        #ret
    };

    eprintln!("{body}");

    Ok(body)
}

fn generate_arm(variant: &Variant) -> syn::Result<proc_macro2::TokenStream> {
    use FieldConfigs::*;

    let configs = FieldConfigs::from_fields(&variant.fields)?;
    let variant_ident = &variant.ident;
    let variant_name = variant_ident.to_string();

    let variant_config = FmtConfig::from_attrs(&variant.attrs)?;

    let custom_fmt = variant_config.custom_fmt;
    let header_fmt = variant_config.header_fmt;

    if custom_fmt.is_some() && header_fmt.is_some() {
        return Err(syn::Error::new_spanned(
            variant.to_token_stream(),
            "Attributes `pretty_fmt` and `header` cannot be used simultaneously",
        ));
    }

    if let Unit = configs {
        if header_fmt.is_some() {
            return Err(syn::Error::new_spanned(
                variant.to_token_stream(),
                "Attribute `header` cannot be used on unit variants",
            ));
        }
        let custom_fmt = custom_fmt.unwrap_or(quote! { #variant_name });
        let body = quote! {
            Self::#variant_ident => write!(f, "{}", #custom_fmt)
        };
        return Ok(body);
    }

    let unpacking = match &configs {
        Unit => unreachable!(),
        Named(it) => {
            let field_patterns: Vec<proc_macro2::TokenStream> = it
                .iter()
                .map(|(field_ident, _)| {
                    quote! { #field_ident }
                })
                .collect();
            quote! { {#(#field_patterns),*} }
        }
        Unnamed(it) => {
            let field_patterns: Vec<proc_macro2::TokenStream> = it
                .iter()
                .map(|(i, _)| {
                    let ident = make_ident(&format!("arg{i}"));
                    quote! { #ident }
                })
                .collect();
            quote! { (#(#field_patterns),*) }
        }
    };

    if let Some(custom_fmt) = custom_fmt {
        let arm = quote! {
            Self::#variant_ident #unpacking => write!(f, "{}", #custom_fmt)
        };
        eprintln!("{arm}");
        return Ok(arm);
    }

    let ret = {
        let field_chains: Vec<proc_macro2::TokenStream> = match &configs {
            Unit => unreachable!(),
            Named(it) => it
                .iter()
                .filter(|(_, config)| !config.skip)
                .map(|(field_ident, _)| {
                    let field_name = field_ident.to_string();
                    quote! {
                        .field(#field_name, #field_ident)?
                    }
                })
                .collect(),
            Unnamed(it) => it
                .iter()
                .filter(|(_, config)| !config.skip)
                .map(|(i, _)| {
                    let is = i.to_string();
                    let ident = make_ident(&format!("arg{i}"));
                    quote! {
                        .field(#is, #ident)?
                    }
                })
                .collect(),
        };
        let header_fmt = header_fmt.unwrap_or(quote! { #variant_name });
        quote! {
            pretty_fmt::NodeFormatter::new(ctx, f)
                .header(&#header_fmt)?
                #(#field_chains)*
                .finish()
        }
    };

    let arm = quote! {
        Self::#variant_ident #unpacking => #ret
    };
    Ok(arm)
}
