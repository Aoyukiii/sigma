use proc_macro::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Attribute, Data, DataEnum, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Ident, Index,
    Variant, parse_macro_input,
};

mod attr_name {
    pub const SKIP: &str = "skip";
    pub const IMPL_DISPLAY: &str = "impl_display";
    pub const PRETTY_FMT: &str = "pretty_fmt";
    pub const HEADER: &str = "header";
}

#[proc_macro_derive(PrettyFmt, attributes(pretty_fmt, skip, header, impl_display))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match generate_from_input(&input) {
        Ok(s) => s.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

struct FieldConfig {
    skip: bool,
    impl_display: bool,
    custom_fmt: Option<proc_macro2::TokenStream>,
    header_fmt: Option<proc_macro2::TokenStream>,
}

impl FieldConfig {
    fn from_attrs(attrs: &Vec<Attribute>) -> syn::Result<Self> {
        use attr_name::*;
        let skip = find_attr(attrs, SKIP).is_some();
        let impl_display = find_attr(attrs, IMPL_DISPLAY).is_some();
        let fmt_attr = find_attr(attrs, PRETTY_FMT);
        let custom_fmt = match fmt_attr {
            Some(attr) => Some(attr.parse_args::<proc_macro2::TokenStream>()?),
            None => None,
        };
        let header_attr = find_attr(attrs, HEADER);
        let header_fmt = match header_attr {
            Some(attr) => Some(attr.parse_args::<proc_macro2::TokenStream>()?),
            None => None,
        };
        let custom_fmt = custom_fmt.map(|f| quote! { format!(#f) });
        let header_fmt = header_fmt.map(|f| quote! { format!(#f) });
        Ok(Self {
            skip,
            impl_display,
            custom_fmt,
            header_fmt,
        })
    }

    fn has_custom_and_header(&self) -> bool {
        self.custom_fmt.is_some() && self.header_fmt.is_some()
    }

    fn get_fmt(&self) -> Option<&proc_macro2::TokenStream> {
        self.custom_fmt.as_ref().or(self.header_fmt.as_ref())
    }
}

fn find_attr<'a, 'b>(attrs: &'a Vec<Attribute>, name: &'b str) -> Option<&'a Attribute> {
    attrs.iter().find(|attr| attr.path().is_ident(name))
}

enum StructConfigs {
    Unit,
    Named(Vec<(Ident, FieldConfig)>),
    Unnamed(Vec<(usize, FieldConfig)>),
}

impl StructConfigs {
    fn from_fields(fields: &Fields) -> syn::Result<Self> {
        match fields {
            Fields::Unit => Ok(Self::Unit),
            Fields::Named(FieldsNamed { named, .. }) => {
                let mut result = Vec::with_capacity(named.len());
                for field in named {
                    let config = FieldConfig::from_attrs(&field.attrs)?;
                    result.push((field.ident.clone().unwrap(), config));
                }
                Ok(Self::Named(result))
            }
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                let mut result = Vec::with_capacity(unnamed.len());
                for (i, field) in unnamed.iter().enumerate() {
                    let config = FieldConfig::from_attrs(&field.attrs)?;
                    result.push((i, config));
                }
                Ok(Self::Unnamed(result))
            }
        }
    }

    fn make_unpacking(&self) -> proc_macro2::TokenStream {
        match self {
            Self::Unit => quote! {},
            Self::Named(it) => {
                let field_patterns: Vec<proc_macro2::TokenStream> = it
                    .iter()
                    .map(|(ident, _)| {
                        quote! { #ident }
                    })
                    .collect();
                quote! { {#(#field_patterns),*} }
            }
            Self::Unnamed(it) => {
                let field_patterns: Vec<proc_macro2::TokenStream> = it
                    .iter()
                    .map(|(i, _)| {
                        let ident = make_idx_ident(*i);
                        quote! { #ident }
                    })
                    .collect();
                quote! { (#(#field_patterns),*) }
            }
        }
    }

    fn make_let_stmts(&self, fmt: Option<&proc_macro2::TokenStream>) -> proc_macro2::TokenStream {
        let idents = match fmt {
            Some(fmt) => collect_idents(fmt),
            None => vec![],
        };
        let let_stmts: Vec<proc_macro2::TokenStream> = match self {
            Self::Unit => vec![],
            Self::Named(it) => it
                .iter()
                .filter_map(|(ident, config)| {
                    if config.skip && !idents.contains(ident) {
                        None
                    } else {
                        Some(quote! { let #ident = &self.#ident; })
                    }
                })
                .collect(),
            Self::Unnamed(it) => it
                .iter()
                .filter_map(|(i, config)| {
                    let ident = make_idx_ident(*i);
                    if config.skip && !idents.contains(&ident) {
                        None
                    } else {
                        let index = Index::from(*i);
                        Some(quote! { let #ident = &self.#index; })
                    }
                })
                .collect(),
        };
        quote! { #(#let_stmts)* }
    }
}

fn make_idx_ident(idx: usize) -> Ident {
    make_ident(&format!("arg{idx}"))
}

fn make_ident(name: &str) -> Ident {
    Ident::new(name, proc_macro2::Span::call_site())
}

fn collect_idents(tokens: &proc_macro2::TokenStream) -> Vec<Ident> {
    let mut idents = Vec::new();
    collect_idents_recursive(tokens, &mut idents);
    idents
}

fn collect_idents_recursive(tokens: &proc_macro2::TokenStream, idents: &mut Vec<Ident>) {
    for token in tokens.clone().into_iter() {
        match token {
            proc_macro2::TokenTree::Ident(ident) => idents.push(ident),
            proc_macro2::TokenTree::Group(group) => {
                collect_idents_recursive(&group.stream(), idents)
            }
            _ => {}
        }
    }
}

fn generate_from_input(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let attrs = &input.attrs;
    let impl_display = FieldConfig::from_attrs(&input.attrs)?.impl_display;
    match &input.data {
        Data::Enum(data) => {
            let body = generate_body_for_enum(data)?;
            Ok(generate_impl(struct_ident, body))
        }
        Data::Struct(data) => {
            let body = generate_body_for_struct(struct_ident, &data.fields, attrs)?;
            Ok(generate_impl(struct_ident, body))
        }
        _ => Err(syn::Error::new_spanned(
            struct_ident,
            "`PrettyFmt` can only be used on enums or structs",
        )),
    }
    .map(|impl_pretty_fmt| {
        if impl_display {
            quote! {
                pretty_fmt::impl_display_for_pretty_fmt!(#struct_ident);
                #impl_pretty_fmt
            }
        } else {
            quote! {
                #impl_pretty_fmt
            }
        }
    })
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
    if data.variants.is_empty() {
        let body = quote! { Ok(()) };
        return Ok(body);
    }
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

fn validate_config<T: ToTokens>(
    configs: &StructConfigs,
    top_config: &FieldConfig,
    report_tokens: T,
) -> syn::Result<()> {
    if top_config.has_custom_and_header() {
        return Err(syn::Error::new_spanned(
            report_tokens,
            "Attributes `pretty_fmt` and `header` cannot be used simultaneously",
        ));
    }

    if let StructConfigs::Unit = configs {
        if top_config.header_fmt.is_some() {
            return Err(syn::Error::new_spanned(
                report_tokens,
                "Attribute `header` cannot be used on the top of empty structs",
            ));
        }
    }

    Ok(())
}

fn generate_body_for_struct(
    struct_ident: &Ident,
    fields: &Fields,
    attrs: &Vec<Attribute>,
) -> syn::Result<proc_macro2::TokenStream> {
    let configs = StructConfigs::from_fields(fields)?;
    let struct_config = FieldConfig::from_attrs(attrs)?;

    validate_config(&configs, &struct_config, struct_ident)?;

    let struct_name = struct_ident.to_string();

    let fmt = struct_config.get_fmt();
    let let_stmts = configs.make_let_stmts(fmt);
    let ret = generate_ret_expr(configs, struct_config, struct_name)?;
    let body = quote! {
        #let_stmts
        #ret
    };
    Ok(body)
}

fn generate_arm(variant: &Variant) -> syn::Result<proc_macro2::TokenStream> {
    let variant_ident = &variant.ident;

    let configs = StructConfigs::from_fields(&variant.fields)?;
    let variant_config = FieldConfig::from_attrs(&variant.attrs)?;

    validate_config(&configs, &variant_config, variant_ident)?;

    let variant_name = variant_ident.to_string();

    let unpacking = configs.make_unpacking();
    let ret = generate_ret_expr(configs, variant_config, variant_name)?;
    let arm = quote! {
        Self::#variant_ident #unpacking => #ret
    };
    Ok(arm)
}

fn generate_ret_expr(
    configs: StructConfigs,
    top_config: FieldConfig,
    header_fallback: String,
) -> syn::Result<proc_macro2::TokenStream> {
    use StructConfigs::*;

    let custom_fmt = top_config.custom_fmt;
    let header_fmt = top_config.header_fmt;

    if let Some(custom_fmt) = custom_fmt {
        return Ok(quote! { write!(f, "{}", #custom_fmt) });
    }

    let header_fmt = header_fmt.unwrap_or(quote! { #header_fallback });

    let field_chains: Vec<proc_macro2::TokenStream> = match &configs {
        Unit => vec![],
        Named(it) => it
            .iter()
            .filter_map(|(field_ident, config)| {
                if config.skip {
                    None
                } else {
                    let field_name = field_ident.to_string();
                    Some(quote! { .field(#field_name, #field_ident)? })
                }
            })
            .collect(),
        Unnamed(it) => it
            .iter()
            .filter_map(|(idx, config)| {
                if config.skip {
                    None
                } else {
                    let is = idx.to_string();
                    let ident = make_idx_ident(*idx);
                    Some(quote! { .field(#is, #ident)? })
                }
            })
            .collect(),
    };

    let ret = quote! {
        pretty_fmt::NodeFormatter::new(ctx, f)
            .header(&#header_fmt)?
            #(#field_chains)*
            .finish()
    };

    Ok(ret)
}
