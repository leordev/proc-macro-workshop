extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::PathArguments::AngleBracketed;
use syn::Type::Path;
use syn::{parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Lit, Meta, NestedMeta};

struct BuilderDerive {
    option_fields: proc_macro2::TokenStream,
    empty_fields: proc_macro2::TokenStream,
    fields_methods: proc_macro2::TokenStream,
    builder_function: proc_macro2::TokenStream,
    extra_fields: proc_macro2::TokenStream,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    // eprintln!("INPUT: {:#?}", input);

    let ident = input.ident;
    let ident_builder = format_ident!("{}Builder", ident);
    let BuilderDerive {
        option_fields,
        empty_fields,
        fields_methods,
        builder_function,
        extra_fields,
    } = generate_builder_derive(&ident, &input.data);

    let expanded = quote! {
        pub struct #ident_builder {
            #option_fields
        }

        impl #ident_builder {
            #fields_methods
            #extra_fields
            #builder_function
        }

        impl #ident {
            pub fn builder() -> #ident_builder {
                #ident_builder {
                    #empty_fields
                }
            }
        }
    };

    // eprintln!("TOKENS:\n{}", expanded);
    TokenStream::from(expanded)
}

fn generate_builder_derive(ident: &proc_macro2::Ident, data: &Data) -> BuilderDerive {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let mut options = Vec::new();
                let mut empties = Vec::new();
                let mut methods = Vec::new();
                let mut fields_checker = Vec::new();
                let mut fields_assign = Vec::new();
                let mut extra_fields = Vec::new();

                for f in &fields.named {
                    let name = &f.ident;
                    let ty = &f.ty;

                    empties.push(quote_spanned!(f.span()=> #name: None));
                    fields_assign.push(quote_spanned!(f.span()=> #name));

                    let optional_field = is_optional(&ty);
                    let (converted_field_type, setter_type, checker) = if optional_field {
                        let optional_inner_type = get_inner_type(&ty);
                        (
                            quote!(#ty),
                            quote!(#optional_inner_type),
                            quote_spanned!(f.span()=> let #name = self.#name.take();),
                        )
                    } else {
                        (
                            quote!(Option<#ty>),
                            quote!(#ty),
                            quote_spanned! {f.span()=>
                                let #name = match self.#name.take() {
                                    Some(value) => value,
                                    None => return Err(From::from(format!("field {} is empty", stringify!(#name)))),
                                };
                            },
                        )
                    };

                    options.push(quote_spanned!(f.span()=> #name: #converted_field_type));

                    methods.push(quote_spanned! {f.span()=>
                        fn #name(&mut self, #name: #setter_type) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    });

                    fields_checker.push(checker);

                    extra_fields.append(generate_extra(&f).as_mut());
                }

                BuilderDerive {
                    option_fields: quote!(#(#options),*),
                    empty_fields: quote!(#(#empties),*),
                    fields_methods: quote!(#(#methods)*),
                    builder_function: quote! {
                        pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                            #(#fields_checker)*
                            Ok(#ident {
                                #(#fields_assign),*
                            })
                        }
                    },
                    extra_fields: quote!(#(#extra_fields)*),
                }
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn is_optional(ty: &syn::Type) -> bool {
    check_type_ident("Option", ty)
}

fn is_vec(ty: &syn::Type) -> bool {
    check_type_ident("Vec", ty)
}

fn check_type_ident(ident_str: &str, ty: &syn::Type) -> bool {
    if let Path(ref ty_path) = ty {
        return ty_path.path.segments[0].ident == ident_str;
    }
    false
}

fn get_inner_type(ty: &syn::Type) -> &syn::Type {
    if let Path(ref type_path) = ty {
        if let AngleBracketed(ref angle_bracketed) = type_path.path.segments[0].arguments {
            if let GenericArgument::Type(ref arg_type) = angle_bracketed.args[0] {
                return arg_type;
            }
        }
    }
    unreachable!()
}

fn generate_extra(field: &syn::Field) -> Vec<proc_macro2::TokenStream> {
    let mut extra = Vec::new();

    for attribute in &field.attrs {
        if attribute.path.is_ident("builder") {
            add_builder_attribute_extras(field, attribute, &mut extra);
        }
    }

    extra
}

fn add_builder_attribute_extras(
    field: &syn::Field,
    attribute: &syn::Attribute,
    extra: &mut Vec<proc_macro2::TokenStream>,
) {
    let parsed_meta = attribute.parse_meta().unwrap();

    if let Meta::List(meta_list) = parsed_meta {
        let each_name_value = &meta_list.nested.iter().find_map(|i| {
            if let NestedMeta::Meta(Meta::NameValue(name_value)) = i {
                if name_value.path.is_ident("each") {
                    return Some(name_value);
                }
            }
            None
        });

        if each_name_value.is_none() {
            extra.push(quote_spanned! {field.span()=>
                compile_error!("attribute `each` not found in `builder`");
            });
            return;
        }

        let each_name_value = each_name_value.unwrap();

        if !is_vec(&field.ty) {
            extra.push(quote_spanned! {field.span()=>
            compile_error!("`each` can only be applied to Vec<T> types");});
            return;
        }

        if let Lit::Str(each) = &each_name_value.lit {
            let name = &field.ident;
            if each.value() == name.as_ref().unwrap().to_string() {
                extra.push(quote_spanned! {field.span()=>
                compile_error!("each has the same name as the struct field");});
            } else {
                let each_value = format_ident!("{}", each.value());
                let inner_type = get_inner_type(&field.ty);
                extra.push(quote_spanned! {field.span()=>
                    pub fn #each_value (&mut self, #each_value: #inner_type) -> &mut Self {
                        let new_vector = match self.#name.take() {
                            Some(mut vector) => {
                                vector.push(#each_value);
                                vector
                            },
                            None => vec![#each_value],
                        };
                        self.#name = Some(new_vector);
                        self
                    }
                });
            }
        } else {
            extra.push(quote_spanned! {field.span()=>
                compile_error!("unrecognized value for `each` attribute in `builder`");
            });
        }
    }
}
