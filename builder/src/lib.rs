extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    // eprintln!("INPUT: {:#?}", input);

    let ident = input.ident;
    let ident_builder = format_ident!("{}Builder", ident);
    let (option_fields, empty_fields, fields_methods, builder_function) =
        generate_struct_fields(&ident, &input.data);

    let expanded = quote! {
        pub struct #ident_builder {
            #option_fields
        }

        impl #ident_builder {
            #fields_methods
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

    eprintln!("TOKENS:\n{}", expanded);
    TokenStream::from(expanded)
}

fn generate_struct_fields(
    ident: &proc_macro2::Ident,
    data: &Data,
) -> (
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
) {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let mut options = Vec::new();
                let mut empties = Vec::new();
                let mut methods = Vec::new();
                let mut fields_checker = Vec::new();
                let mut fields_assign = Vec::new();

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
                }

                (
                    quote!(#(#options),*),
                    quote!(#(#empties),*),
                    quote!(#(#methods)*),
                    quote! {
                        pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                            #(#fields_checker)*
                            Ok(#ident {
                                #(#fields_assign),*
                            })
                        }
                    },
                )
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn is_optional(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(ref ty_path) => {
            ty_path.path.segments[0].ident == "Option"
        },
        _ => false
    }
}

fn get_inner_type(ty: &syn::Type) -> &syn::Type {
    match ty {
        syn::Type::Path(ref type_path) => {
            match type_path.path.segments[0].arguments {
                syn::PathArguments::AngleBracketed(ref angle_bracketed) => {
                    match angle_bracketed.args[0] {
                        syn::GenericArgument::Type(ref arg_type) => arg_type,
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!()
            }
        }
        _ => unreachable!()
    }
}
