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
    let (option_fields, empty_fields) = generate_struct_fields(&input.data);

    let expanded = quote! {
        pub struct #ident_builder {
            #option_fields
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

fn generate_struct_fields(data: &Data) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let mut options = Vec::new();
                let mut empties = Vec::new();

                for f in &fields.named {
                    let name = &f.ident;
                    let ty = &f.ty;
                    options.push(quote_spanned!(f.span()=> #name: Option<#ty>));
                    empties.push(quote_spanned!(f.span()=> #name: None))
                }

                (quote!(#(#options),*), quote!(#(#empties),*))
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
