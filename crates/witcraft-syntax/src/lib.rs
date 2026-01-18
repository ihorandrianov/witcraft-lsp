pub mod ast;
mod format;
mod index;
mod kind;
mod lexer;
pub mod lookup;
mod parse;
mod parser;
mod text;

pub use format::Formatter;
pub use index::{
    Definition, DefinitionKind, DuplicateDefinition, GlobalDefinition, Import, PackageId,
    Reference, ReferenceKind, SymbolIndex, UnusedImport,
};
pub use kind::SyntaxKind;
pub use lexer::{Lexer, Token, lex, lex_non_trivia};
pub use lookup::{NodeRef, node_at};
pub use parse::{ErrorKind, ParseError, ParseResult};
pub use parser::parse;
pub use text::{LineIndex, Position, TextRange};

// Re-export commonly used AST types at crate root
pub use ast::{InterfaceItem, InterfaceUse, Item, SourceFile, Type, TypeAlias, TypeDef, WorldItem};
