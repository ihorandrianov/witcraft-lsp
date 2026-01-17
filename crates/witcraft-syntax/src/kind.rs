/// Syntax element kinds for WIT.
///
/// This enum represents both token kinds (from lexer) and composite node kinds (from parser).
/// Token kinds are used during lexing, composite kinds are added during parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // ==========
    // Tokens
    // ==========

    // Keywords
    PackageKw,
    WorldKw,
    InterfaceKw,
    ImportKw,
    ExportKw,
    IncludeKw,
    UseKw,
    AsKw,
    WithKw,
    TypeKw,
    ResourceKw,
    FuncKw,
    RecordKw,
    FlagsKw,
    VariantKw,
    EnumKw,
    StaticKw,
    ConstructorKw,
    BorrowKw,
    OwnKw,
    AsyncKw,
    FromKw,

    // Builtin types
    BoolKw,
    U8Kw,
    U16Kw,
    U32Kw,
    U64Kw,
    S8Kw,
    S16Kw,
    S32Kw,
    S64Kw,
    F32Kw,
    F64Kw,
    CharKw,
    StringKw,
    ListKw,
    OptionKw,
    ResultKw,
    TupleKw,
    FutureKw,
    StreamKw,

    // Punctuation
    LBrace,    // {
    RBrace,    // }
    LParen,    // (
    RParen,    // )
    LAngle,    // <
    RAngle,    // >
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;
    Eq,        // =
    Dot,       // .
    At,        // @
    Slash,     // /
    Minus,     // -
    Arrow,     // ->
    Star,      // *
    Underscore, // _

    // Literals and identifiers
    Ident,
    Integer,

    // Trivia (preserved for formatting, ignored by parser)
    Whitespace,
    LineComment,
    DocComment,
    BlockComment,

    // Special
    Error,
    Eof,

    // ==========
    // Composite nodes (added by parser)
    // ==========
    SourceFile,
}

impl SyntaxKind {
    /// Returns true if this is trivia (whitespace or comments).
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            SyntaxKind::Whitespace
                | SyntaxKind::LineComment
                | SyntaxKind::DocComment
                | SyntaxKind::BlockComment
        )
    }

    /// Returns true if this is a keyword (not including builtin types).
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            SyntaxKind::PackageKw
                | SyntaxKind::WorldKw
                | SyntaxKind::InterfaceKw
                | SyntaxKind::ImportKw
                | SyntaxKind::ExportKw
                | SyntaxKind::IncludeKw
                | SyntaxKind::UseKw
                | SyntaxKind::AsKw
                | SyntaxKind::WithKw
                | SyntaxKind::TypeKw
                | SyntaxKind::ResourceKw
                | SyntaxKind::FuncKw
                | SyntaxKind::RecordKw
                | SyntaxKind::FlagsKw
                | SyntaxKind::VariantKw
                | SyntaxKind::EnumKw
                | SyntaxKind::StaticKw
                | SyntaxKind::ConstructorKw
                | SyntaxKind::BorrowKw
                | SyntaxKind::OwnKw
                | SyntaxKind::AsyncKw
                | SyntaxKind::FromKw
        )
    }

    /// Returns true if this is a builtin type keyword.
    pub fn is_builtin_type(self) -> bool {
        matches!(
            self,
            SyntaxKind::BoolKw
                | SyntaxKind::U8Kw
                | SyntaxKind::U16Kw
                | SyntaxKind::U32Kw
                | SyntaxKind::U64Kw
                | SyntaxKind::S8Kw
                | SyntaxKind::S16Kw
                | SyntaxKind::S32Kw
                | SyntaxKind::S64Kw
                | SyntaxKind::F32Kw
                | SyntaxKind::F64Kw
                | SyntaxKind::CharKw
                | SyntaxKind::StringKw
                | SyntaxKind::ListKw
                | SyntaxKind::OptionKw
                | SyntaxKind::ResultKw
                | SyntaxKind::TupleKw
                | SyntaxKind::FutureKw
                | SyntaxKind::StreamKw
        )
    }

    /// Returns true if this token can start a top-level item.
    pub fn can_start_item(self) -> bool {
        matches!(
            self,
            SyntaxKind::PackageKw
                | SyntaxKind::InterfaceKw
                | SyntaxKind::WorldKw
                | SyntaxKind::UseKw
                | SyntaxKind::TypeKw
                | SyntaxKind::RecordKw
                | SyntaxKind::VariantKw
                | SyntaxKind::EnumKw
                | SyntaxKind::FlagsKw
                | SyntaxKind::ResourceKw
        )
    }

    /// Returns true if this token can start an interface item.
    pub fn can_start_interface_item(self) -> bool {
        matches!(
            self,
            SyntaxKind::UseKw
                | SyntaxKind::TypeKw
                | SyntaxKind::RecordKw
                | SyntaxKind::VariantKw
                | SyntaxKind::EnumKw
                | SyntaxKind::FlagsKw
                | SyntaxKind::ResourceKw
                | SyntaxKind::Ident // function definitions start with ident
        )
    }
}

impl From<SyntaxKind> for u16 {
    fn from(kind: SyntaxKind) -> Self {
        kind as u16
    }
}
