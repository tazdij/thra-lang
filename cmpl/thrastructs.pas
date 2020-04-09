unit thrastructs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ELexTokenType = (
        (* None, is used when there is no Token needed *)
        ELexNone,
        ELexIdent,
        ELexLParen, ELexRParen,
        ELexLBrace, ELexRBrace,
        ELexLBracket, ELexRBracket,

        ELexComment,
        ELexMultiComment,

        ELexComma,
        ELexEquals,
        ELexAssign,
        ELexColon,
        ELexSemiColon,
        ELexAccessor,

        ELexLessThan,
        ELexGreaterThan,

        ELexAdd,
        ELexSubtract,
        ELexMultiply,
        ELexDivide,
        ELexIntDivide,
        ELexModulo,

        ELexCarat,
        ELexAt,

        ELexSymbol,

        ELexInt,
        ELexFloat,
        ELexHexLiteral,
        ELexBinLiteral,
        ELexOctLiteral,
        ELexString,

        ELexMacro);

    PLexToken = ^TLexToken;
    TLexToken = packed record
        TokenType : ELexTokenType;
        Name : AnsiString;
        LexValue : AnsiString;
        LineNum : Cardinal;
        CharNum : Cardinal;
    end;

    TLexTokenArray = Array of TLexToken;


    (* Define AST Node types *)

implementation

end.

