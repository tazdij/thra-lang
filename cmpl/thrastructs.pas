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

    (* Used for the storage of literal values. maybe in a compiler VM, too? *)
    ELexValueType = (
        ELexValNone,        (* None is for non-literal source values *)
        ELexValBool,
        ELexValUInt8,
        ELexValInt8,
        ELexValUInt16,
        ELexValInt16,
        ELexValUInt32,
        ELexValInt32,
        ELexValUInt64,
        ELexValInt64,
        ELexValFloat32,
        ELexValFloat64,
        ELexValString
    );

    PLexToken = ^TLexToken;
    TLexToken = packed record
        TokenType : ELexTokenType;
        Name : AnsiString;
        LexValue : AnsiString;
        LineNum : Cardinal;
        CharNum : Cardinal;
        case ValueType : ELexValueType of
            ELexValNone : (NoneVal : Pointer);
            ELexValBool : (BoolVal : Boolean);
            ELexValUInt8 : (UInt8Val : Byte);
            ELexValInt8 : (Int8Val : ShortInt);
            ELexValUInt16 : (UInt16 : Word);
            ELexValInt16 : (Int16Val : SmallInt);
            ELexValUInt32 : (UInt32Val : Cardinal);
            ELexValInt32 : (Int32Val : LongInt);
            ELexValInt64 : (Int64Val : Int64);
            ELexValFloat32 : (Float32Val : Single);
            ELexValFloat64 : (Float64Val : Double);
            ELexValString : (StringVal : PChar);       (* We have to manage Heap allocation for the String Value *)
    end;

    TLexTokenArray = Array of TLexToken;


    (* Define AST Node types *)
    EAstNodeType = (
        EAstAssign,
        EAstIf,
        EAstCompare,
        EAstCall
    );

    PAstNode = ^TAstNode;
    TAstNode = packed record
        Token : PLexToken;
        NodeType : EAstNodeType;

    end;

    TAstNode_If = packed record

    end;

const
    BUFFER_CHUNK_SIZE = 100;

implementation

end.

