unit thralex;

{$mode objfpc}{$H+}

interface

uses chardfa;

type
    ELexTokenType = (
        (* None, is used when there is no Token needed *)
        ELexNone,
        ELexIdent,
        ELexLParen, ELexRParen,
        ELexLBrace, ELexRBrace,

        ELexComment,
        ELexMultiComment,

        ELexColon,
        ELexSemiColon,

        ELexSymbol,

        ELexInt,
        ELexFloat,
        ELexHexLiteral,
        ELexBinLiteral,
        ELexOctLiteral,
        ELexString,

        ELexMacro);
    
    PLexToken = ^TLexToken;
    TLexToken = record
        TokenType : ELexTokenType;
        Name : AnsiString;
        LexValue : AnsiString;
        LineNum : Cardinal;
        CharNum : Cardinal;
    end;
    
    TLexTokenArray = Array of TLexToken;
    
    TLexer = class(TObject)
        private
            FDfa : TCharDFA;
            FCurChar, FCurLine: Cardinal;
            FTokenList : TLexTokenArray;
        public
            constructor Create();
            destructor Destroy(); Override;

            procedure HandleDFAToken(token : PDFAToken);
            
            function Lex(s : AnsiString) : TLexTokenArray;
    end;

implementation

uses sysutils, lazutf8;

var
    // CharList : Array[0..1] of AnsiString = ('a', '四');
    ValidNumberCL : Array[0..10] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.');
    DigitCL : Array[0..9] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
    StartDecimalCL : Array[0..8] of AnsiString = ('1', '2', '3', '4', '5', '6', '7', '8', '9');
    BinDigitCL : Array[0..1] of AnsiString = ('0', '1');
    OctDigitCL : Array[0..7] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7');

    HexDigitCL : Array[0..21] of AnsiString = (
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd',
        'e', 'f');

    LowerAlphaCL : Array[0..25] of AnsiString = (
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 
        'u', 'v', 'w', 'x', 'y', 'z');
        
    UpperAlphaCL : Array[0..25] of AnsiString = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
        'U', 'V', 'W', 'X', 'Y', 'Z');

    IdentStartCL : Array[0..26] of AnsiString = (
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
        'u', 'v', 'w', 'x', 'y', 'z', '_');

    SymbolCL : Array[0..62] of AnsiString = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
        'U', 'V', 'W', 'X', 'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
        'u', 'v', 'w', 'x', 'y', 'z',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        '_');

    WhitespaceCL : Array[0..3] of AnsiString = (#13, #10, #7, #32);
    StringEscapeCL : Array[0..1] of AnsiString = ('\', '"');

    IdentCLs : Array[0..1] of TDFAComparator;


constructor TLexer.Create();
var
    StartState, WhitespaceState,
    CommentStartState,
    CommentMultiLineState, CommentMultiLineEndingState, CommentMultiLineEndState,
    CommentSingleLineState, CommentEndState,
    IdentState, IdentEndState,
    LParenState, //LParenEndState,
    RParenState, //RParenEndState,
    LBraceState,
    RBraceState,

    StringState, StringEscapeState, StringEndState,

    SymbolState, SymbolEndState,


    IntState, IntEndState,
    FloatState, FloatEndState,
    AltBaseLiteralStartState,
    OctLiteralState, OctLiteralEndState,
    BinLiteralState, BinLiteralEndState,
    HexLiteralState, HexLiteralEndState,

    ColonState,
    SemiColonState : TDFAState;

begin
    self.FCurLine := 1;
    self.FCurChar := 0;
    SetLength(self.FTokenList, 0);

    FDfa := TCharDFA.Create();

    (* Assign the LexToken Generator *)
    FDfa.SetTokenHandler(@Self.HandleDFAToken);
    
    (* configure DFA to Lex LnfwSource *)
    StartState := TDFAState.Create('START', 'START', Integer(ELexNone));
    WhitespaceState := TDFAState.Create('WHITESPACE', 'WS', Integer(ELexNone));

    CommentStartState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELexComment));
    CommentMultiLineState := TDFAState.Create('MULTICOMMENT', 'MCMNT', Integer(ELexMultiComment));
    CommentMultiLineEndingState := TDFAState.Create('MULTICOMMENT', 'MCMNT', Integer(ELexMultiComment));
    CommentMultiLineEndState := TDFAState.Create('MULTICOMMENT', 'MCMNT', Integer(ELexMultiComment));
    CommentSingleLineState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELexComment));
    CommentEndState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELexComment));

    IdentState := TDFAState.Create('IDENT', 'IDENT', Integer(ELexIdent));
    IdentEndState := TDFAState.Create('IDENT', 'IDENT', Integer(ELexIdent));

    LParenState := TDFAState.Create('LPAREN', 'LPAREN', Integer(ELexLParen));
    RParenState := TDFAState.Create('RPAREN', 'RPAREN', Integer(ELexRParen));

    LBraceState := TDFAState.Create('LBRACE', 'LBRACE', Integer(ELexLBrace));
    RBraceState := TDFAState.Create('RBRACE', 'RBRACE', Integer(ELexRBrace));

    ColonState := TDFAState.Create('COLON', 'COLON', Integer(ELexColon));
    SemiColonState := TDFAState.Create('SEMICOLON', 'SEMICOLON', Integer(ELexSemiColon));

    SymbolState := TDFAState.Create('SYMBOL', 'SYMBOL', Integer(ELexSymbol));
    SymbolEndState := TDFAState.Create('SYMBOL', 'SYMBOL', Integer(ELexSymbol));


    StringState := TDFAState.Create('STRING', 'STRING', Integer(ELexString));
    StringEscapeState := TDFAState.Create('STRINGESCAPE', 'STRING', Integer(ELexString));
    StringEndState := TDFAState.Create('STRING', 'STRING', Integer(ELexString));

    IntState := TDFAState.Create('INT', 'INT', Integer(ELexInt));
    IntEndState := TDFAState.Create('INT', 'INT', Integer(ELexInt));
    FloatState := TDFAState.Create('FLOAT', 'FLOAT', Integer(ELexFloat));
    FloatEndState := TDFAState.Create('FLOAT', 'FLOAT', Integer(ELexFloat));

    AltBaseLiteralStartState := TDFAState.Create('INT', 'INT', Integer(ELexInt));
    OctLiteralState := TDFAState.Create('OCT', 'OCT', Integer(ELexOctLiteral));
    OctLiteralEndState := TDFAState.Create('OCT', 'OCT', Integer(ELexOctLiteral));
    BinLiteralState := TDFAState.Create('BIN', 'BIN', Integer(ELexBinLiteral));
    BinLiteralEndState := TDFAState.Create('BIN', 'BIN', Integer(ELexBinLiteral));
    HexLiteralState := TDFAState.Create('HEX', 'HEX', Integer(ELexHexLiteral));
    HexLiteralEndState := TDFAState.Create('HEX', 'HEX', Integer(ELexHexLiteral));


    FDfa.AddState(StartState); (* Must add the First "Start" State, before all others *)
    FDfa.AddState(WhitespaceState);
    FDfa.AddState(CommentStartState);
    FDfa.AddState(CommentMultiLineState);
    FDfa.AddState(CommentMultiLineEndingState);
    FDfa.AddState(CommentMultiLineEndState);
    FDfa.AddState(CommentSingleLineState);
    FDfa.AddState(CommentEndState);

    FDfa.AddState(IdentState);
    FDfa.AddState(IdentEndState);

    FDfa.AddState(LParenState);
    FDfa.AddState(RParenState);

    FDfa.AddState(LBraceState);
    FDfa.AddState(RBraceState);

    FDfa.AddState(SemiColonState);
    FDfa.AddState(ColonState);

    FDfa.AddState(SymbolState);
    FDfa.AddState(SymbolEndState);

    FDfa.AddState(StringState);
    FDfa.AddState(StringEscapeState);
    FDfa.AddState(StringEndState);

    FDfa.AddState(IntState);
    FDfa.AddState(IntEndState);
    FDfa.AddState(FloatState);
    FDfa.AddState(FloatEndState);

    FDfa.AddState(AltBaseLiteralStartState);
    FDfa.AddState(OctLiteralState);
    FDfa.AddState(OctLiteralEndState);
    FDfa.AddState(BinLiteralState);
    FDfa.AddState(BinLiteralEndState);
    FDfa.AddState(HexLiteralState);
    FDfa.AddState(HexLiteralEndState);


    (* Loop whitespace back to start, we don't care about it *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), StartState, False));

    (* Handle comments *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('/'), CommentStartState, False));
    CommentStartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('/'), CommentSingleLineState, False));
    CommentStartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('*'), CommentMultiLineState, False));
    CommentSingleLineState.AddDelta(TDFADelta.Create(TDFAComp_IsNot.Create(#10), CommentSingleLineState));
    CommentSingleLineState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(#10), CommentEndState, False));
    CommentMultiLineState.AddDelta(TDFADelta.Create(TDFAComp_IsNot.Create('*'), CommentMultiLineState));
    CommentMultiLineState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('*'), CommentMultiLineEndingState));
    CommentMultiLineEndingState.AddDelta(TDFADelta.Create(TDFAComp_IsNot.Create('/'), CommentMultiLineState));
    CommentMultiLineEndingState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('/'), CommentMultiLineEndState, False));
    // All MultiComments have a trailing * to cleanup at some stage


    (* Handle LParen *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('('), LParenState));
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(')'), RParenState));

    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('{'), LBraceState));
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('}'), RBraceState));

    (* Handle SemiColon *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(';'), SemiColonState));

    (* Handle Colon *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(':'), ColonState));

    (* Handle IdentStart *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(IdentStartCL), IdentState));
    IdentState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(IdentStartCL), IdentState));
    IdentState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), IdentState));
    IdentState.AddDelta(TDFADelta.Create(TDFAComp_And.Create(IdentCLs), IdentEndState, False, True));

    (* Handle Symbols *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), SymbolState));
    SymbolState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(SymbolCL), SymbolState));
    SymbolState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(SymbolCL), SymbolEndState, False, True));

    (* Handle String *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('"'), StringState, False));
    StringState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(StringEscapeCL), StringState));
    StringState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('"'), StringEndState, False)); // String ended
    StringState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('\'), StringEscapeState, False));
    StringEscapeState.AddDelta(TDFADelta.Create(TDFAComp_AnyChar.Create(), StringState));



    (* Handle Decimal Int or Float *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(StartDecimalCL), IntState));
    IntState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), IntState));
    IntState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(ValidNumberCL), IntEndState, False, True));
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('.'), FloatState)); // Skip to the Decimal Right away
    IntState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('.'), FloatState)); // Transition to Decimal
    FloatState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), FloatState));
    FloatState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(DigitCL), FloatEndState, False, True));

    (* Handle Alternative Base Numbers (Floats must be ) *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('0'), AltBaseLiteralStartState, False));
    AltBaseLiteralStartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('b'), BinLiteralState, False));
    AltBaseLiteralStartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('o'), OctLiteralState, False));
    AltBaseLiteralStartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('x'), HexLiteralState, False));
    BinLiteralState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(BinDigitCL), BinLiteralState));
    BinLiteralState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('_'), BinLiteralState, False));
    BinLiteralState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(BinDigitCL), BinLiteralEndState, False, True));
    OctLiteralState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(OctDigitCL), OctLiteralState));
    OctLiteralState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(OctDigitCL), OctLiteralEndState, False, True));
    HexLiteralState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(HexDigitCL), HexLiteralState));
    HexLiteralState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(HexDigitCL), HexLiteralEndState, False, True));
    
end;


destructor TLexer.Destroy();
begin
    FreeAndNil(Fdfa);
    SetLength(self.FTokenList, 0);
    
    inherited Destroy();
end;

procedure TLexer.HandleDFAToken(token : PDFAToken);
var pToken : PLexToken;
    i : Integer;
begin
  WriteLn('#TOKEN: ', token^.TokenName, ' -> ', token^.TokenVal);

  i := Length(self.FTokenList);
  SetLength(self.FTokenList, i + 1);
  pToken := @self.FTokenList[i];

  pToken^.LexValue := token^.TokenVal;
  pToken^.TokenType := ELexTokenType(token^.TokenId);
  pToken^.Name := token^.TokenName;

end;

function TLexer.Lex(s : AnsiString) : TLexTokenArray;
var
    len : Integer;
    curCodePoint : AnsiString;
    curP, endP : PChar;
    reprocessCodePoint : Boolean;
begin
    curP := PChar(s);
    endP := curP + Length(s);
    
    while curP < endP do
    begin
        len := UTF8CodePointSize(CurP);
        SetLength(curCodePoint, len);
        Move(curP^, curCodePoint[1], len);
        
        if curCodePoint = #10 then
        begin
            self.FCurChar := 0;
            self.FCurLine := self.FCurLine + 1;
        end
        else if curCodePoint = #13 then
        begin
            (* Ignore cariage return *)
        end
        else
        begin
            Inc(self.FCurChar);
            //WriteLn('Line: ', curLineNum, ', Char: ', curCharNum, ', => ', curCodePoint);
        end;
        //Write(curCodePoint);

        reprocessCodePoint := False;
        
        (* Pass char into dfa state *)
        if not self.FDfa.nextChar(curCodePoint, reprocessCodePoint) then
        begin
            WriteLn('Error: no debugging info yet.');
        end;

        if not reprocessCodePoint then
            Inc(curP, len)
        else
        begin

        end;
    end;

    Result := self.FTokenList;

end;

initialization

IdentCLs[0] := TDFAComp_IsNotIn.Create(IdentStartCL);
IdentCLs[1] := TDFAComp_IsNotIn.Create(DigitCL);

finalization

FreeAndNil(IdentCLs[0]);
FreeAndNil(IdentCLs[1]);

end.
