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
        ELexComment,
        ELexSemiColon,

        ELexNumber,
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
    LowerAlphaCL : Array[0..25] of AnsiString = (
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 
        'u', 'v', 'w', 'x', 'y', 'z');
        
    UpperAlphaCL : Array[0..25] of AnsiString = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
        'U', 'V', 'W', 'X', 'Y', 'Z');

    AtomCL : Array[0..67] of AnsiString = (
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
        'u', 'v', 'w', 'x', 'y', 'z',
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
        'U', 'V', 'W', 'X', 'Y', 'Z',
        '-', '+', '=', '>', '<', '''', ':', '@', '#', '$',
        '%', '^', '&', '*', '!', '~');

    WhitespaceCL : Array[0..3] of AnsiString = (#13, #10, #7, #32);
    StringEscapeCL : Array[0..1] of AnsiString = ('\', '"');


constructor TLexer.Create();
var
    StartState, WhitespaceState,
    CommentState, CommentEndState,
    AtomState, AtomEndState,
    LParenState, //LParenEndState,
    RParenState, //RParenEndState,
    StringState, StringEscapeState, StringEndState,
    NumberState, NumberDecimalState, NumberEndState : TDFAState;

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

    CommentState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELexComment));
    CommentEndState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELexComment));

    AtomState := TDFAState.Create('ATOM', 'ATOM', Integer(ELexAtom));
    AtomEndState := TDFAState.Create('ATOM', 'ATOM', Integer(ELexAtom));

    LParenState := TDFAState.Create('LPAREN', 'LPAREN', Integer(ELexLParen));
    //LParenEndState := TDFAState.Create('LPAREN', 'LPAREN', Integer(ELexLParen));

    RParenState := TDFAState.Create('RPAREN', 'RPAREN', Integer(ELexRParen));
    //RParenEndState := TDFAState.Create('RPAREN', 'RPAREN', Integer(ELexRParen));


    StringState := TDFAState.Create('STRING', 'STRING', Integer(ELexString));
    StringEscapeState := TDFAState.Create('STRINGESCAPE', 'STRING', Integer(ELexString));
    StringEndState := TDFAState.Create('STRING', 'STRING', Integer(ELexString));

    NumberDecimalState := TDFAState.Create('NUMBER', 'NUMBER', Integer(ELexNumber));
    NumberState := TDFAState.Create('NUMBER', 'NUMBER', Integer(ELexNumber));
    NumberEndState := TDFAState.Create('NUMBER', 'NUMBER', Integer(ELexNumber));


    FDfa.addState(StartState); (* Must add the First "Start" State, before all others *)
    FDfa.addState(WhitespaceState);
    FDfa.addState(CommentState);
    Fdfa.AddState(CommentEndState);

    FDfa.addState(AtomState);
    FDfa.addState(AtomEndState);

    FDfa.addState(LParenState);
    //FDfa.AddState(LParenEndState);

    FDfa.addState(RParenState);
    //FDfa.AddState(RParenEndState);

    FDfa.addState(StringState);
    FDfa.addState(StringEscapeState);
    FDfa.AddState(StringEndState);

    FDfa.AddState(NumberState);
    FDfa.AddState(NumberDecimalState);
    FDfa.AddState(NumberEndState);


    (* Loop whitespace back to start, we don't care about it *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), StartState, False));

    (* Handle comments *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(';'), CommentState, False));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_IsNot.Create(#10), CommentState));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(#10), CommentEndState, False));

    (* Handle LParen *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('('), LParenState));
    //LParenState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(DigitCL), RegisterEndState, False, True));

    (* Handle RParen *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(')'), RParenState));

    (* Handle Atom *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(AtomCL), AtomState));
    AtomState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(AtomCL), AtomState));
    AtomState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), AtomState));
    AtomState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(AtomCL), AtomEndState, False, True));

    (* Handle String *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('"'), StringState, False));
    StringState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(StringEscapeCL), StringState));
    StringState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('"'), StringEndState, False)); // String ended
    StringState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('\'), StringEscapeState, False));
    StringEscapeState.AddDelta(TDFADelta.Create(TDFAComp_AnyChar.Create(), StringState));



    (* Handle Numbers *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), NumberState));
    NumberState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), NumberState));
    NumberState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(ValidNumberCL), NumberEndState, False, True));
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('.'), NumberDecimalState)); // Skip to the Decimal Right away
    NumberState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('.'), NumberDecimalState)); // Transition to Decimal
    NumberDecimalState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), NumberDecimalState));
    NumberDecimalState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(DigitCL), NumberEndState, False, True));

    
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

end.
