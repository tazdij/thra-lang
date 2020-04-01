program cmpl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, StrUtils, chardfa, bingen, lexdfa, thralex, vm;

function ReadTextFile(path : AnsiString) : AnsiString;
var
    f : TextFile;
    s : AnsiString;
begin

    Result := '';
    if FileExists(path) then
    begin
        AssignFile(f, path);

        (* Open for reading at top *)
        Reset(f);

        while not eof(f) do
        begin
            readln(f, s);
            Result := Result + s + #10;
        end;
    end;
end;

var
    lexer : TLexer;
    src : AnsiString;
    tokens : TLexTokenArray;
    //parser : TLfnwParseGen;

    outBytes : TBytes;

    testInt : Integer;
    testBytes : Array[0..3] of Byte;

begin

  lexer := TLexer.Create();
  //parser := TLfnwParseGen.Create();

  src := ReadTextFile(ParamStr(1));
  WriteLn('File: ', ParamStr(1));
  WriteLn(src);

  tokens := lexer.Lex(src);


  //parser.Run(tokens);


  FreeAndNil(lexer);
  //FreeAndNil(parser);
  src := '';

end.

