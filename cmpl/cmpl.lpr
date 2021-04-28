program cmpl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, StrUtils, chardfa, bingen, lexdfa, thralex, vm, lexanalyzer, thrastructs;

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
    lanalyzer : TLexAnalyzer;
    src : AnsiString;
    tokens : TLexTokenArray;
    //parser : TLfnwParseGen;

    //outBytes : TBytes;

    //testInt : Integer;
    //testBytes : Array[0..3] of Byte;

begin

  lexer := TLexer.Create();
  lanalyzer := TLexAnalyzer.Create();
  //parser := TLfnwParseGen.Create();

  src := ReadTextFile(ParamStr(1));
  WriteLn('File: ', ParamStr(1));
  //WriteLn(src);

  tokens := lexer.Lex(src);

  lanalyzer.ProcessTokens(tokens);
  //parser.Run(tokens);

  WriteLn('');
  WriteLn('### Compilation Completed. ###');
  WriteLn('Press Enter To Quit.');
  ReadLn();

  FreeAndNil(lexer);
  FreeAndNil(lanalyzer);
  //FreeAndNil(parser);
  src := '';
end.

