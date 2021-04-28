unit lexanalyzer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, thrastructs, thralex;

type

  (*
     TLexAnalyzer - loads in a stream of LexTokens, and modifies them to be more
     correct.
  *)
  TLexAnalyzer = class
    private

    public
      constructor Create();
      destructor Destroy(); override;

      function ProcessTokens(ATokens : TLexTokenArray) : TLexTokenArray;
  end;

implementation

constructor TLexAnalyzer.Create();
begin

end;

destructor TLexAnalyzer.Destroy();
begin

end;

function TLexAnalyzer.ProcessTokens(ATokens : TLexTokenArray) : TLexTokenArray;
var curToken : PLexToken;
    i : Integer;
begin

  SetLength(Result, BUFFER_CHUNK_SIZE);

  for i := 0 to Length(ATokens) - 1 do
  begin
    curToken := @ATokens[i];
    WriteLn('ANALYZE TOKEN: Name: ', (* SizeOf(curToken^.TokenType) *) curToken^.Name, ' => ', curToken^.LexValue);
  end;

  SetLength(Result, 0);

end;

end.

