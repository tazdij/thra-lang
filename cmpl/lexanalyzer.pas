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

      function ProcessTokens(ATokens : TLexTokenArray) : Boolean;
  end;

implementation

constructor TLexAnalyzer.Create();
begin

end;

destructor TLexAnalyzer.Destroy();
begin

end;

function TLexAnalyzer.ProcessTokens(ATokens : TLexTokenArray) : Boolean;
var curToken : PLexToken;
    i : Integer;
begin

  for i := 0 to Length(ATokens) - 1 do
  begin
    curToken := @ATokens[i];
    WriteLn('ANALYZE TOKEN: Name: ', (* SizeOf(curToken^.TokenType) *) curToken^.Name, ' => ', curToken^.LexValue);
  end;

end;

end.

