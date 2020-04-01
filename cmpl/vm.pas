unit vm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fgl;

type
  EInstruction = (
    EInst
  );
  VMInstruction = packed record
    Instruction : EInstruction;

  end;

  PQLispVMObj = ^QLispVMObj;
  QLispVMObj = packed record

  end;

  QLispVMObjMap = specialize TFPGMap<AnsiString, PQLispVMObj>;

  QLispVM = record
    Heap : QLispVMObjMap;

  end;

implementation



end.

