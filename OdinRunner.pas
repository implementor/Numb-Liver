{/    Copyright 2011 Marvin Cohrs
//
//    This file is part of OdinVM.
//
//    OdinVM is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.}
{$mode objfpc}
(**********************************************************************
**             OdinRunner.pas                                        **
**             Odin VM - Infrastruktur                               **
**             (c) 2011 Marvin Cohrs                                 **
**             Dev. start: Mo 15 Nov 2010                            **
**********************************************************************)

unit OdinRunner;

interface

uses SysUtils;

const
	// RegIds
	REG_INVALID					= $00;
	REG_RAX						= $01;
	REG_RBX						= $02;
	REG_RCX						= $03;
	REG_RDX						= $04;
	REG_RSI						= $05;
	REG_RDI						= $06;
	REG_X01						= $07;
	REG_X02						= $08;
	REG_X03						= $09;
	REG_X04						= $0A;
	REG_MIN						= REG_RAX;
	REG_MAX						= REG_RDI;
    // FwLookups
    FWL_MIN                     = $30;
    FWL_OUTSTRING               = $31;
    FWL_INSTRING                = $32;
    FWL_RANDOM                  = $33;
    FWL_MAX                     = $40;

type
	// Predecl
    IFirmware = interface;
	IInvoker = interface;
	IInstructionContext = interface;
	IRunningContext = interface;
	TRunner = class;
	
	// Aliases
	ushort = Word;
	uint = DWord;
	ulong = QWord;
	sbyte = shortint;
	short = smallint;
	int = longint;
	long = int64;
	plong = ^long;
	pulong = ^ulong;
	pbyte = ^byte;
	bool = boolean;
	
	// Main Components
	TRegId = REG_MIN .. REG_MAX;
    TFwLookup = FWL_MIN .. FWL_MAX;
	TData = array of byte;
    IFirmware = interface
        procedure Interrupt(const iid: ushort; const ic: IInstructionContext);
        function Lookup(const fwlookup: tfwlookup): ushort;
    end;
	IInvoker = interface
		procedure Invoke(const cmd: uint; const ic: IInstructionContext);
		function QueryPrefix(var cmd: uint; const ic: IInstructionContext): Boolean;
        procedure TellFirmware(const fw: IFirmware);
	end;
	IInstructionContext = interface
		procedure Jump(const addr: ulong);
		procedure SetRegisterU(const regid: TRegId; const val: ulong);
		procedure SetRegisterS(const regid: TRegId; const val: long);
		function GetRegisterU(const regid: TRegId): ulong; overload;
		procedure GetRegisterU(const regid: TRegId; out val: ulong); overload;
		function GetRegisterS(const regid: TRegId): long; overload;
		procedure GetRegisterS(const regid: TRegId; out val: long); overload;
		property RegisterU[regid:TRegId]: ulong read GetRegisterU write SetRegisterU;
		property RegisterS[regid:TRegId]: long read GetRegisterS write SetRegisterS;
		procedure Test(const val: ulong); overload;
		procedure Test(const v1, v2: ulong); overload;
		procedure Quit;
		function NextParam: Byte; overload;
		procedure NextParam(out b: byte); overload;
		procedure NextParam(out u: ushort); overload;
		procedure NextParam(out u: uint); overload;
		procedure NextParam(out u: ulong); overload;
	end;
	IRunningContext = interface
		procedure Run; overload;
		procedure Run(const EntryAddress: ulong); overload;
		procedure Dismiss;
	end;
	TRunner = class
	private
		FRAM: TData;
		FRegs: array[TRegId] of ulong;
		FPos, FEntry, FTable: ulong;
		FIC: IInstructionContext;
		FRC: IRunningContext;
		FRCSC: long;
		FMask: uint;
		FInvoker: IInvoker;
        FFirmware: IFirmware;
		FQuit: Boolean;
		FGreater, FEqual, FLower, FAbove, FBelow,
		FNull: Boolean;
	public
		constructor Create;
		function Feed(const Bytes: TData; const Invoker: IInvoker; const fw: IFirmware; const EntryAddress: ulong = 0): IRunningContext;
	protected
		procedure Run(const sc: long); overload;
		procedure Run(const sc: long; const EntryAddress: ulong); overload;
		procedure Dismiss(const sc: long);
		procedure Proceed;
	end;

implementation

type
	TRunningContext = class (TInterfacedObject, IRunningContext)
	private
		FRunner: TRunner;
		FSC: ulong;
		FValid: Boolean;
	public
		constructor Create(const Runner: TRunner; const sc: long);
		procedure Run; overload;
		procedure Run(const EntryAddress: ulong); overload;
		procedure Dismiss;
	end;
	TInstructionContext = class (TInterfacedObject, IInstructionContext)
	private
		FRunner: TRunner;
	public
		procedure Jump(const addr: ulong);
		procedure SetRegisterU(const regid: TRegId; const val: ulong);
		procedure SetRegisterS(const regid: TRegId; const val: long);
		function GetRegisterU(const regid: TRegId): ulong; overload;
		procedure GetRegisterU(const regid: TRegId; out val: ulong); overload;
		function GetRegisterS(const regid: TRegId): long; overload;
		procedure GetRegisterS(const regid: TRegId; out val: long); overload;
		property RegisterU[regid:TRegId]: ulong read GetRegisterU write SetRegisterU;
		property RegisterS[regid:TRegId]: long read GetRegisterS write SetRegisterS;
		procedure Test(const val: ulong); overload;
		procedure Test(const v1, v2: ulong); overload;
		procedure Quit;
		constructor Create(const Runner: TRunner);
		function NextParam: Byte; overload;
		procedure NextParam(out b: byte); overload;
		procedure NextParam(out u: ushort); overload;
		procedure NextParam(out u: uint); overload;
		procedure NextParam(out u: ulong); overload;
	end;

// TRunner
constructor TRunner.Create;
begin
	inherited Create;
	FRC := nil;
	FIC := nil;
	FEntry := 0;
	FTable := 0;
	FInvoker := nil;
    FFirmware := nil;
end;

function TRunner.Feed(const Bytes: TData; const Invoker: IInvoker; const fw: IFirmware; const EntryAddress: ulong = 0): IRunningContext;
begin
	if FRC <> nil then FRC.Dismiss;
	FRCSC := Random(High(long));
	FRC := TRunningContext.Create(Self, FRCSC);
	FRAM := Bytes;
	FEntry := EntryAddress;
	FInvoker := Invoker;
    FFirmware := fw;
	Result := FRC;
end;

procedure TRunner.Run(const sc: long);
var
	i: TRegId;
	cmd: uint;
begin
	if sc <> FRCSC then
		raise Exception.Create('Safety Code doesn''t match! Invalid try to invoke IRunningContext.Run!');
	WriteLn('x');
	FPos := FEntry;
	FMask := 0;
	for i := Low(i) to High(i) do
		FRegs[i] := 0;
	Writeln('y');
	FQuit := false;
	FGreater := false;
	FLower := false;
	FEqual := false;
	FAbove := false;
	FBelow := false;
	FNull := false;
	FIC := TInstructionContext.Create(Self);
	WriteLn('z');
    FInvoker.TellFirmware(FFirmware);
	while (not FQuit) and (FPos < Length(FRAM)) do
	begin
		cmd := FMask or FRAM[FPos];
		Proceed;
		if FInvoker.QueryPrefix(cmd, FIC) then begin
			FMask := cmd * 256;
		end else begin
			FInvoker.Invoke(cmd, FIC);
			FMask := 0;
		end;
	end;
end;

procedure TRunner.Run(const sc: long; const EntryAddress: ulong);
begin
	FEntry := EntryAddress;
	Run(sc);
end;

procedure TRunner.Dismiss(const sc: long);
begin
	if sc <> FRCSC then
		raise Exception.Create('Safety Code doesn''t match! Invalid try to invoke IRunningContext.Dismiss!');
	SetLength(FRAM, 0);
	FRCSC := Random(High(long));
end;

procedure TRunner.Proceed;
begin
	Inc(FPos);
end;

// TRunningContext
constructor TRunningContext.Create(const Runner: TRunner; const sc: long);
begin
	if Runner = nil then
		raise Exception.Create('Argument ''Runner'' mustn''t be null.');
	FRunner := Runner;
	FSC := sc;
	FValid := true;
end;

procedure TRunningContext.Run;
begin
	FRunner.Run(FSC);
end;

procedure TRunningContext.Run(const EntryAddress: ulong);
begin
	FRunner.Run(FSC, EntryAddress);
end;

procedure TRunningContext.Dismiss;
begin
	FRunner.Dismiss(FSC);
	FValid := false;
end;

// TInstructionContext
procedure TInstructionContext.Jump(const addr: ulong);
begin
	FRunner.FPos := addr;
end;

procedure TInstructionContext.SetRegisterU(const regid: TRegId; const val: ulong);
begin
	FRunner.FRegs[regid] := val;
end;

procedure TInstructionContext.SetRegisterS(const regid: TRegId; const val: long);
begin
	FRunner.FRegs[regid] := pulong(@val)^;
end;

procedure TInstructionContext.GetRegisterU(const regid: TRegId; out val: ulong);
begin
	val := FRunner.FRegs[regid];
end;

procedure TInstructionContext.GetRegisterS(const regid: TRegId; out val: long);
begin
	pulong(@val)^ := FRunner.FRegs[regid];
end;

function TInstructionContext.GetRegisterU(const regid: TRegId): ulong;
begin
	GetRegisterU(regid, Result);
end;

function TInstructionContext.GetRegisterS(const regid: TRegId): long;
begin
	GetRegisterS(regid, Result);
end;

procedure TInstructionContext.Test(const val: ulong);
begin
	FRunner.FNull := val = 0;
end;

procedure TInstructionContext.Test(const v1, v2: ulong);
begin
	FRunner.FAbove := v1 > v2;
	FRunner.FBelow := v1 < v2;
	FRunner.FEqual := v1 = v2;
	FRunner.FGreater := plong(@v1)^ > plong(@v2)^;
	FRunner.FLower := plong(@v1)^ < plong(@v2)^;
end;

procedure TInstructionContext.Quit;
begin
	FRunner.FQuit := true;
end;

constructor TInstructionContext.Create(const Runner: TRunner);
begin
	inherited Create;
	FRunner := Runner;
end;

function TInstructionContext.NextParam: Byte; overload;
begin
	NextParam(Result);
end;

procedure TInstructionContext.NextParam(out b: byte); overload;
begin
	b := FRunner.FRAM[FRunner.FPos];
	FRunner.Proceed;
end;

procedure TInstructionContext.NextParam(out u: ushort); overload;
begin
    u := NextParam + (NextParam shl 8);
	{NextParam(pbyte(@u)^);
	NextParam(pbyte(ulong(@u)+1)^);}
end;

procedure TInstructionContext.NextParam(out u: uint); overload;
begin
    u := NextParam + (NextParam shl 8) + (NextParam shl 16) + (NextParam shl 24);
	{NextParam(pbyte(@u)^);
	NextParam(pbyte(ulong(@u)+1)^);
	NextParam(pbyte(ulong(@u)+2)^);
	NextParam(pbyte(ulong(@u)+3)^);}
end;

procedure TInstructionContext.NextParam(out u: ulong); overload;
begin
    u := NextParam + (NextParam shl 8) + (NextParam shl 16) + (NextParam shl 24)
        + (NextParam shl 32) + (NextParam shl 40) + (NextParam shl 48) + (NextParam shl 56);
	{NextParam(pbyte(@u)^);
	NextParam(pbyte(ulong(@u)+1)^);
	NextParam(pbyte(ulong(@u)+2)^);
	NextParam(pbyte(ulong(@u)+3)^);
	NextParam(pbyte(ulong(@u)+4)^);
	NextParam(pbyte(ulong(@u)+5)^);
	NextParam(pbyte(ulong(@u)+6)^);
	NextParam(pbyte(ulong(@u)+7)^);}
end;

initialization
	randomize;
end.
