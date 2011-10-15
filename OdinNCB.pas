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
**             OdinNCB.pas                                           **
**             Odin VM - NCB Instruction Set                         **
**             (c) 2010 Marvin Cohrs                                 **
**             Dev. start: Mo 15 Nov 2010                            **
**********************************************************************)

unit OdinNCB;

interface

uses OdinRunner;

type
	TOdinNCB = class (TInterfacedObject, IInvoker)
    private
        FFirmware: IFirmware;
    public
		procedure Invoke(const cmd: uint; const ic: IInstructionContext);
		function QueryPrefix(var cmd: uint; const ic: IInstructionContext): Boolean;
        procedure TellFirmware(const fw: IFirmware);
	end;

implementation

type
	TPrefixes = set of byte;
const
	Prefixes: TPrefixes = [ $00, $F1, $F2, $F3 ];

procedure TOdinNCB.Invoke(const cmd: uint; const ic: IInstructionContext);
var
	buf8u:	byte;
	buf8s:	sbyte;
	buf16u:	ushort;
	buf16s:	short;
	buf32u:	uint;
	buf32s:	int;
	buf64u:	ulong;
	buf64s:	long;
label quit, mov01, mov02, mov03, mov04, strout01;
begin
	// Switch
	case cmd of
	$00000001:	goto mov01;
	$00000002:	goto mov02;
    $00000003:  goto mov03;
    $00000004:  goto mov04;
    $0000F101:  goto strout01;
	else goto quit;
	end;
	
	mov01:
	// 0x 01 mov reg, reg
	// Move Reg (64) into Reg (64)
	ic.RegisterU[ic.NextParam] := ic.RegisterU[ic.NextParam];
	goto quit;
	
	mov02:
	// 0x 02 mov reg, int32
	// Move Int (32) -> Reg (32, low); Zero Reg (32, high)
	ic.NextParam(buf8u);
	ic.NextParam(buf32u);
	ic.RegisterU[buf8u] := buf32u;
	WriteLn(buf32u);
	goto quit;
    
    mov03:
    // 0x 03 mov reg, addr32
    // Move Mem (64, at Addr32) into Reg (64)
    // To be continued...
    goto quit;
    
    mov04:
    // 0x 04 mov reg, int64
    // Move Int (64) into Reg (64)
    ic.NextParam(buf8u);
    ic.NextParam(buf64u);
    ic.RegisterU[buf8u] := buf64u;
    Writeln(buf64u);
    goto quit;
    
    strout01:
    // 0x F1 01 strout chr (int8), ..., #0
    // Output String via Firmware
    FFirmware.Interrupt(FFirmware.Lookup(FWL_OUTSTRING),ic);
    goto quit;
	
	// End
quit:end;

function TOdinNCB.QueryPrefix(var cmd: uint; const ic: IInstructionContext): Boolean;
begin
	Result := cmd in Prefixes;
end;

procedure TOdinNCB.TellFirmware(const fw: IFirmware);
begin
    fFirmware := fw;
end;

end.
