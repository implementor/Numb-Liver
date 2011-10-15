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
**             NIOS.pas                                              **
**             Odin VM - Nano Input Output System                    **
**             (c) 2011 Marvin Cohrs                                 **
**             Dev. start: Fri 14 Oct 2011                           **
**********************************************************************)

unit NIOS;

interface

uses OdinRunner;

type
    TNIOS = class(TInterfacedObject, IFirmware)
        procedure Interrupt(const iid: ushort; const iic: IInstructionContext);
        function Lookup(const fwlookup: tfwlookup): ushort;
    end;

implementation

procedure TNIOS.Interrupt(const iid: ushort; const iic: IInstructionContext);
label iid_outs, iid_ins, quit;
begin
    case iid of
    $7A:    goto iid_outs;
    $7C:    goto iid_ins;
    end;
    
    iid_outs:
    Writeln('OUTSTRING');
    goto quit;
    
    iid_ins:
    Writeln('INSTRING');
    goto quit;
    
quit:end;

function TNIOS.Lookup(const fwlookup: tfwlookup): ushort;
begin
    case fwlookup of
    FWL_OUTSTRING:      Result := $7A;
    FWL_INSTRING:       Result := $7C;
    FWL_RANDOM:         Result := $90;
    else Result := $FFFF;
    end;
end;

end.
