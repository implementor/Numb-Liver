{$mode objfpc}

library libOdin;

uses OdinRunner, OdinNCB;

// Dictionary Utils
type 
	TDictRec = record
		hOdin:		ulong;
		pOdin:		TRunner;
		bRemoval:	bool;
	end;
var
	Walhalla: array of TDictRec;
	
function GetOdin(h: ulong): TDictRec;
var i: byte;
begin
	for i := Length(Walhalla)-1 downto 0 do
		if Walhalla[i].hOdin = h then
			GetOdin := Walhalla[i];
end;

procedure CleanWalhalla;
var i, cc: byte; dr: TDictRec;
begin
	for i := 0 to Length(Walhalla)-1 do
		if Walhalla[i].bRemoval and (i < Length(Walhalla)-1) then
		begin
			dr := Walhalla[i];
			Walhalla[i+1] := Walhalla[i];
			Walhalla[i] := dr;
		end;
	i := Length(Walhalla)-1;
	cc := 0;
	repeat
		 dr := Walhalla[i];
		 if dr.bRemoval then Inc(cc);
		 Dec(i);
	until (not dr.bRemoval) or (i < 0);
	if cc > 0 then begin
		SetLength(Walhalla, Length(Walhalla)-cc);
		CleanWalhalla;
	end;
end;

end.
