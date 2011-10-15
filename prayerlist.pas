{/    Copyright 2011 Marvin Cohrs
//
//    This file is part of Prayer.
//
//    Prayer is free software: you can redistribute it and/or modify
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

unit prayerlist;

interface

// List
type
	generic TPrayerList<T> = class
	public
		type
			TArray = array of T;
		var
	private
		FData: TArray;
		FLength: Integer;
		FInitial: T;
		procedure SetElement(const i: integer; const val: T);
		function GetElement(const i: integer): T;
		function GetIndex(const item: T): Integer;
	public
		// Initialisiert das Feld
		constructor Create(const Data: TArray; const Initial: T);
		constructor Create(const Count: Integer; const Initial: T);
		// Setzt die Feldeinträge zurück
		procedure Clear;
		// Feldzugriff
		property Elements[i: Integer]: T read GetElement write SetElement; default;
		// Feldlänge
		property Count: Integer read FLength;
		// Initialwert
		property Initial: T read FInitial;
		// Hinzufügen von Werten
		procedure Add(const Item: T);
		procedure AddRange(const Items: array of T);
		procedure Insert(const Item: T; const At: Integer);
		procedure InsertRange(const Items: array of T; const At: Integer);
		procedure Grow(const c: integer);
		// Löschen von Werten
		procedure Remove(const Item: T);
		procedure RemoveAt(const Index: Integer);
		procedure RemoveRange(const First, Last: Integer);
		procedure Shrink;
		// Suchen von Werten
		property Index[item: T]: Integer read GetIndex;
	end;

implementation

// Listenkonstruktor
constructor TPrayerList.Create(const Data: TArray; const Initial: T);
begin
	// Vererbter Konstruktor
	inherited Create;
	// Zuweisen der Felder
	FData := Data;
	FLength := Length(Data);
	FInitial := Initial
end;

constructor TPrayerList.Create(const Count: Integer; const Initial: T);
begin
	// Vererbter Konstruktor
	inherited Create;
	// Zuweisen der Felder
	SetLength(FData, Count);
	FLength := Count;
	FInitial := Initial;
	Clear
end;

// Setzt die Feldeinträge zurück
procedure TPrayerList.Clear;
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		FData[i] := Initial;
    FLength := 0;
end;

// Feldzugriff
procedure TPrayerList.SetElement(const i: integer; const val: T);
begin
	FData[i] := val
end;

function TPrayerList.GetElement(const i: integer): T;
begin
	Result := FData[i]
end;

// Suchen von Werten
function TPrayerList.GetIndex(const item: T): Integer;
var
	i: Integer;
begin
	for i := 0 to Count-1 do
		if T(FData[i]) = T(item) then
			Result := i
end;

// Hinzufügen von Werten
procedure TPrayerList.Add(const item: T);
begin
    if length(FData)=FLength then
        SetLength(FData, FLength+1);
	FData[FLength] := item;
	Inc(FLength);
end;

procedure TPrayerList.AddRange(const items: array of T);
var
	rlen, i: integer;
begin
	rlen := Length(items);
	SetLength(FData, FLength+rlen);
	for i := 0 to rlen-1 do
		FData[FLength+i] := items[i];
	Inc(FLength,rlen)
end;

procedure TPrayerList.Insert(const item: T; const at: Integer);
var
	current, next: t;
	i: integer;
begin
	Add(Initial);
	current := item;
	for i := At to Count-1 do
	begin
		next := FData[i];
		FData[i] := current;
		current := next
	end
end;

procedure TPrayerList.InsertRange(const items: array of T; const at: Integer);
var
	i: integer;
begin
	for i := 0 to Length(items) do
		Insert(items[i], at+i)
end;

// Löschen von Werten
procedure TPrayerList.Remove(const item: T);
begin
	RemoveAt(Index[item])
end;

procedure TPrayerList.RemoveAt(const index: integer);
var
	current, next: t;
	i: integer;
begin
	current := Initial;
	for i := Count-1 downto index do
	begin
		next := FData[i];
		FData[i] := current;
		current := next
	end;
	//SetLength(FData, Count-1);
	Dec(FLength)
end;

procedure TPrayerList.RemoveRange(const first, last: integer);
var
	i: integer;
begin
	for i := last downto first do
		RemoveAt(i)
end;

// Vergrößern/Verkleinern
procedure TPrayerList.Grow(const c: integer);
var
	i: integer;
begin
	SetLength(FData, Count+c);
	for i := 0 to count - 1 do
		FData[FLength+i] := Initial;
end;

procedure TPrayerList.Shrink;
begin
	SetLength(FData, FLength);
end;

end.
