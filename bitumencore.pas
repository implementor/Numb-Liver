{/    Copyright 2011 Marvin Cohrs
//
//    This file is part of Bitumen.
//
//    Bitumen is free software: you can redistribute it and/or modify
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
{$mode objfpc}{$longstrings on}{$coperators on}

unit bitumencore;

interface

uses prayerlist, sysutils;

type
    TSegment = (segSelected, segData, segResources, segCode);
    TSelSegment = segData .. segCode;
    TBookmark = record
        Name: string;
        At: Longword;
        Seg: TSelSegment;
    end;
    PBookmark = ^TBookmark;
    EBitumen = class(Exception);

procedure ClearInput;
procedure AppendInput(const s: string);
function GetChar: Char;
procedure Match(const c: char);
procedure Match(const s: string);
function GetName: string;
procedure SkipWhite;
function GetInt: QWord;
procedure Error(const s: string);
procedure Expected(const s: string);
procedure Expected(const exp,found: string; const quotes: boolean = false);

procedure Yield(const b: byte; const seg: TSegment = segSelected); overload;
procedure Yield(const b: array of byte; const seg: TSegment = segSelected); overload;
procedure Select(const seg: TSelSegment);

procedure ClearBookmarks;
procedure AddBookmark(const s: string);
function RequestBookmark(const s: string): TBookmark;
function IsBookmark(const s: string): Boolean;

implementation

type
    TByteCode = array of byte;
    TBook = specialize TPrayerList<PBookmark>;

var
    Segment: TSelSegment;
    Look: Char;
    DataSeg, ResSeg, CodeSeg, Binary: TByteCode;
    DataPos, ResPos, CodePos, BinPos: Longword;
    Book: TBook;
    Input: string;
    InPos: Longword;
    
procedure ClearInput;
begin
    Input := '';
end;

procedure AppendInput(const s: string);
begin
    Input += s;
end;

function GetChar: Char;
begin
    if InPos < Length(Input) then
        look := Input[InPos]
    else look := #$03; // <ETX>
    Result := look
end;

procedure Match(const c: char);
begin
    if c<>look then Expected(''''+c+'''',look,true);
    GetChar;
end;

procedure Match(const s: string);
var i: word;
begin
    for i := 1 to length(s) do
        Match(s[i]);
end;

function GetName: string;
var nm: string;
begin
    SkipWhite;
    nm := '';
    if not (look in ['A'..'Z','a'..'z','_']) then
        Expected('Name',look,true);
    while look in ['A'..'Z','a'..'z','_','0'..'9'] do begin
        nm += look;
        GetChar;
    end;
    Result := nm;
    SkipWhite;
end;

procedure SkipWhite;
begin
    while look in [#$20, #$09, #$0A, #$0D] do GetChar;
end;

function GetInt: QWord;
var
    base, s, e, i: byte;
    str: string;
    valid: set of char;
    function Digit(const c: char): byte;
    begin
        if (c>='0') and (c<='9') then
            Result := Ord(c)-Ord('0')
        else if (c>='A') and (c<='F') then
            Result := Ord(c)-Ord('A')+10
        else if (c>='a') and (c<='f') then
            Result := Ord(c)-Ord('a')+10
    end;
begin
    SkipWhite;
    str := '';
    if not (look in ['$','0'..'9']) then
        Expected('Integer',look,true);
    while look in ['$','A'..'F','a'..'f','h','H','o','O','0'..'9','x','X'] do begin
        str += look;
        GetChar;
    end;
    if str[1] = '$' then begin
        base := 16;
        s := 2; e := length(str);
    end else if (length(str)>1) and (str[1]='0') and (str[2] in ['x','X']) then begin
        base := 16;
        s := 3; e := length(str);
    end else if str[length(str)] in ['b','B'] then begin
        base := 2;
        s := 1; e := length(str)-1;
    end else if str[length(str)] in ['x','X','h','H'] then begin
        base := 16;
        s := 1; e := length(str)-1;
    end else if str[length(str)] in ['o','O'] then begin
        base := 8;
        s := 1; e := length(str)-1;
    end else if str[length(str)] in ['d','D'] then begin
        base := 10;
        s := 1; e := length(str)-1;
    end else begin
        base := 10;
        s := 1; e := length(str);
    end;
    case base of
    2:  valid := ['0','1'];
    8:  valid := ['0'..'7'];
    10: valid := ['0'..'9'];
    16: valid := ['0'..'9','A'..'F','a'..'f'];
    end;
    Result := 0;
    for i := s to e do begin
        if not (str[i] in valid) then Expected('Digit',str[i],true);
        Result := Result * base + Digit(str[i]);
    end;
end;

procedure Error(const s: string);
begin
    raise EBitumen.Create(s);
end;

procedure Expected(const s: string);
begin
    Error(s+' expected.');
end;

procedure Expected(const exp, found: string; const quotes: boolean = false);
begin
    if quotes then
        Error(exp+' expected, but '''+found+''' found.')
    else Error(exp+' expected, but '+found+' found.')
end;

procedure Yield(const b: byte; const seg: TSegment = segSelected);
var segx: TSelSegment;
begin
    if seg=segSelected then segx := Segment
    else segx := seg;
    case segx of
    segData:    begin
                    if DataPos=Length(DataSeg) then
                        SetLength(DataSeg, Length(DataSeg)+48);
                    DataSeg[DataPos] := b;
                    Inc(DataPos);
                end;
    segCode:    begin
                    if CodePos=Length(CodeSeg) then
                        SetLength(CodeSeg, Length(CodeSeg)+48);
                    CodeSeg[CodePos] := b;
                    Inc(CodePos);
                end;
    segResources: begin
                      if ResPos=Length(ResSeg) then
                          SetLength(ResSeg, Length(ResSeg)+48);
                      ResSeg[ResPos] := b;
                      Inc(ResPos);
                  end;
    end;
end;

procedure Yield(const b: array of byte; const seg: TSegment = segSelected);
var segx: TSelSegment; i: word;
begin
    if seg=segSelected then segx := Segment
    else segx := seg;
    for i := 0 to length(b)-1 do
        Yield(b[i],segx)
end;

procedure Select(const seg: TSelSegment);
begin
    Segment := seg;
end;

procedure ClearBookmarks;
var i: word;
begin
    for i := 0 to Book.Count-1 do
        Dispose(Book[i]);
    Book.Clear;
    Book.Shrink;
end;

procedure AddBookmark(const s: string);
var p: PBookmark;
begin
    New(p);
    p^.Name := s;
    p^.Seg := Segment;
    case Segment of
    segCode:        p^.At := CodePos;
    segData:        p^.At := DataPos;
    segResources:   p^.At := ResPos;
    end;
    Book.Add(p);
end;

function RequestBookmark(const s: string): TBookmark;
var i: word;
begin
    Result.Name := '-- not found';
    Result.At := High(Longword);
    Result.Seg := segData;
    for i := 0 to Book.Count-1 do
        if Book[i]^.Name=s then
            Result := Book[i]^;
end;

function IsBookmark(const s: string): Boolean;
var i: word;
begin
    Result := false;
    for i := 0 to Book.Count-1 do
        Result := Result or (Book[i]^.Name=s);
end;

initialization
    Book := TBook.Create(0, nil);
    Segment := segCode;
    look := #10;
    Input := '';
    InPos := 0;
    SetLength(DataSeg, 0); SetLength(ResSeg, 0);
    SetLength(CodeSeg, 0); SetLength(Binary, 0);
    DataPos := 0; ResPos := 0;
    CodePos := 0; BinPos := 0;
finalization
    ClearBookmarks;
    Book.Free;
    SetLength(DataSeg, 0); SetLength(ResSeg, 0);
    SetLength(CodeSeg, 0); SetLength(Binary, 0);
end.
