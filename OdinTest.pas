program OdinTest;

uses OdinRunner, OdinNCB, NIOS;

const
	Code: array[0..11] of byte = (
		$04, $01, $10, $00, $00, $00, $00, $00, $00, $00, $F1, $01
	);

var
	ivk: IInvoker;
    fw: IFirmware;
	rnr: TRunner;
	irc: IRunningContext;
	dat: TData;
	i: ulong;

begin
	rnr := TRunner.Create();
	ivk := TOdinNCB.Create();
    fw := TNIOS.Create;
	SetLength(dat, Length(code));
	for i := Low(code) to High(code) do
		dat[i] := Code[i];
	irc := rnr.Feed(dat, ivk, fw);
	irc.Run();
    ivk := nil;
    fw := nil;
    rnr.Free;
end.
