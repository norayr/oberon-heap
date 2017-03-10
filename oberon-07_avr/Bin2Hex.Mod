MODULE Bin2Hex;

IMPORT AosFS:=Files, Commands, AosIO:=Streams;

PROCEDURE Byte (VAR w: AosFS.Writer; val: LONGINT);
BEGIN w.Hex (val MOD 100H, -2);
END Byte;

PROCEDURE ExtendedAddressRecord* (VAR w: AosFS.Writer; extadr: LONGINT);
BEGIN
		w.Char (':'); Byte (w, 2); Byte (w, 0); Byte (w, 0); Byte (w, 4);
		Byte (w, extadr DIV 100H); Byte (w, extadr);
		Byte (w, 100H - (2 + 0 + 0 + 4 + extadr DIV 100H MOD 100H + extadr MOD 100H) MOD 100H); w.Ln;
END ExtendedAddressRecord;

PROCEDURE DataRecord* (VAR w: AosFS.Writer; CONST data: ARRAY OF CHAR; len, offset: LONGINT);
VAR checksum, i: LONGINT;
BEGIN
	checksum := len MOD 100H;
	INC (checksum, offset DIV 100H MOD 100H + offset MOD 100H);
	w.Char (':'); Byte (w, len); Byte (w, offset DIV 100H); Byte (w, offset); Byte (w, 0);
	FOR i := 0 TO len - 1 DO Byte (w, ORD (data[i])); INC (checksum, ORD (data[i])) END;
	Byte (w, 100H - checksum MOD 100H); w.Ln;
END DataRecord;

PROCEDURE EndOfFileRecord* (VAR w: AosFS.Writer);
BEGIN w.Char (':'); Byte (w, 0); Byte (w, 0); Byte (w, 0); Byte (w, 1); Byte (w, 255); w.Ln;
END EndOfFileRecord;

PROCEDURE ConvertFile* (CONST source: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR; offset, maxlen: LONGINT);
VAR src, dst: AosFS.File; r: AosFS.Reader; w: AosFS.Writer; extadr: LONGINT;
	c: CHAR; len: LONGINT; data: ARRAY 255 OF CHAR;
BEGIN
	src := AosFS.Old (source); dst := AosFS.New (dest);
	AosFS.OpenReader (r, src, 0); AosFS.OpenWriter (w, dst, 0);
	
	extadr := offset DIV 10000H;
	IF extadr # 0 THEN ExtendedAddressRecord (w, extadr); END;

	REPEAT	
		len := 0;
		LOOP
			r.Char (c); IF r.res # AosFS.Ok THEN EXIT END;
			data[len] := c; INC (len); IF len = maxlen THEN EXIT END;
		END;
		
		IF len # 0 THEN DataRecord (w, data, len, offset) END;

		INC (offset, len);
		IF offset DIV 10000H # extadr THEN
			extadr := offset DIV 10000H;
			ExtendedAddressRecord (w, extadr);
		END;
	UNTIL r.res # AosFS.Ok;
		
	EndOfFileRecord (w);

	w.Update ();
	AosFS.Register (dst);

END ConvertFile;

PROCEDURE Convert* (context: Commands.Context);
VAR 
	source, dest: ARRAY AosFS.NameLength OF CHAR;
	offset, maxlen: LONGINT;
BEGIN
	context.arg.GetString (source);
	context.arg.GetString (dest);
	context.arg.GetInteger (offset, TRUE); IF context.arg.res # AosIO.Ok THEN offset := 0 END;
	context.arg.GetInteger (maxlen, TRUE); IF context.arg.res # AosIO.Ok THEN maxlen := 255 END;
	ConvertFile (source, dest, offset, maxlen)
END Convert;

END Bin2Hex.
