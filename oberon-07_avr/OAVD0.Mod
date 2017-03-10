<?xml version="1.0" encoding="UTF-8"?>
<?bluebottle format version="0.1" ?>
<?xml-stylesheet type="text/xsl" href="http://bluebottle.ethz.ch/bluebottle.xsl" ?>
<Text>
<Span style="Normal"><![CDATA[MODULE OAVD0; (*Module used for disassembling, Ling Liu 14.07.07*)
	IMPORT AosFS:=Files, AosIO:=Streams, AosOut:=KernelLog, Commands;
	TYPE
		Asm = RECORD
			op: LONGINT;
			name: ARRAY 32 OF CHAR;
		END;
		
	VAR
			f: ARRAY 19 OF POINTER TO ARRAY OF Asm; (*disassemble tables*)
			
	PROCEDURE lookUpCode(cd: LONGINT; CONST f: ARRAY OF Asm): INTEGER;
		VAR i: INTEGER;
	BEGIN
		i := 0;
		WHILE (i < LEN(f)) & (cd # f[i].op) DO
			INC(i)
		END;
		IF (i < LEN(f)) THEN
			RETURN i
		ELSE
			RETURN -1
		END
	END lookUpCode;
	
	PROCEDURE Hex(x, w: LONGINT; r: AosIO.Writer);
		]]></Span><Span style="Bold"><![CDATA[VAR]]></Span><Span style="Normal"><![CDATA[ i, j: INTEGER;  
			buf: ]]></Span><Span style="Bold"><![CDATA[ARRAY]]></Span><Span style="Normal"><![CDATA[ 10 ]]></Span><Span style="Bold"><![CDATA[OF]]></Span><Span style="Normal"><![CDATA[ CHAR;  
	]]></Span><Span style="Bold"><![CDATA[BEGIN]]></Span><Span style="Normal"><![CDATA[ 
		r.String("0x");
		j :=SHORT( w);
		]]></Span><Span style="Bold"><![CDATA[FOR]]></Span><Span style="Normal"><![CDATA[ i := j - 1 ]]></Span><Span style="Bold"><![CDATA[TO]]></Span><Span style="Normal"><![CDATA[ 0 ]]></Span><Span style="Bold"><![CDATA[BY]]></Span><Span style="Normal"><![CDATA[ -1 ]]></Span><Span style="Bold"><![CDATA[DO]]></Span><Span style="Normal"><![CDATA[ 
			buf[i] := CHR( x ]]></Span><Span style="Bold"><![CDATA[MOD]]></Span><Span style="Normal"><![CDATA[ 10H + 48 );  
			]]></Span><Span style="Bold"><![CDATA[IF]]></Span><Span style="Normal"><![CDATA[ buf[i] > ]]></Span><Span style="AdHoc Oberon 10 2 0 008282FF 00000000"><![CDATA["9"]]></Span><Span style="Normal"><![CDATA[ ]]></Span><Span style="Bold"><![CDATA[THEN]]></Span><Span style="Normal"><![CDATA[ buf[i] := CHR( ORD( buf[i] ) - 48 + 65 - 10 ) ]]></Span><Span style="Bold"><![CDATA[END]]></Span><Span style="Normal"><![CDATA[;  
			x := x ]]></Span><Span style="Bold"><![CDATA[DIV]]></Span><Span style="Normal"><![CDATA[ 10H
		]]></Span><Span style="Bold"><![CDATA[END]]></Span><Span style="Normal"><![CDATA[;  
		buf[j] := 0X;  r.String( buf )
	]]></Span><Span style="Bold"><![CDATA[END]]></Span><Span style="Normal"><![CDATA[ Hex;  
	
	PROCEDURE ListFile*(context: Commands.Context);
	VAR
			input, output: ARRAY 256 OF CHAR;
			ops: ARRAY 19 OF LONGINT; (*op code of 19 formats*)
			opindexs: ARRAY 19 OF INTEGER; (*index opcode in the corresponding codeTab*)
			instr: INTEGER;
			i, j, cd,  src, dst, imm, len: LONGINT;
			FIN, FOUT: AosFS.File; R: AosFS.Reader; W: AosFS.Writer;
	BEGIN
		context.arg.GetString(input);
		context.arg.GetString(output);
		FIN := AosFS.Old(input);
		AosFS.OpenReader(R, FIN, 0);
		IF FIN # NIL THEN
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[	FOUT := AosFS.New(output);
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[	AosFS.OpenWriter(W, FOUT, 0);
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[	len := R.Available();
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[	i := 0;
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[	WHILE i < len DO
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[		R.RawInt(instr);
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[		cd := instr DIV 100H MOD 100H;
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[		cd := cd *100H+instr MOD 100H;
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[		
				ops[0] := (cd DIV 1000H)*10H + (cd DIV 400H MOD 4H); ops[1] := cd DIV 1000H; 
				ops[2] := cd DIV 100H; ops[3] := 0H*10H + (cd DIV 1000H);
				ops[4] := cd; ops[5] := cd DIV 100H; ops[6] := ops[0]; ops[7] := ops[1] *100H + (cd DIV 200H MOD 8H)*10H+ (cd MOD 10H);
				ops[8] := (cd DIV 1000H)*10H + (cd DIV 800H MOD 2H); 
				ops[9] := (cd DIV 4000H)*1000H+(cd DIV 1000H MOD 2H)*100H+ (cd DIV 200H MOD 2H)*10H + (cd DIV 8H MOD 2H);
				ops[10] := ops[2]*100H+(cd DIV 80H MOD 2H)*10H + (cd DIV 8H MOD 2H);
				ops[11] := ops[1]*100H + (cd DIV 400H MOD 4H)*10H + (cd MOD 8H);
				ops[12] := ops[2]*100H + (cd DIV 80H MOD 2H)*10H + (cd MOD 10H);
				ops[13] := ops[1]*100H + (cd DIV 200H MOD 8H)*10H + (cd DIV 8H MOD 2H);
				ops[14] := ops[2]; ops[15] := ops[2]*10H + (cd MOD 10H); ops[16] := cd; 
				ops[17] := ops[7]; 
				ops[18] := ops[1]*100H + (cd DIV 200H MOD 8H)*10H+(cd DIV 2H MOD 8H);			
;
			
				j := 0;
				WHILE (j <=18) DO
					opindexs[j] := lookUpCode(ops[j], f[j]^);
					INC(j)
				END;
				
				IF opindexs[4] # -1 THEN
					W.String(f[4][opindexs[4]].name)
				ELSIF opindexs[16] # -1 THEN
					W.String(f[16][opindexs[16]].name)
				ELSIF opindexs[18] # -1 THEN
					W.String(f[18][opindexs[18]].name);
					W.String(" ");
					R.RawInt(instr);
]]></Span><Span style="Stupid"><![CDATA[			]]></Span><Span style="Normal"><![CDATA[		cd := instr DIV 100H MOD 100H;
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[			cd := cd *100H+instr MOD 100H;
]]></Span><Span style="Stupid"><![CDATA[			]]></Span><Span style="Normal"><![CDATA[		INC(i, 2);
					Hex(cd, 4, W);
				ELSIF opindexs[17] # -1 THEN
					W.String(f[17][opindexs[17]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 20H;
					INC(i, 2);
					R.RawInt(instr);
]]></Span><Span style="Stupid"><![CDATA[			]]></Span><Span style="Normal"><![CDATA[		imm := instr DIV 100H MOD 100H;
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[			imm := imm *100H+instr MOD 100H;
					IF opindexs[17] = 0 THEN
						W.String("r"); W.Int(dst, 1); W.String(", "); Hex(imm,4,W);
					ELSE
						Hex(imm, 4, W); W.String(", "); W.String("r"); W.Int(dst, 1);
					END
				ELSIF opindexs[12] # -1 THEN
					W.String(f[12][opindexs[12]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 8H;
					W.Int(dst, 1)
				ELSIF opindexs[9] # -1 THEN
					W.String(f[9][opindexs[9]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 20H;
					imm := (cd DIV 2000H MOD 2H)*32 + (cd DIV 400H MOD 4H)*8 + (cd MOD 8H);
					IF opindexs[9] = 0 THEN
						W.String("r"); W.Int(dst, 1); W.String(", Z+"); W.Int(imm, 1)
					ELSIF opindexs[9] = 1 THEN
						W.String("r"); W.Int(dst, 1); W.String(", Y+"); W.Int(imm, 1)
					ELSIF opindexs[9] = 2 THEN
						W.String("Z+"); W.Int(imm, 1); W.String(", r"); W.Int(dst, 1)
					ELSIF opindexs[9] = 3 THEN
						W.String("Y+"); W.Int(imm, 1); W.String(", r"); W.Int(dst, 1)
					END
				ELSIF opindexs[10] # -1 THEN
					W.String(f[10][opindexs[10]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 8H;
					src := cd MOD 8H;
					W.String("r"); W.Int(dst, 1); W.String(", r"); W.Int(src, 1)
				ELSIF opindexs[15] # -1 THEN
					W.String(f[15][opindexs[15]].name);
					W.String(" ");
					dst := 16+cd DIV 10H MOD 10H;
					W.String("r"); W.Int(dst, 1)
				ELSIF opindexs[11] # -1 THEN
					W.String(f[11][opindexs[11]].name);
					W.String(" ");
					W.String ("PC");					
					imm := cd DIV 8H MOD 80H;
					IF (imm+1) DIV 40H MOD 2H = 1H THEN
						W.String("-");
						Hex((10000H-(imm+1)) MOD 40H, 4, W)
					ELSE
						W.String("+");
						Hex(imm+1, 4, W)
					END
				ELSIF opindexs[7] # -1 THEN
					W.String(f[7][opindexs[7]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 20H;
					IF (opindexs[7] = 10) OR (opindexs[7] >= 18) THEN
						W.String("r"); W.Int(dst, 1)
					ELSIF (opindexs[7] = 0) OR (opindexs[7] = 5) THEN
						W.String("r"); W.Int(dst, 1); W.String(", Z")
					ELSIF (opindexs[7] = 3) OR (opindexs[7] = 6) THEN
						W.String("r"); W.Int(dst, 1); W.String(", Z+")
					ELSIF opindexs[7] = 4 THEN
						W.String("r"); W.Int(dst, 1); W.String(", -Z")
					ELSIF opindexs[7] = 1 THEN
						W.String("Z, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 2 THEN
						W.String("Y, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 7 THEN
						W.String("r"); W.Int(dst, 1); W.String(", X")
					ELSIF opindexs[7] = 8 THEN
						W.String("r"); W.Int(dst, 1); W.String(", X+")
					ELSIF opindexs[7] = 9 THEN
						W.String("r"); W.Int(dst, 1); W.String(", -X")
					ELSIF opindexs[7] = 11 THEN
						W.String("Z+, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 12 THEN
						W.String("-Z, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 13 THEN
						W.String("Y+, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 14 THEN
						W.String("-Y, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 15 THEN
						W.String("X, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 16 THEN
						W.String("X+, r"); W.Int(dst, 1)
					ELSIF opindexs[7] = 17 THEN
						W.String("-X, r"); W.Int(dst, 1)
					END
				ELSIF opindexs[13] # -1 THEN
					W.String(f[13][opindexs[13]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 20H;
					imm := cd MOD 8H;
					W.String("r"); W.Int(dst, 1); W.String(", "); W.Int(imm, 1)
				ELSIF opindexs[8] # -1 THEN
					W.String(f[8][opindexs[8]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 20H;
					imm := (cd DIV 200H MOD 4H) * 10H + (cd MOD 10H);
					IF opindexs[8] = 0 THEN
						W.String("r"); W.Int(dst, 1); W.String(", "); Hex(imm, 2, W)
					ELSE
						Hex(imm, 2, W); W.String(", r"); W.Int(dst, 1)
					END
				ELSIF (opindexs[6] # -1) & (opindexs[0] # -1) THEN
					dst := cd DIV 10H MOD 20H;
					src := (cd DIV 200H MOD 2H)*10H + (cd MOD 10H);
					IF dst = src THEN
						W.String(f[6][opindexs[6]].name);
						W.String(" ");
						W.String("r");
						W.Int(dst, 1)
					ELSE
						W.String(f[0][opindexs[0]].name);
						W.String(" ");
						W.String("r"); W.Int(dst, 1); W.String(", r"); W.Int(src, 1)
					END
				ELSIF opindexs[6] # -1 THEN
					W.String(f[6][opindexs[6]].name);
					W.String(" ");
					dst := cd MOD 400H;
					W.String("r");
					W.Int(dst, 1)
				ELSIF opindexs[5] # -1 THEN
					W.String(f[5][opindexs[5]].name);
					W.String(" ");
					dst := cd DIV 8H MOD 20H;
					src := cd MOD 8H;
					Hex(dst, 4, W); W.String(", "); W.Int(src, 1)
				ELSIF opindexs[14] # -1 THEN
					W.String(f[14][opindexs[14]].name);
					W.String(" ");
					dst := (cd DIV 10H MOD 10H)*2;
					src := (cd MOD 10H)*2;
					W.String("r"); W.Int(dst, 1); W.String(", r"); W.Int(src, 1)
				ELSIF opindexs[2] # -1 THEN
					W.String(f[2][opindexs[2]].name);
					W.String(" ");
					dst := 24 + (cd DIV 10H MOD 4H)*2;
					imm := cd MOD 10H;
					W.String("r"); W.Int(dst, 1); W.String(", "); W.Int(imm, 1)
				ELSIF opindexs[0] # -1 THEN
					W.String(f[0][opindexs[0]].name);
					W.String(" ");
					dst := cd DIV 10H MOD 20H;
					src := (cd DIV 200H MOD 2H)*10H + (cd MOD 10H);
					W.String("r"); W.Int(dst, 1); W.String(", r"); W.Int(src, 1)
				ELSIF opindexs[3] # -1 THEN
					W.String(f[3][opindexs[3]].name);
					W.String(" ");
					W.String("PC");
					imm := cd MOD 1000H;
					IF (imm+1) DIV 800H MOD 2H = 1H THEN
						W.String("-");
						Hex((10000H-(imm+1)) MOD 1000H, 4, W)
					ELSE
					W.String("+");
					Hex(imm+1, 4, W)
					END
				ELSIF opindexs[1] # -1 THEN
					W.String(f[1][opindexs[1]].name);
					W.String(" ");
					dst := 16 + (cd DIV 10H MOD 10H);
					imm := (cd DIV 100H MOD 10H)*10H + (cd MOD 10H);
					W.String("r"); W.Int(dst, 1); W.String(", "); Hex(imm, 2, W)
				END;
				W.String(" ; "); Hex(i DIV 2, 4, W); W.String(":    "); Hex(cd, 4, W);
				W.Ln();
				INC(i, 2)
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[	END;
]]></Span><Span style="Stupid"><![CDATA[		]]></Span><Span style="Normal"><![CDATA[	]]></Span><Span style="AdHoc Oberon 10 0 0 6000C0FF 00000000"><![CDATA[W.Update();
]]></Span><Span style="Normal"><![CDATA[		]]></Span><Span style="AdHoc Oberon 10 0 0 6000C0FF 00000000"><![CDATA[	AosFS.Register(FOUT)]]></Span><Span style="Normal"><![CDATA[
	ELSE
		AosOut.String("Input file doesn't exist!")
	END
	END ListFile;
	
	PROCEDURE ListArray*(CONST codeArr: ARRAY OF LONGINT; CONST  modname: ARRAY OF CHAR; len: LONGINT);
		VAR
			ops: ARRAY 19 OF LONGINT; (*op code of 19 formats*)
			opindexs: ARRAY 19 OF INTEGER; (*index opcode in the corresponding codeTab*)
			i, j: INTEGER;
			cd, src, dst, imm: LONGINT;
			name: ARRAY 32 OF CHAR;
			F: AosFS.File; R: AosFS.Writer;
	BEGIN
		(*OSAB.MakeFileName(modname, name, ".asm");*)
		 i := 0; j := 0;  (*assume name suffix less than 4 characters*)
		WHILE (i < 32-5) & (modname[i] > 0X) DO name[i] := modname[i]; INC(i) END ;
		name[i]:= '.'; INC(i); name[i]:= 'a'; INC(i); name[i]:= 's'; INC(i); name[i]:= 'm'; INC(i);    
		name[i] := 0X;
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	F := AosFS.New(name);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	AosFS.OpenWriter(R, F, 0);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	i := 0;
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	WHILE i < len DO
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[		cd := codeArr[i];
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[		
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[		
			ops[0] := (cd DIV 1000H)*10H + (cd DIV 400H MOD 4H); ops[1] := cd DIV 1000H; 
			ops[2] := cd DIV 100H; ops[3] := 0H*10H + (cd DIV 1000H);
			ops[4] := cd; ops[5] := cd DIV 100H; ops[6] := ops[0]; ops[7] := ops[1] *100H + (cd DIV 200H MOD 8H)*10H+ (cd MOD 10H);
			ops[8] := (cd DIV 1000H)*10H + (cd DIV 800H MOD 2H); 
			ops[9] := (cd DIV 4000H)*1000H+(cd DIV 1000H MOD 2H)*100H+ (cd DIV 200H MOD 2H)*10H + (cd DIV 8H MOD 2H);
			ops[10] := ops[2]*100H+(cd DIV 80H MOD 2H)*10H + (cd DIV 8H MOD 2H);
			ops[11] := ops[1]*100H + (cd DIV 400H MOD 4H)*10H + (cd MOD 8H);
			ops[12] := ops[2]*100H + (cd DIV 80H MOD 2H)*10H + (cd MOD 10H);
			ops[13] := ops[1]*100H + (cd DIV 200H MOD 8H)*10H + (cd DIV 8H MOD 2H);
			ops[14] := ops[2]; ops[15] := ops[2]*10H + (cd MOD 10H); ops[16] := cd; 
			ops[17] := ops[7]; 
			ops[18] := ops[1]*100H + (cd DIV 200H MOD 8H)*10H+(cd DIV 2H MOD 8H);			
			
			j := 0;
			WHILE (j <=18) DO
				opindexs[j] := lookUpCode(ops[j], f[j]^);
				INC(j)
			END;
			
			IF opindexs[4] # -1 THEN
				R.String(f[4][opindexs[4]].name)
			ELSIF opindexs[16] # -1 THEN
				R.String(f[16][opindexs[16]].name)
			ELSIF opindexs[18] # -1 THEN
				R.String(f[18][opindexs[18]].name);
				R.String(" ");
				INC(i);
				cd := codeArr[i];
				Hex(cd, 4, R)
			ELSIF opindexs[17] # -1 THEN
				R.String(f[17][opindexs[17]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 20H;
				INC(i);
				imm := codeArr[i];
				IF opindexs[17] = 0 THEN
					R.String("r"); R.Int(dst, 1); R.String(", "); Hex(imm,4,R)
				ELSE
					Hex(imm,4,R); R.String(", "); R.String("r"); R.Int(dst, 1);
				END
			ELSIF opindexs[12] # -1 THEN
				R.String(f[12][opindexs[12]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 8H;
				R.Int(dst, 1)
			ELSIF opindexs[9] # -1 THEN
				R.String(f[9][opindexs[9]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 20H;
				imm := (cd DIV 2000H MOD 2H)*32 + (cd DIV 400H MOD 4H)*8 + (cd MOD 8H);
				IF opindexs[9] = 0 THEN
					R.String("r"); R.Int(dst, 1); R.String(", Z+"); R.Int(imm, 1)
				ELSIF opindexs[9] = 1 THEN
					R.String("r"); R.Int(dst, 1); R.String(", Y+"); R.Int(imm, 1)
				ELSIF opindexs[9] = 2 THEN
					R.String("Z+"); R.Int(imm, 1); R.String(", r"); R.Int(dst, 1)
				ELSIF opindexs[9] = 3 THEN
					R.String("Y+"); R.Int(imm, 1); R.String(", r"); R.Int(dst, 1)
				END
			ELSIF opindexs[10] # -1 THEN
				R.String(f[10][opindexs[10]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 8H;
				src := cd MOD 8H;
				R.String("r"); R.Int(dst, 1); R.String(", r"); R.Int(src, 1)
			ELSIF opindexs[15] # -1 THEN
				R.String(f[15][opindexs[15]].name);
				R.String(" ");
				dst := 16+cd DIV 10H MOD 10H;
				R.String("r"); R.Int(dst, 1)
			ELSIF opindexs[11] # -1 THEN
				R.String(f[11][opindexs[11]].name);
				R.String(" ");
				R.String("PC");
				imm := cd DIV 8H MOD 80H;
				IF (imm+1) DIV 40H MOD 2H = 1H THEN
					R.String("-");
					Hex((10000H-(imm+1)) MOD 40H, 4, R)
				ELSE
					R.String("+");
					Hex(imm+1, 4, R)
				END
			ELSIF opindexs[7] # -1 THEN
				R.String(f[7][opindexs[7]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 20H;
				IF (opindexs[7] = 10) OR (opindexs[7] >= 18) THEN
					R.String("r"); R.Int(dst, 1)
				ELSIF (opindexs[7] = 0) OR (opindexs[7] = 5) THEN
					R.String("r"); R.Int(dst, 1); R.String(", Z")
				ELSIF (opindexs[7] = 3) OR (opindexs[7] = 6) THEN
					R.String("r"); R.Int(dst, 1); R.String(", Z+")
				ELSIF opindexs[7] = 4 THEN
					R.String("r"); R.Int(dst, 1); R.String(", -Z")
				ELSIF opindexs[7] = 1 THEN
					R.String("Z, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 2 THEN
					R.String("Y, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 7 THEN
					R.String("r"); R.Int(dst, 1); R.String(", X")
				ELSIF opindexs[7] = 8 THEN
					R.String("r"); R.Int(dst, 1); R.String(", X+")
				ELSIF opindexs[7] = 9 THEN
					R.String("r"); R.Int(dst, 1); R.String(", -X")
				ELSIF opindexs[7] = 11 THEN
					R.String("Z+, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 12 THEN
					R.String("-Z, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 13 THEN
					R.String("Y+, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 14 THEN
					R.String("-Y, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 15 THEN
					R.String("X, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 16 THEN
					R.String("X+, r"); R.Int(dst, 1)
				ELSIF opindexs[7] = 17 THEN
					R.String("-X, r"); R.Int(dst, 1)
				END
			ELSIF opindexs[13] # -1 THEN
				R.String(f[13][opindexs[13]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 20H;
				imm := cd MOD 8H;
				R.String("r"); R.Int(dst, 1); R.String(", "); R.Int(imm, 1)
			ELSIF opindexs[8] # -1 THEN
				R.String(f[8][opindexs[8]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 20H;
				imm := (cd DIV 200H MOD 4H) * 10H + (cd MOD 10H);
				IF opindexs[8] = 0 THEN
					R.String("r"); R.Int(dst, 1); R.String(", "); Hex(imm, 2, R)
				ELSE
					Hex(imm, 2, R); R.String(", r"); R.Int(dst, 1)
				END
			ELSIF (opindexs[6] # -1) & (opindexs[0] # -1) THEN
				dst := cd DIV 10H MOD 20H;
				src := (cd DIV 200H MOD 2H)*10H + (cd MOD 10H);
				IF dst = src THEN
					R.String(f[6][opindexs[6]].name);
					R.String(" ");
					R.String("r");
					R.Int(dst, 1)
				ELSE
					R.String(f[0][opindexs[0]].name);
					R.String(" ");
					R.String("r"); R.Int(dst, 1); R.String(", r"); R.Int(src, 1)
				END
			ELSIF opindexs[6] # -1 THEN
				R.String(f[6][opindexs[6]].name);
				R.String(" ");
				dst := cd MOD 400H;
				R.String("r");
				R.Int(dst, 1)
			ELSIF opindexs[5] # -1 THEN
				R.String(f[5][opindexs[5]].name);
				R.String(" ");
				dst := cd DIV 8H MOD 20H;
				src := cd MOD 8H;
				Hex(dst, 4, R); R.String(", "); R.Int(src, 1)
			ELSIF opindexs[14] # -1 THEN
				R.String(f[14][opindexs[14]].name);
				R.String(" ");
				dst := (cd DIV 10H MOD 10H)*2;
				src := (cd MOD 10H)*2;
				R.String("r"); R.Int(dst, 1); R.String(", r"); R.Int(src, 1)
			ELSIF opindexs[2] # -1 THEN
				R.String(f[2][opindexs[2]].name);
				R.String(" ");
				dst := 24 + (cd DIV 10H MOD 4H)*2;
				imm := cd MOD 10H;
				R.String("r"); R.Int(dst, 1); R.String(", "); R.Int(imm, 1)
			ELSIF opindexs[0] # -1 THEN
				R.String(f[0][opindexs[0]].name);
				R.String(" ");
				dst := cd DIV 10H MOD 20H;
				src := (cd DIV 200H MOD 2H)*10H + (cd MOD 10H);
				R.String("r"); R.Int(dst, 1); R.String(", r"); R.Int(src, 1)
			ELSIF opindexs[3] # -1 THEN
				R.String(f[3][opindexs[3]].name);
				R.String(" ");
				R.String("PC");
				imm := cd MOD 1000H;
				IF (imm+1) DIV 100H MOD 10H = 0FH THEN
					R.String("-");
					Hex((10000H-(imm+1)) MOD 1000H, 4, R)
				ELSE
					R.String("+");
					Hex(imm+1, 4, R)
				END
			ELSIF opindexs[1] # -1 THEN
				R.String(f[1][opindexs[1]].name);
				R.String(" ");
				dst := 16 + (cd DIV 10H MOD 10H);
				imm := (cd DIV 100H MOD 10H)*10H + (cd MOD 10H);
				R.String("r"); R.Int(dst, 1); R.String(", "); Hex(imm, 2, R)
			END;
			R.String(" ; ");
			IF (opindexs[17] # -1) OR (opindexs[18] # -1) THEN
				Hex(i-1, 4, R); R.String(":    "); Hex(codeArr[i-1], 4, R); R.String("    ");
				Hex(i, 4, R); R.String(":    "); Hex(cd, 4, R)
			ELSE
				Hex(i, 4, R); R.String(":    "); Hex(cd, 4, R)
			END;
			R.Ln();
			INC(i)
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	END;
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	]]></Span><Span style="AdHoc Oberon 10 0 0 6000C0FF 00000000"><![CDATA[R.Update();
]]></Span><Span style="Normal"><![CDATA[	]]></Span><Span style="AdHoc Oberon 10 0 0 6000C0FF 00000000"><![CDATA[	AosFS.Register(F)]]></Span><Span style="Normal"><![CDATA[		
	END ListArray;
BEGIN
	NEW(f[0], 12);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][0].op := 01H; f[0][0].name := "CPC"; 
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][1].op := 02H; f[0][1].name := "SBC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][2].op := 03H; f[0][2].name := "ADD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][3].op := 10H; f[0][3].name := "CPSE";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][4].op := 11H; f[0][4].name := "CP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][5].op := 13H; f[0][5].name := "ADC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][6].op := 20H; f[0][6].name := "AND";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][7].op := 21H; f[0][7].name := "EOR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][8].op := 22H; f[0][8].name := "OR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][9].op := 23H; f[0][9].name := "MOV";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][10].op := 12H; f[0][10].name := "SUB";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[0][11].op := 93H; f[0][11].name := "MUL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[1], 6);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[1][0].op := 3H; f[1][0].name := "CPI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[1][1].op := 4H; f[1][1].name := "SBCI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[1][2].op := 5H; f[1][2].name := "SUBI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[1][3].op := 6H; f[1][3].name := "ORI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[1][4].op := 7H; f[1][4].name := "ANDI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[1][5].op := 0EH; f[1][5].name := "LDI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[2], 2);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[2][0].op := 96H; f[2][0].name := "ADIW";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[2][1].op := 97H; f[2][1].name := "SBIW";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[3], 2);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[3][0].op := 0DH; f[3][0].name := "RCALL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[3][1].op := 0CH; f[3][1].name := "RJMP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[4], 28);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][0].op := 0000H; f[4][0].name := "NOP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][1].op := 9408H; f[4][1].name := "SEC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][2].op := 9409H; f[4][2].name := "IJMP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][3].op := 9418H; f[4][3].name := "SEZ";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][4].op := 9419H; f[4][4].name := "EIJMP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][5].op := 9428H; f[4][5].name := "SEN";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][6].op := 9438H; f[4][6].name := "SEV";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][7].op := 9448H; f[4][7].name := "SES";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][8].op := 9458H; f[4][8].name := "SEH";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][9].op := 9468H; f[4][9].name := "SET";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][10].op := 9478H; f[4][10].name := "SEI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][11].op := 9488H; f[4][11].name := "CLC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][12].op := 9498H; f[4][12].name := "CLZ";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][13].op := 94A8H; f[4][13].name := "CLN";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][14].op := 94B8H; f[4][14].name := "CLV";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][15].op := 94C8H; f[4][15].name := "CLS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][16].op := 94D8H; f[4][16].name := "CLH";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][17].op := 94E8H; f[4][17].name := "CLT";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][18].op := 94F8H; f[4][18].name := "CLI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][19].op := 9508H; f[4][19].name := "RET";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][20].op := 9509H; f[4][20].name := "ICALL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][21].op := 9518H; f[4][21].name := "RETI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][22].op := 9519H; f[4][22].name := "EICALL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][23].op := 9588H; f[4][23].name := "SLEEP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][24].op := 95A8H; f[4][24].name := "WDR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][25].op := 95C8H; f[4][25].name := "LPM";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][26].op := 95D8H; f[4][26].name := "ELPM";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[4][27].op := 95E8H; f[4][27].name := "SPM";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[5], 4);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[5][0].op := 98H; f[5][0].name := "CBI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[5][1].op := 99H; f[5][1].name := "SBIC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[5][2].op := 9AH; f[5][2].name := "SBI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[5][3].op := 9BH; f[5][3].name := "SBIS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[6], 4);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[6][0].op := 03H; f[6][0].name := "LSL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[6][1].op := 13H; f[6][1].name := "ROL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[6][2].op := 20H; f[6][2].name := "TST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[6][3].op := 21H; f[6][3].name := "CLR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[7], 27);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][0].op :=0800H; f[7][0].name := "LD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][1].op := 0810H; f[7][1].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][2].op := 0818H; f[7][2].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][3].op := 0901H; f[7][3].name := "LD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][4].op := 0902H; f[7][4].name := "LD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][5].op := 0906H; f[7][5].name := "ELPM";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][6].op := 0907H; f[7][6].name := "ELPM";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][7].op := 090CH; f[7][7].name := "LD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][8].op := 090DH; f[7][8].name := "LD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][9].op := 090EH; f[7][9].name := "LD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][10].op := 090FH; f[7][10].name := "POP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][11].op := 0911H; f[7][11].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][12].op := 0912H; f[7][12].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][13].op := 0919H; f[7][13].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][14].op :=091AH; f[7][14].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][15].op := 091CH; f[7][15].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][16].op := 091DH; f[7][16].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][17].op := 091EH; f[7][17].name := "ST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][18].op := 091FH; f[7][18].name := "PUSH";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][19].op := 0920H; f[7][19].name := "COM";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][20].op := 0921H; f[7][20].name := "NEG";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][21].op := 0922H; f[7][21].name := "SWAP";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][22].op := 0923H; f[7][22].name := "INC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][23].op := 0925H; f[7][23].name := "ASR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][24].op := 0926H; f[7][24].name := "LSR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][25].op := 0927H; f[7][25].name := "ROR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[7][26].op := 092AH; f[7][26].name := "DEC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[8], 2);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[8][0].op := 0B0H; f[8][0].name := "IN";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[8][1].op := 0B1H; f[8][1].name := "OUT";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[9], 4);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[9][0].op := 2000H; f[9][0].name := "LDD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[9][1].op := 2001H; f[9][1].name := "LDD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[9][2].op := 2010H; f[9][2].name := "STD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[9][3].op := 2011H; f[9][3].name := "STD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[10], 4);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[10][0].op := 0300H; f[10][0].name := "MULSU";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[10][1].op := 0301H; f[10][1].name := "FMUL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[10][2].op := 0310H; f[10][2].name := "FMULS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[10][3].op := 0311H; f[10][3].name := "FMULSU";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[11], 18);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][0].op := 0F0H; f[11][0].name := "BRBS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][1].op := 0F1H; f[11][1].name := "BRBC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][2].op := 0F00H; f[11][2].name := "BRLO";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][3].op := 0F01H; f[11][3].name := "BREQ";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][4].op := 0F02H; f[11][4].name := "BRMI";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][5].op := 0F03H; f[11][5].name := "BRVS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][6].op := 0F04H; f[11][6].name := "BRLT";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][7].op := 0F05H; f[11][7].name := "BRHS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][8].op := 0F06H; f[11][8].name := "BRTS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][9].op := 0F07H; f[11][9].name := "BRIE";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][10].op  := 0F10H; f[11][10].name := "BRCC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][11].op := 0F11H; f[11][11].name := "BRNE";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][12].op := 0F12H; f[11][12].name := "BRPL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][13].op := 0F13H; f[11][13].name := "BRVC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][14].op := 0F14H; f[11][14].name := "BRGE";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][15].op := 0F15H; f[11][15].name := "BRHC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][16].op := 0F16H; f[11][16].name := "BRTC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[11][17].op := 0F17H; f[11][17].name := "BRID";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[12], 2);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[12][0].op := 9408H; f[12][0].name := "BSET";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[12][1].op := 9418H; f[12][1].name := "BCLR";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[13], 4);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[13][0].op := 0F40H; f[13][0].name := "BLD";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[13][1].op := 0F50H; f[13][1].name := "BST";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[13][2].op := 0F60H; f[13][2].name := "SBRC";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[13][3].op := 0F70H; f[13][3].name := "SBRS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[14], 2);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[14][0].op := 01H; f[14][0].name := "MOVW";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[14][1].op := 02H; f[14][1].name := "MULS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[15], 1);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[15][0].op := 0EFFH; f[15][0].name := "SER";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[16], 1);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[16][0].op := 0F598H; f[16][0].name := "BREAK";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[17], 2);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[17][0].op :=0900H; f[17][0].name := "LDS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[17][1].op := 0910H; f[17][1].name := "STS";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	NEW(f[18], 2);
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[18][0].op := 0927H; f[18][0].name := "CALL";
]]></Span><Span style="Stupid"><![CDATA[	]]></Span><Span style="Normal"><![CDATA[	f[18][1].op :=0926H; f[18][1].name := "JMP"	
END OAVD0.]]></Span>

</Text>
