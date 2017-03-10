MODULE Image;

IMPORT File, VR := VDIRaster, SYSTEM;

TYPE
  HeaderType = RECORD
    Version,
    Length,
    Planes,
    PatternLength,
    PixelWidth,
    PixelHeight,
    LineWidth,
    Lines : INTEGER; (* 2 Bytes Typ! *)
    XIMGID : ARRAY 4 OF CHAR;
    ColorModel : INTEGER;
  END;
  PtrHeaderType = POINTER TO HeaderType;

  RGBListType = RECORD
    red, green, blue : INTEGER;
  END;

  FilePtr = POINTER TO ARRAY MAX(LONGINT) OF CHAR;


PROCEDURE Decompress (img : PtrHeaderType; dest : FilePtr);
VAR repetitions, count, k, l, p, z, LineBytes, LineBytes2 : INTEGER;
    source : FilePtr;
    i, j, off, PlaneBytes : LONGINT;
BEGIN
  source := SYSTEM.VAL(LONGINT,img) + 2 * img^.Length;
  LineBytes := (img^.LineWidth + 7) DIV 8;
  LineBytes2 := (img^.LineWidth + 15) DIV 16 * 2;
  PlaneBytes := LONG(LineBytes2) * LONG(img^.Lines);
  i := 0; z := 0;

  REPEAT (* FÅr jede Zeile *)

    repetitions := 1;
    IF (source^[i] = CHR(0)) AND (source^[i+1] = CHR(0)) AND
       (source^[i+2] = CHR(255)) THEN
      repetitions := ORD(source^[i+3]);
      INC(i, 4);
    END;

    FOR p := 0 TO img^.Planes-1 DO (* FÅr jede Farbebene *)
      off := LONG(p) * PlaneBytes + LONG(z) * LONG(LineBytes2);
      j := 0;
      REPEAT
        IF source^[i] = CHR(0) THEN (* Pattern Run *)
          INC(i); count := ORD(source^[i]);
          ASSERT(count = 0, 100);
          FOR k := 1 TO count DO
            FOR l := 1 TO img^.PatternLength DO
              dest^[j+off] := source^[i+LONG(l)]; INC(j);
            END;
          END;
          INC(i, img^.PatternLength + 1);
        ELSIF source^[i] = CHR(128) THEN (* Bitstring *)
          INC(i); count := ORD(source^[i]); INC(i);
          FOR k := 1 TO count DO
            dest^[j+off] := source^[i]; INC(j); INC(i);
          END;
        ELSIF source^[i] < CHR(128) THEN (* Solid Run mit 0 *)
          count := ORD(source^[i]) MOD 128; INC(i);
          FOR k := 1 TO count DO
            dest^[j+off] := CHR(0); INC(j);
          END;
        ELSE (* Solid Run mit 1 *)
          count := ORD(source^[i]) MOD 128; INC(i);
          FOR k := 1 TO count DO
            dest^[j+off] := CHR(255); INC(j);
          END;
        END;
      UNTIL j >= LONG(LineBytes);
      ASSERT(j = LONG(LineBytes), 101);
    END; (* FOR p ... *)

    INC(z);

    WHILE repetitions > 1 DO
      DEC(repetitions);
      FOR p := 0 TO img^.Planes-1 DO
        off := LONG(p) * PlaneBytes + LONG(z) * LONG(LineBytes2);
        FOR k := 0 TO LineBytes2-1 DO
          dest^[off+LONG(k)] := dest^[off+LONG(k)-LONG(LineBytes2)];
        END;
      END;
      INC(z);
    END;

  UNTIL z >= img^.Lines;
  ASSERT(z = img^.Lines, 102);
END Decompress;


PROCEDURE Load* (Filename : ARRAY OF CHAR; VAR raster : VR.mfdbrec);
VAR img : PtrHeaderType;
    PicSize, FileLength : LONGINT;
    Start : POINTER TO ARRAY OF CHAR;
BEGIN
  IF File.Load(Filename, 0, 0, img, FileLength) THEN
    PicSize := LONG((img^.LineWidth + 15) DIV 16 * 2) * 
               LONG(img^.Lines) * LONG(img^.Planes);
    SYSTEM.NEW(Start, PicSize); raster.Addr := SYSTEM.VAL(LONGINT, Start);
    raster.Width := img^.LineWidth;
    raster.Height := img^.Lines;
    raster.WidthW := (raster.Width + 15) DIV 16;
    raster.Format := 0;
    raster.Planes := img^.Planes;

    Decompress (img, raster.Addr);
  END;
END Load;


END Image.