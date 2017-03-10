MODULE Thrust;

IMPORT Math, IO, Strings, NumStr, File,

       VC := VDIControl, VA := VDIAttributes, VO := VDIOutput,
       VR := VDIRaster, VI := VDIInquiry,

       GemApp, Graf, Evnt, Wind, Form, Menus, Rsrc, Dialogs, Objc,

       (* Debug, Halt, *)

       XBIOS, GEMDOS, SYSTEM, LinkedList, Memory, Sys, Task,
       
       Timer, Image, IWinView, WindowDialog;

CONST

      (* Resource Datei Indizes fÅr THRUST *)
    
    MENU     =   0;    (* Menuebaum *)
    THRUST   =   3;    (* TITLE in Baum MENU *)
    SPIEL    =   4;    (* TITLE in Baum MENU *)
    INFO     =   7;    (* STRING in Baum MENU *)
    START    =  16;    (* STRING in Baum MENU *)
    ENDE     =  17;    (* STRING in Baum MENU *)

    ABOUT    =   1;    (* Formular/Dialog *)
    ICON     =   3;    (* IMAGE in Baum ABOUT *)

    BILD     =   2;    (* Formular/Dialog *)


      (*****************)

      pi       = Math.pi;
      twopi    = 2 * pi;
      radtodeg = 57; (* 360 / twopi *)

      fixcomma = 4096;  (* 12 Nachkommastellen *)
      nangle   = 360;   (* Anzahl der Winkel (in Grad) *)
      nangle2  = 360 * fixcomma;

      abstand     = 40; (* Abstand Raumschiff - Kugel in Pixel *)
      abstand2    = abstand * abstand; (* abstand hoch 2 *)
      ballmasse   = 3; (* Masse der Kugel (relativ zur Raumschiffmasse) *)
      gesamtmasse = ballmasse + 1;
      distball    = abstand DIV gesamtmasse; (* Distanz Schwerpunkt-Ball *)
      distschiff  = abstand * ballmasse DIV gesamtmasse;
      traegschiff = distschiff * distschiff * 1;
      traeggesamt = traegschiff + distball * distball * ballmasse;

      NoHit = 0;
      HitBackground = 1;
      HitExit = 2;
      HitShip = 3;
      HitEsc = 4;

      WindowTitle = " Thrust 0.5 ";

TYPE

  Sprite = POINTER TO SpriteDesc;
  SpriteDesc = RECORD
                 shape : ARRAY 32 OF SET; (* maximal 32 x 32 Pixel *)
                 w, h : INTEGER; (* Breite, Hîhe *)
               END;
  Bitmap = POINTER TO BitmapDesc;
  BitmapDesc = RECORD
                 width, height, widthSet : INTEGER;
                 size : LONGINT;
                 mem, col : POINTER TO ARRAY MAX(LONGINT) OF SET;
                 visible, update : VR.pxyarray;
                 mfdb : VR.mfdbrec;
                 RandX, RandY : INTEGER; (* ab wo wird gescrollt? *)
               END;

  Point = POINTER TO PointDesc;
  PointDesc = RECORD(LinkedList.ElemDesc)
                x, y : LONGINT; (* Position *)
                vx, vy : LONGINT; (* Geschwindigkeit *)
                sx, sy : INTEGER; (* Zeichenkoordinaten *)
                hit : INTEGER; (* getroffen ? *)
              END;

  Explosion = POINTER TO ExplosionDesc;
  ExplosionDesc = RECORD(PointDesc)
                    shape : Sprite;
                    LastShape : LONGINT;
                  END;

  Ship = POINTER TO ShipDesc;
  ShipDesc = RECORD(PointDesc)
               beta  : LONGINT; (* Schiffswinkel *)
               sb : INTEGER; (* Zeichen-Winkel *)
               shape, shapet : ARRAY 90 OF Sprite;
               thrust, last : BOOLEAN;
               LastShoot, ShootFreq : LONGINT;
             END;

  Ball = POINTER TO BallDesc;
  BallDesc = RECORD(PointDesc);
               shape : Sprite;
               loading : BOOLEAN;
               bx , by : INTEGER; (* Schiffskoordinaten *)
             END;

  LoadedShip = POINTER TO LoadedShipDesc;
  LoadedShipDesc = RECORD(ShipDesc)
                     alpha,
                     valpha, valphaconst : LONGINT;
                     mx, my, lx, ly : INTEGER; (* Stange *)
                     ball : Sprite;
                   END;

  Defender = POINTER TO DefenderDesc;
  DefenderDesc = RECORD(PointDesc)
                   shape : Sprite;
                   StartAngle, StopAngle : INTEGER;
                   LastShoot, ShootFreq, Range : LONGINT;
                 END;

  Level = POINTER TO LevelDesc;
  LevelDesc = RECORD
                image : ARRAY 128 OF CHAR;
                corners, ballx, bally, shipx, shipy : INTEGER;
              END;

  scTab = ARRAY nangle OF LONGINT;

  myViewer = POINTER TO myViewDesc;
  myViewDesc = RECORD(IWinView.IViewDesc);
               END;


VAR sin, cos : scTab; (* sinus/cosinus Tabelle *)

    Station : INTEGER;
    Workout : VC.workout;
    Color : BOOLEAN; (* Farbbildschirm *)
    win : myViewer; (* Ausgabefenster *)
    InfoDialog : WindowDialog.Dialog;
    menu : Menus.Menu;

    Screen : VR.mfdbrec;
    VScreen : Bitmap;
    Collision : BOOLEAN;

    objects : LinkedList.Ptr; (* aktuelle Objekte *)
    nobj, ende : INTEGER;
    debug : BOOLEAN;

    myShip : Ship;
    myBall : Ball;
    myLoadedShip : LoadedShip;
    myLevel : Level;
    myDefender : Defender;

    dt : LONGINT; (* Zeitschritt *)
    AccStep, BetaStep, GravStep : LONGINT; (* Schrittweite fÅr Beschl. u. Winkel *)
    Gravitation : BOOLEAN;
    HitCount : INTEGER;
    
(* Allgemeine Hilfsprozeduren *******************************************)

PROCEDURE InitTables ();
VAR i, l : LONGINT; r : REAL;
BEGIN
  FOR i := 0 TO nangle - 1 DO
    r := i;
    sin[i] := ENTIER(Math.sin(r * 0.0174533) * fixcomma);
    cos[i] := ENTIER(Math.cos(r * 0.0174533) * fixcomma);
  END;
(*
  IO.WriteString("InitTables finished!"); IO.WriteLn;
*)
(*
  l := SIZE(scTab);
  IF File.LoadTo("SINUS.TAB", SYSTEM.ADR(sin), l) THEN


    IO.WriteString("SINUS.TAB written!"); IO.WriteLn();
  END;
  IF File.LoadTo("COSIN.TAB", SYSTEM.ADR(cos), l) THEN
    IO.WriteString("COSIN.TAB written!"); IO.WriteLn();
  END;
*)
  Screen.Addr := 0; (* Bildschirm *)
  HitCount := 0;
END InitTables;

PROCEDURE ComparePos (str1, str2 : ARRAY OF CHAR; pos : INTEGER) : BOOLEAN;
  VAR
    i : INTEGER;
 BEGIN
  i := 0;
  WHILE str1[i] = str2[pos] DO
    INC(i); INC(pos);
    IF str1[i] = 0X THEN RETURN TRUE END(*IF*);
  END(*WHILE*);
  RETURN FALSE
 END ComparePos;

PROCEDURE I (s : ARRAY OF CHAR; a,b,c,d : LONGINT; Wait : BOOLEAN);
BEGIN
  IO.WriteString(s);
  IO.WriteInt(a); IO.WriteString(" ");
  IO.WriteInt(b); IO.WriteString(" ");
  IO.WriteInt(c); IO.WriteString(" ");
  IO.WriteInt(d); IO.WriteString(" ");
  IO.WriteLn;
END I;

PROCEDURE R (s : ARRAY OF CHAR; r1,r2,r3,r4 : REAL; Wait : BOOLEAN);
VAR ch : CHAR;
BEGIN
  IO.WriteString(s);
  IO.WriteReal(r1, 2, 0, 10, ' ');
  IO.WriteReal(r2, 2, 0, 10, ' ');
  IO.WriteReal(r3, 2, 0, 10, ' ');
  IO.WriteReal(r4, 2, 0, 10, ' '); IO.WriteLn;
  IF Wait THEN
    ch := IO.ReadChar();
  END;
END R;

PROCEDURE M (name : ARRAY OF CHAR; m : VR.mfdbrec);
VAR ch : CHAR;
BEGIN
  IO.WriteString(name); IO.WriteLn;
  IO.WriteString("MFDB.Addr = "); IO.WriteInt(m.Addr); IO.WriteLn;
  IO.WriteString("     Width = "); IO.WriteInt(m.Width); IO.WriteLn;
  IO.WriteString("     Height = "); IO.WriteInt(m.Height); IO.WriteLn;
  IO.WriteString("     WidthW = "); IO.WriteInt(m.WidthW); IO.WriteLn;
  IO.WriteString("     Format = "); IO.WriteInt(m.Format); IO.WriteLn;
  IO.WriteString("     Planes = "); IO.WriteInt(m.Planes); IO.WriteLn;
END M;


PROCEDURE B (s : ARRAY OF CHAR; x : SET; LF : BOOLEAN);
VAR i : INTEGER;
BEGIN
  IO.WriteString(s);
  FOR i := 31 TO 0 BY -1 DO
    IF i IN x THEN
      IO.WriteChar("o");
    ELSE
      IO.WriteChar(".");
    END;
  END;
  IF LF THEN
    IO.WriteLn();
  END;
END B;

(* Prozeduren auf Levels ************************************************)

PROCEDURE (l : Level) Init ();
VAR length, count : LONGINT;
    start : POINTER TO ARRAY OF CHAR;
    t : ARRAY 255 OF CHAR;
    Comment : INTEGER;
    b : BOOLEAN;
    
  PROCEDURE GetToken (VAR s : ARRAY OF CHAR) : BOOLEAN;
  VAR i : INTEGER;
  BEGIN
    WHILE (count < length) AND (start^[count] <= " ") DO 
      INC(count);
    END;
    IF count >= length THEN RETURN FALSE END;
    i := 0;
    WHILE (count < length) AND (start^[count] > " ") DO 
      s[i] := start^[count]; INC(i); INC(count);
    END;
    s[i] := CHR(0);
    RETURN TRUE;
  END GetToken;

  PROCEDURE GetNumber () : LONGINT;
  VAR s : ARRAY 128 OF CHAR;
  BEGIN
    IF GetToken(s) THEN
      RETURN NumStr.ToLInt(10, s);
    END;
    RETURN 0;
  END GetNumber;

BEGIN
  l.image := "level.img"; count := 0;

  IF ~File.Load ("level1.lev", 0, 1, start, length) THEN
    RETURN 
  END;

  NEW(objects); objects.Initialize(); (* Liste intialisieren *)

  Comment := 0;
  WHILE GetToken(t) DO
    IF ComparePos(t, "(*", 0) THEN
      INC(Comment);
    ELSIF ComparePos(t, "*)", 0) THEN
      DEC(Comment);
      IF Comment < 0 THEN Comment := 0 END;
    END;
    IF Comment = 0 THEN
      IF ComparePos(t, "image", 0) THEN
        b := GetToken(l.image);
        NEW(VScreen); VScreen.Init(l.image);
      ELSIF ComparePos(t, "ship", 0) THEN
        NEW(myShip);
        myShip.Init (GetNumber() * fixcomma, GetNumber() * fixcomma, 0, 0);
        objects.Insert(myShip);
      ELSIF ComparePos(t, "ball", 0) THEN
        NEW(myBall);
        myBall.Init(GetNumber() * fixcomma, GetNumber() * fixcomma, 0, 0);
        objects.Insert(myBall);
      ELSIF ComparePos(t, "defender", 0) THEN
        NEW(myDefender);
        myDefender.Init(GetNumber() * fixcomma, GetNumber() * fixcomma, 0, 0);
        objects.Insert(myDefender);
      END;
    END;
  END;
  
(*
  NEW(objects); objects.Initialize(); (* Liste intialisieren *)

  NEW(VScreen); VScreen.Init("level1.img");

  NEW(myShip);
  myShip.Init (550 * fixcomma, 50 * fixcomma, 0, 0);
  objects.Insert(myShip);

  NEW(myBall);
  myBall.Init(400 * fixcomma, 700 * fixcomma, 0, 0);
  objects.Insert(myBall);

  NEW(myDefender);
  myDefender.Init(100 * fixcomma, 250 * fixcomma, 0, 0);
  objects.Insert(myDefender);
  
  NEW(myDefender);
  myDefender.Init(500 * fixcomma, 550 * fixcomma, 0, 0);
  objects.Insert(myDefender);
*)
END Init;

(* Prozeduren auf Sprites ***********************************************)

PROCEDURE (s : Sprite) Init (w, h : INTEGER);
VAR i : INTEGER;
BEGIN
  s.w := w; s.h := h;
  FOR i := 0 TO 31 DO
    s.shape[i] := {};
  END;
END Init;

PROCEDURE (s : Sprite) Circle (x, y, r : INTEGER);
VAR i, j : INTEGER;
BEGIN
  FOR i := 0 TO 31 DO
    FOR j := 0 TO 31 DO
      IF (i-y)*(i-y) + (j-x)*(j-x) < r*r THEN
        INCL(s.shape[i], 31 - j);
      END;
    END;
  END;
END Circle;

PROCEDURE (s : Sprite) Line (x0, y0, x1, y1 : INTEGER);

VAR dx, dy, ix, iy, ax, ay, ct, of : INTEGER;
BEGIN
  dx := x1 - x0; dy := y1 - y0;
  ax := 0; ay := 0; ix := 1; iy := 1;
  IF dx < 0 THEN dx := -dx; ix := -1 END;
  IF dy < 0 THEN dy := -dy; iy := -1 END;
  IF dx < dy THEN
    ct := dx; dx := dy; dy := ct; ay := ix; ax := iy; ix := 0; iy := 0;
  END;
  of := dx DIV 2; ct := 0;

  WHILE (dx >= ct) DO
    INCL(s.shape[y0], 31 - x0 MOD 32);
    INC(x0, ix); INC(y0, ax); INC (of, dy);
    IF of >= dx THEN
      DEC (of, dx); INC(x0, ay); INC(y0, iy);
    END;
    INC(ct);
  END;
END Line;

(* Prozeduren auf einer Bitmap ******************************************)

PROCEDURE (b : Bitmap) Init (File : ARRAY OF CHAR);
VAR size, dest, dest2, source : LONGINT;
    i : INTEGER;
BEGIN
  Image.Load(File, b.mfdb);

  b.width := b.mfdb.Width; b.height := b.mfdb.Height;
  b.widthSet := (b.width + 31) DIV 32;
  b.size := LONG(b.widthSet) * LONG(b.height);

  SYSTEM.NEW (b.mem, b.size * 4);
  SYSTEM.NEW (b.col, b.size * 4);

  size := LONG(b.width + 15) DIV 16 * 2;
  FOR i := 0 TO b.height-1 DO
    source := LONG(i) * LONG(b.mfdb.WidthW * 2) + b.mfdb.Addr;
    dest := LONG(i) * LONG(b.widthSet * 4) + SYSTEM.VAL(LONGINT, b.mem);
    dest2 := LONG(i) * LONG(b.widthSet * 4) + SYSTEM.VAL(LONGINT, b.col);
    Memory.Copy(source, dest, size);
    Memory.Copy(source, dest2, size);
  END;

  b.mfdb.Addr := SYSTEM.VAL(LONGINT,b.mem);
  b.mfdb.Width := b.width;
  b.mfdb.Height := b.height;
  b.mfdb.WidthW := b.widthSet * 2;
  b.mfdb.Format := 0;
  b.mfdb.Planes := 1;

  b.update[0] := 0;
  b.update[1] := 0;
  b.update[2] := b.width - 1;
  b.update[3] := b.height - 1;


  b.visible[0] := 0; (* sichtbarer Ausschnitt *)
  b.visible[1] := 0;
  b.visible[2] := win.w - 1;

  IF b.width < win.w THEN b.visible[2] := b.width - 1 END;
  b.visible[3] := win.h - 1;
  IF b.height < win.h THEN b.visible[3] := b.height - 1 END;



  b.RandX := win.w DIV 4;
  b.RandY := win.h DIV 4;

END Init;

PROCEDURE (b : Bitmap) Clear ();
VAR i : LONGINT;
BEGIN
  FOR i := 0 TO b.size - 1 DO
    b.mem[i] := {};
    b.col[i] := {};
  END;
  b.update[0] := 0;
  b.update[1] := 0;
  b.update[2] := b.width - 1;
  b.update[3] := b.height - 1;
END Clear;

PROCEDURE (b : Bitmap) Point (x, y : INTEGER) : INTEGER;
BEGIN
  IF (x < 0) OR (y < 0) OR
     (x >= b.width) OR (y >= b.height) THEN RETURN HitBackground END;

  IF x < b.update[0] THEN b.update[0] := x END;
  IF y < b.update[1] THEN b.update[1] := y END;
  IF x > b.update[2] THEN b.update[2] := x END;
  IF y > b.update[3] THEN b.update[3] := y END;

  y := x DIV 32 + y * b.widthSet;
  x := 31 - x MOD 32;
  
  IF x IN b.mem[y] THEN
    EXCL(b.mem[y], x);
    IF x IN b.col[y] THEN
      EXCL(b.col[y], x);
      RETURN HitBackground;
    END;
    RETURN HitShip;
  ELSE
    INCL(b.mem[y], x);
  END;
  RETURN NoHit;
END Point;

PROCEDURE (b : Bitmap) GetPoint (x, y : INTEGER) : BOOLEAN;
BEGIN
  IF (x < 0) OR (y < 0) OR
     (x >= b.width) OR (y >= b.height) THEN RETURN FALSE END;
  y := x DIV 32 + y * b.widthSet;
  x := 31 - x MOD 32;
  RETURN x IN b.col[y];
END GetPoint;


PROCEDURE (b : Bitmap) Sprite (x, y : INTEGER; s : Sprite) : INTEGER;
VAR i, w, h : INTEGER;
    m : SET;
    collision : INTEGER;
BEGIN
  IF (x < 0) OR (y < 0) OR
     (x > b.width - s.w) OR (y > b.height - s.h) THEN
    RETURN HitExit;
  END;

  IF x < b.update[0] THEN b.update[0] := x END;
  IF y < b.update[1] THEN b.update[1] := y END;
  IF x + s.w - 1 > b.update[2] THEN b.update[2] := x + s.w - 1 END;
  IF y + s.h - 1 > b.update[3] THEN b.update[3] := y + s.h - 1 END;

  collision := NoHit;
  h := x MOD 32; w := x DIV 32 + b.widthSet * y;
  FOR i := 0 TO s.h - 1 DO
 
    m := SYSTEM.VAL(SET, SYSTEM.LSH(SYSTEM.VAL(LONGINT, s.shape[i]), -h));
    IF b.mem[w] * m # {} THEN
      IF b.col[w] * m # {} THEN
        IF collision = NoHit THEN
          collision := HitBackground
        END;
      ELSE
        collision := HitShip;
      END;
    END;
    b.mem[w] := b.mem[w] / m;


    IF s.w > (32 - h) THEN
      INC(w);


      m := SYSTEM.VAL(SET, SYSTEM.LSH(SYSTEM.VAL(LONGINT, s.shape[i]), 32-h));
      IF b.mem[w] * m # {} THEN
        IF b.col[w] * m # {} THEN
          IF collision = NoHit THEN
            collision := HitBackground
          END;
        ELSE
          collision := HitShip;
        END;
      END;
      b.mem[w] := b.mem[w] / m;
      INC(w, b.widthSet - 1);
    ELSE
      INC(w, b.widthSet);
    END;

  END;
  RETURN collision;
END Sprite;


PROCEDURE (b : Bitmap) Line (x0, y0, x1, y1 : INTEGER) : INTEGER;
VAR dx, dy, ix, iy, ax, ay, ct, of, xbit, xset : INTEGER;
    collision : INTEGER;
BEGIN
  IF (x0 < 0) OR (y0 < 0) OR (x1 < 0) OR (y1 < 0) THEN
    RETURN HitBackground
  END;
  IF (x0 >= b.width) OR (x1 >= b.width) OR
     (y0 >= b.height) OR (y1 >= b.height) THEN
    RETURN HitBackground;
  END;

  (* Update Rechteck aktualisieren *)
  IF x0 > x1 THEN ix := x0; ax := x1 ELSE ix := x1; ax := x0 END;
  IF y0 > y1 THEN iy := y0; ay := y1 ELSE iy := y1; ay := y0 END;
  IF ax < b.update[0] THEN b.update[0] := ax END;
  IF ay < b.update[1] THEN b.update[1] := ay END;
  IF ix > b.update[2] THEN b.update[2] := ix END;
  IF iy > b.update[3] THEN b.update[3] := iy END;

  (* Linie zeichnen *)
  collision := NoHit;
  dx := x1 - x0; dy := y1 - y0;
  ax := 0; ay := 0; ix := 1; iy := 1;
  IF dx < 0 THEN dx := -dx; ix := -1 END;
  IF dy < 0 THEN dy := -dy; iy := -1 END;
  IF dx < dy THEN
    ct := dx; dx := dy; dy := ct; ay := ix; ax := iy; ix := 0; iy := 0;
  END;
  of := dx DIV 2; ct := 0;

  WHILE (dx >= ct) DO
    xbit := 31 - x0 MOD 32;
    xset := b.widthSet * y0 + x0 DIV 32;
    IF xbit IN b.mem[xset] THEN
      EXCL(b.mem[xset], xbit);
    ELSE
      INCL(b.mem[xset], xbit);
      IF xbit IN b.col[xset] THEN
        collision := HitBackground;
      END;
    END;
    INC(x0, ix); INC(y0, ax); INC (of, dy);
    IF of >= dx THEN
      DEC (of, dx); INC(x0, ay); INC(y0, iy);
    END;
    INC(ct);
  END;
  RETURN collision;
END Line;

PROCEDURE (b : Bitmap) BackLine (x0, y0, x1, y1 : INTEGER);
(* zeichnet eine Linie auf die Bitmap und den Hintergrund *)
VAR dx, dy, ix, iy, ax, ay, ct, of, xbit, xset : INTEGER;
BEGIN
  IF (x0 < 0) OR (y0 < 0) OR (x1 < 0) OR (y1 < 0) THEN RETURN END;
  IF (x0 >= b.width) OR (x1 >= b.width) OR
     (y0 >= b.height) OR (y1 >= b.height) THEN RETURN END;

  (* Update Rechteck aktualisieren *)
  IF x0 > x1 THEN ix := x0; ax := x1 ELSE ix := x1; ax := x0 END;
  IF y0 > y1 THEN iy := y0; ay := y1 ELSE iy := y1; ay := y0 END;
  IF ax < b.update[0] THEN b.update[0] := ax END;
  IF ay < b.update[1] THEN b.update[1] := ay END;
  IF ix > b.update[2] THEN b.update[2] := ix END;
  IF iy > b.update[3] THEN b.update[3] := iy END;

  (* Linie zeichnen *)
  dx := x1 - x0; dy := y1 - y0;
  ax := 0; ay := 0; ix := 1; iy := 1;
  IF dx < 0 THEN dx := -dx; ix := -1 END;
  IF dy < 0 THEN dy := -dy; iy := -1 END;
  IF dx < dy THEN
    ct := dx; dx := dy; dy := ct; ay := ix; ax := iy; ix := 0; iy := 0;
  END;
  of := dx DIV 2; ct := 0;

  WHILE (dx >= ct) DO
    xbit := 31 - x0 MOD 32;
    xset := b.widthSet * y0 + x0 DIV 32;
    INCL(b.mem[xset], xbit);
    INCL(b.col[xset], xbit);
    INC(x0, ix); INC(y0, ax); INC (of, dy);
    IF of >= dx THEN
      DEC (of, dx); INC(x0, ay); INC(y0, iy);
    END;
    INC(ct);
  END;
END BackLine;

PROCEDURE (b : Bitmap) Fill (); 
VAR Erster, Farbe, Punkt : BOOLEAN; x, y, dummy : INTEGER;
BEGIN
  RETURN;

  Erster := TRUE;
  FOR x := 0 TO b.width-1 DO
    Farbe := TRUE;
    FOR y := 0 TO b.height-1 DO
      Punkt := b.GetPoint(x, y);
      IF Punkt THEN
        IF Erster THEN
          Erster := FALSE;
          Farbe := ~Farbe;
        END;
      ELSE
        Erster := TRUE;
        IF Farbe THEN
          dummy := b.Point(x, y);
        END;
      END;
    END;
  END;
END Fill;


PROCEDURE (b : Bitmap) Rectangle (x, y, w, h : INTEGER);
VAR i, j, xb, yb : INTEGER;
BEGIN
  FOR i := x TO x + w - 1 DO
    FOR j := y TO y + h - 1 DO
      yb := x DIV 32 + y * b.widthSet;
      xb := 31 - x MOD 32;
      INCL(b.mem[yb], xb);
      INCL(b.col[yb], xb);
    END;
  END;
END Rectangle;


PROCEDURE Overlap(VAR r, p : VR.pxyarray) : BOOLEAN;
BEGIN
  IF r[0] < p[0] THEN r[0] := p[0] END;
  IF r[1] < p[1] THEN r[1] := p[1] END;
  IF r[2] > p[2] THEN r[2] := p[2] END;
  IF r[3] > p[3] THEN r[3] := p[3] END;
  RETURN (r[2] - r[0] >= 0) & (r[3] - r[1] >= 0);
END Overlap;

PROCEDURE (b : Bitmap) Update(x, y : INTEGER);
VAR w, h : INTEGER; visold : VR.pxyarray;
BEGIN
  IF b.update[2] < 0 THEN RETURN END; (* Nichts zum Updaten *)

(*
  b.update[0] := b.update[0] DIV 16 * 16;
  b.update[2] := b.update[2] DIV 16 * 16 + 15;
*)

  (* Bildausschnitt verschieben ? *)
  IF (x >= 0) & (y >= 0) THEN
    w := b.visible[2] - b.visible[0]; h := b.visible[3] - b.visible[1];
    visold := b.visible;
    IF x < b.visible[0] + b.RandX THEN
      b.visible[0] := x - b.RandX * 5 DIV 2;
      IF b.visible[0] < 0 THEN b.visible[0] := 0 END;
      IF b.visible[0] # visold[0] THEN
        b.visible[2] := b.visible[0] + w;
        b.update := b.visible
      END;
    ELSIF x > b.visible[2] - b.RandX THEN
      b.visible[2] := x + b.RandX * 5 DIV 2;
      IF b.visible[2] >= b.width THEN b.visible[2] := b.width - 1 END;
      IF b.visible[2] # visold[2] THEN
        b.visible[0] := b.visible[2] - w;
        b.update := b.visible;
      END;
    END;
    IF y < b.visible[1] + b.RandY THEN
      b.visible[1] := y - b.RandY * 5 DIV 2;
      IF b.visible[1] < 0 THEN b.visible[1] := 0 END;
      IF b.visible[1] # visold[1] THEN
        b.visible[3] := b.visible[1] + h;
        b.update := b.visible;
      END;
    ELSIF y > b.visible[3] - b.RandY THEN
      b.visible[3] := y + b.RandY * 5 DIV 2;
      IF b.visible[3] >= b.height THEN b.visible[3] := b.height - 1 END;
      IF b.visible[3] # visold[3] THEN
        b.visible[1] := b.visible[3] - h;
        b.update := b.visible;
      END;
    END;
  END;

  IF Overlap(b.update, b.visible) THEN

    b.update[4] := b.update[0] - b.visible[0] + win.x;
    b.update[5] := b.update[1] - b.visible[1] + win.y;
    b.update[6] := b.update[2] - b.visible[0] + win.x;
    b.update[7] := b.update[3] - b.visible[1] + win.y;

    IF Color THEN
      VR.VrtCpyfm(Station, 1, b.update, b.mfdb, Screen, 0, 1);
    ELSE
      VR.VroCpyfm(Station, 3, b.update, b.mfdb, Screen);
    END;

  END;

  b.update[0] := b.width;
  b.update[1] := b.height;
  b.update[2] := -1;
  b.update[3] := -1;

END Update;

(* Point ****************************************************************)

PROCEDURE (p : Point) Init (x, y, vx, vy : LONGINT);
BEGIN
  p.x := x; p.y := y; p.vx := vx; p.vy := vy;
  p.sx := -1; p.sy := -1; p.hit := NoHit;
END Init;

PROCEDURE (p : Point) Move (dummy1, dummy2 : LONGINT);
BEGIN
  INC(p.x, p.vx * dt DIV fixcomma);
  INC(p.y, p.vy * dt DIV fixcomma);
END Move;

PROCEDURE (p : Point) Draw (Neu : BOOLEAN);
BEGIN
  p.hit := VScreen.Point(p.sx, p.sy);
  IF Neu THEN
    p.sx := SHORT(p.x DIV fixcomma);
    p.sy := SHORT(p.y DIV fixcomma);
    p.hit := VScreen.Point(p.sx, p.sy);
  END;
END Draw;

PROCEDURE (p : Point) Do() : BOOLEAN;
VAR exp : Explosion;

BEGIN
  IF p.hit = HitShip THEN
    p.Draw(FALSE); (* lîschen *);
    DEC(HitCount);
    RETURN TRUE;
  ELSE
    p.Move(0, 0); p.Draw(TRUE); (* setzt p.hit *)
    IF p.hit = HitShip THEN
      INC(HitCount);
    END;
  END;
  RETURN p.hit = HitBackground;
END Do;


(* Ball *****************************************************************)

PROCEDURE (b : Ball) Init(x, y, vx, vy : LONGINT);
BEGIN
  b.Init^(x, y, vx, vy);

  b.loading := FALSE; b.bx := -1; b.by := -1;
  NEW(b.shape); b.shape.Init(19, 19);
  b.shape.Circle (9, 9, 8);

  b.Draw (TRUE);
END Init;

PROCEDURE (b : Ball) Draw(Neu : BOOLEAN);
BEGIN
  b.hit := VScreen.Sprite(b.sx - 9, b.sy - 9, b.shape);
  IF Neu THEN
    b.sx := SHORT(b.x DIV fixcomma);
    b.sy := SHORT(b.y DIV fixcomma);
    b.hit := VScreen.Sprite(b.sx - 9, b.sy - 9, b.shape);
  END;
END Draw;

PROCEDURE (b : Ball) Do () : BOOLEAN;
VAR dx, dy, v, beta, alpha, vs : LONGINT; vx2, vy2 : REAL; i : INTEGER;
BEGIN
  dx := LONG(myShip.sx + 10 - b.sx); dy := LONG(myShip.sy + 10 - b.sy);
  IF b.loading THEN
    i := VScreen.Line (b.sx, b.sy, b.bx, b.by);
  END;
  IF dx * dx + dy * dy < abstand2 THEN
    b.bx := myShip.sx + 10; b.by := myShip.sy + 10;
    i := VScreen.Line (b.sx, b.sy, b.bx, b.by);
    b.loading := TRUE;
  ELSIF b.loading THEN (* Abflug ... *)
    (* Kugel und Schiff lîschen *)
    b.Draw(FALSE); (* lîschen *)
    myShip.Draw(FALSE);

    NEW(myLoadedShip);

    vx2 := myShip.vx * myShip.vx / fixcomma / fixcomma;
    vy2 := myShip.vy * myShip.vy / fixcomma / fixcomma;
    v := ENTIER(Math.sqrt(vx2 + vy2) * fixcomma);

    beta  := ENTIER(Math.arctan2 (myShip.vy, myShip.vx) * radtodeg);
    alpha := ENTIER(Math.arctan2 (dy, dx) * radtodeg);
    vs := v * sin[(beta - alpha + nangle) MOD nangle] DIV fixcomma;

    (* Impulserhaltung *)
    myLoadedShip.Init(b.x + dx DIV gesamtmasse * fixcomma,
                      b.y + dy DIV gesamtmasse * fixcomma,
                      myShip.vx DIV gesamtmasse,
                      myShip.vy DIV gesamtmasse);
    myLoadedShip.beta := myShip.beta;

    myLoadedShip.alpha := alpha * fixcomma;

    (* Drallerhaltung *)
    myLoadedShip.valpha := LONG(radtodeg) * LONG(traegschiff) DIV
                           LONG(traeggesamt) * vs DIV LONG(distschiff);

    b.loading := FALSE;
    objects.Remove(myShip);
    objects.Insert(myLoadedShip);

    myShip := myLoadedShip;
    RETURN TRUE;
  END;
  RETURN FALSE;
END Do;



(* Defender *************************************************************)

PROCEDURE (d : Defender) Init(x, y, vx, vy : LONGINT);
VAR b : BOOLEAN;
BEGIN
  d.Init^(x, y, vx, vy);
  
  d.LastShoot := 0; d.ShootFreq := 100000;
  d.Range := 200 * 2 * fixcomma;

  NEW(d.shape); d.shape.Init(32, 32);
  d.shape.Circle(15, 15, 10);
  d.shape.Line(1, 15, 29, 15);
  d.shape.Line(15, 1, 15, 29);
  d.Draw(TRUE);
END Init;

PROCEDURE (d : Defender) Draw (Neu : BOOLEAN);
BEGIN
  d.hit := VScreen.Sprite(d.sx-15, d.sy-15, d.shape);
  IF Neu THEN
    d.sx := SHORT(d.x DIV fixcomma);
    d.sy := SHORT(d.y DIV fixcomma);
    d.hit := VScreen.Sprite(d.sx-15, d.sy-15, d.shape);


  END;
END Draw;

PROCEDURE (d : Defender) Do () : BOOLEAN;

VAR dx, dy, vx, vy, beta : LONGINT;
    bullet : Point;
BEGIN
  IF HitCount > 0 THEN
    d.Draw(TRUE);
    IF d.hit = HitShip THEN
      d.Draw(FALSE); (* lîschen *)
      RETURN TRUE;
    END;
  END;
  
  IF (d.LastShoot <= 0) THEN
    dx := myShip.x - d.x;
    dy := myShip.y - d.y;
    beta  := ENTIER(Math.arctan2 (dy, dx) * radtodeg);

    IF ABS(dx) + ABS(dy) < d.Range THEN
      vx := 15 * cos[(beta + nangle) MOD nangle];
      vy := 15 * sin[(beta + nangle) MOD nangle];

      NEW(bullet);
      bullet.Init(d.x + vx, d.y + vy, vx DIV 16 + d.vx, vy DIV 16 + d.vy);
      objects.Insert(bullet);
    END;
    
    d.LastShoot := d.ShootFreq + 4 * (XBIOS.Random() MOD d.ShootFreq);
  END;
  IF d.LastShoot > 0 THEN
    DEC(d.LastShoot, dt);
  END;
  RETURN FALSE;
END Do;


(* Explosion ************************************************************)

PROCEDURE (e : Explosion) Init (x, y, vx, vy : LONGINT);
BEGIN
  e.Init^(x, y, vx, vy);
  NEW(e.shape); e.shape.Init(32, 32);
END Init;

PROCEDURE (e : Explosion) Do() : BOOLEAN;
VAR i, j, x, y : INTEGER; newshape : ARRAY 32 OF SET;
BEGIN
  IF e.LastShape < 0 THEN
    e.hit := VScreen.Sprite(e.sx, e.sy, e.shape);
    e.sx := SHORT(e.x DIV fixcomma)-15; e.sy := SHORT(e.y DIV fixcomma)-15;

    FOR j := 0 TO 31 DO
      newshape[j] := {};
    END;
    FOR i := 0 TO 31 DO
      FOR j := 0 TO 31 DO
        IF i IN e.shape.shape[j] THEN
(*
          x := i + Rnd(3) - 1;
          y := j + Rnd(3) - 1;
*)
          IF (x >= 0) AND (y >= 0) AND (x < 32) AND (y < 32) THEN
            INCL(newshape[y], x);
          END;
        END;
      END;
    END;
    FOR j := 0 TO 31 DO
      e.shape.shape[j] := newshape[j];
    END;
    e.hit := VScreen.Sprite(e.sx, e.sy, e.shape);
    e.LastShape := 10000;
  ELSE
    DEC(e.LastShape, dt);
  END;
  RETURN FALSE;
END Do;



(* Schiff ***************************************************************)

PROCEDURE (s : Ship) Init(x, y, vx, vy : LONGINT);
VAR ax, ay, bx, by, cx, cy, i : INTEGER;
    co, si : LONGINT;
BEGIN
  s.Init^(x, y, vx, vy);
  
  s.beta := 271 * fixcomma;
  s.sb := SHORT(s.beta DIV fixcomma DIV 4);
  
  FOR i := 0 TO 89 DO
    NEW(s.shape[i]); s.shape[i].Init(21, 20);
    co := cos[4 * i]; si := sin[4 * i];
    ax := SHORT((-7 * co - 7 * si) DIV fixcomma) + 10;
    ay := SHORT((-7 * si + 7 * co) DIV fixcomma) + 10;
    bx := SHORT((10 * co) DIV fixcomma) + 10;
    by := SHORT((10 * si) DIV fixcomma) + 10;
    cx := SHORT((-7 * co + 7 * si) DIV fixcomma) + 10;
    cy := SHORT((-7 * si - 7 * co) DIV fixcomma) + 10;
    s.shape[i].Line (ax, ay, bx, by);
    s.shape[i].Line (bx, by, cx, cy);
    s.shape[i].Line (cx, cy, ax, ay);
    NEW(s.shapet[i]); s.shapet[i].Init(21, 21);
    s.shapet[i].shape := s.shape[i].shape;
    ax := SHORT((-7 * co - 4 * si) DIV fixcomma) + 10;
    ay := SHORT((-7 * si + 4 * co) DIV fixcomma) + 10;
    bx := SHORT((-10 * co) DIV fixcomma) + 10;
    by := SHORT((-10 * si) DIV fixcomma) + 10;
    cx := SHORT((-7 * co + 4 * si) DIV fixcomma) + 10;
    cy := SHORT((-7 * si - 4 * co) DIV fixcomma) + 10;
    s.shapet[i].Line (ax, ay, bx, by);
    s.shapet[i].Line (bx, by, cx, cy);

  END;
  s.thrust := FALSE; s.last := FALSE;
  s.LastShoot := 0; s.ShootFreq := 100000;
END Init;

PROCEDURE(s : Ship) Draw (Neu : BOOLEAN);
BEGIN
  IF s.last THEN
    s.hit := VScreen.Sprite(s.sx, s.sy, s.shapet[s.sb]);
  ELSE
    s.hit := VScreen.Sprite(s.sx, s.sy, s.shape[s.sb]);
  END;
  IF Neu THEN
    s.last := s.thrust;
    s.sx := SHORT(s.x DIV fixcomma) - 10;
    s.sy := SHORT(s.y DIV fixcomma) - 10;
    s.sb := SHORT(s.beta DIV fixcomma DIV 4);
    IF s.last THEN
      s.hit := VScreen.Sprite(s.sx, s.sy, s.shapet[s.sb]);
    ELSE
      s.hit := VScreen.Sprite(s.sx, s.sy, s.shape[s.sb]);
    END;
  END;
END Draw;

PROCEDURE(s : Ship) Move (acc, angle : LONGINT);
BEGIN
  INC(s.vx, cos[angle DIV fixcomma] * acc DIV fixcomma);
  INC(s.vy, sin[angle DIV fixcomma] * acc DIV fixcomma + GravStep);
  INC(s.x, s.vx * dt DIV fixcomma);
  INC(s.y, s.vy * dt DIV fixcomma);
END Move;

PROCEDURE(s : Ship) Do () : BOOLEAN;
VAR mb, Shifts : SET;
    mx, my, i : INTEGER;
    acc, bx, by : LONGINT;
    Shoot : Point;
    Shift : SET; Scan : INTEGER; Ascii : CHAR;
BEGIN
  Evnt.MKstate(mx, my, mb, Shifts);
  IF Evnt.LSHIFT IN Shifts THEN
    s.beta := (s.beta + nangle2 - BetaStep) MOD nangle2;
  END;
  IF Evnt.ALTERNATE IN Shifts THEN
    s.beta := (s.beta + BetaStep) MOD nangle2;
  END;
  IF Evnt.RSHIFT IN Shifts THEN
    acc := AccStep; s.thrust := TRUE;
  ELSE
    acc := 0; s.thrust := FALSE;
  END;

  IF (Evnt.CONTROL IN Shifts) AND (s.LastShoot <= 0) THEN
    bx := 11 * cos[s.beta DIV fixcomma];
    by := 11 * sin[s.beta DIV fixcomma];
    NEW(Shoot);
    IF s IS LoadedShip THEN
      Shoot.Init(LONG(s(LoadedShip).mx) * fixcomma + bx,
                 LONG(s(LoadedShip).my) * fixcomma + by,
                 bx DIV 5 + s.vx, by DIV 5 + s.vy);
    ELSE
      Shoot.Init(s.x + bx, s.y + by, bx DIV 5 + s.vx, by DIV 5 + s.vy);
    END;
    objects.Add(Shoot); INC(nobj);
    s.LastShoot := s.ShootFreq;
  END;

  IF s.LastShoot > 0 THEN
    DEC(s.LastShoot, dt)
  END;

  s.Move(acc, s.beta); s.Draw(TRUE);

  IF s.hit = HitBackground THEN (* rummms... *)
    s.vx := 0; s.vy := 0;
    IF s IS LoadedShip THEN
      s(LoadedShip).valpha := 0;
    END;
  ELSIF s.hit = HitExit THEN
    ende := HitExit;
  ELSIF (HitCount > 0) AND (s.hit = HitShip) THEN (* bummms... *)
    s.Draw(FALSE); (* lîschen *)
    ende := HitShip;
    RETURN TRUE;
  END;
  RETURN FALSE;
END Do;

(* Schiff mit Kugel *****************************************************)

PROCEDURE(l : LoadedShip) Init (x, y, vx, vy : LONGINT);
BEGIN
  l.Init^(x, y, vx, vy);
  l.alpha := 0; l.valpha := 0;
  l.mx := -1; l.my := -1; l.lx := -1; l.ly := -1;

  l.valphaconst := LONG(radtodeg) * LONG(distschiff) * LONG(fixcomma) DIV
                   LONG(traeggesamt);

  NEW(l.ball); l.ball.Init(19, 19);
  l.ball.Circle (9, 9, 8);
END Init;

PROCEDURE(l : LoadedShip) Move (acc, angle : LONGINT);
VAR da : LONGINT; (* Winkeldifferenz *)
BEGIN

  da := (angle - l.alpha + nangle2) MOD nangle2 DIV fixcomma;

  (* Verschiebung *)
  l.Move^(acc DIV (ballmasse + 1), angle);

  (* Drehbewegung *)
  INC(l.valpha, acc * sin[da] DIV fixcomma * l.valphaconst DIV fixcomma);
  l.alpha := (l.alpha + l.valpha * dt DIV fixcomma + nangle2) MOD nangle2;

END Move;

PROCEDURE(l : LoadedShip) Draw (Neu : BOOLEAN);
VAR a, hit : INTEGER;
BEGIN
  l.hit := VScreen.Line(l.mx, l.my, l.lx, l.ly);

  l.hit := VScreen.Sprite(l.lx - 9, l.ly - 9, l.ball);

  IF l.last THEN
    l.hit := VScreen.Sprite(l.sx, l.sy, l.shapet[l.sb]);
  ELSE
    l.hit := VScreen.Sprite(l.sx, l.sy, l.shape[l.sb]);
  END;

  IF Neu THEN
    a := SHORT(l.alpha DIV fixcomma);
    l.mx := SHORT((l.x + distschiff * cos[a]) DIV fixcomma);
    l.my := SHORT((l.y + distschiff * sin[a]) DIV fixcomma);
    l.lx := SHORT((l.x - distball * cos[a]) DIV fixcomma);
    l.ly := SHORT((l.y - distball * sin[a]) DIV fixcomma);
  
    l.last := l.thrust;
    l.sx := l.mx - 10;
    l.sy := l.my - 10;
    l.sb := SHORT(l.beta DIV fixcomma DIV 4);

    IF l.last THEN
      l.hit := VScreen.Sprite(l.sx, l.sy, l.shapet[l.sb]);
    ELSE
      l.hit := VScreen.Sprite(l.sx, l.sy, l.shape[l.sb]);
    END;

    hit := VScreen.Sprite(l.lx - 9, l.ly - 9, l.ball);
    IF hit > l.hit THEN l.hit := hit END;
  
    hit := VScreen.Line(l.mx, l.my, l.lx, l.ly);
  END;
END Draw;

(* Los geht's ***********************************************************)

PROCEDURE Run;
VAR Workin  : VC.workin;
BEGIN
  InitTables();

  Graf.ChangeMouse( Graf.ARROW);
  Station := 1; (* VDI Virtual Workstation îffnen *)
  Workin.Id := 1; Workin.LineType := 1;
  Workin.LineColor := 1; Workin.MarkType := 1;
  Workin.MarkColor := 1; Workin.Font := 1;
  Workin.TextColor := 1; Workin.FillStyle := 0;
  Workin.FillPat := 0; Workin.FillColor := 1;
  Workin.KoorType := 2;
  VC.VOpnvwk(Workin, Station, Workout);
  VA.VswrMode(Station,VA.EXOR);
  VA.VsfPerimeter(Station, FALSE);

  Color := ~(Workout.NumColor = 2);

  IF ~Rsrc.Load("THRUSTN.RSC") THEN
    Task.Exit(0);
  END;
  NEW(menu); menu.Init(Rsrc.GetAddr( MENU ));
  menu.Set(SPIEL, START, Spiel);
  menu.Set(SPIEL, ENDE, Ende);
  menu.Set(THRUST, INFO, Info);
  menu.Show;
  
  NEW(InfoDialog); InfoDialog.InitDialog (Rsrc.GetAddr(ABOUT), 0, TRUE);
  InfoDialog.SetTitle ("Thrust Info");

  NEW(win); win.Initialize;
  win.SetTitle(WindowTitle);

  Graf.ChangeMouse( Graf.BEE);
  NEW(myLevel); myLevel.Init();
  Graf.ChangeMouse( Graf.ARROW);

  win.SetDataWH (VScreen.width, VScreen.height);
  IF VScreen.width < win.w THEN
    win.w := VScreen.width;
    win.fw := win.w;
  END;
  IF VScreen.height < win.h THEN
    win.h := VScreen.height;
    win.fh := win.h;
  END;
  
  win.SetInfo(" L-SHIFT/ALTERNATE = Rotation, R-SHIFT = Thrust, CONTROL = Fire, ESC = Ende");
  win.Open;

  GemApp.Run;
END Run;


PROCEDURE (v : myViewer) Redraw (x, y, w, h : INTEGER);
VAR Off : LONGINT; update : BOOLEAN;
    icon : Objc.ptr; ix, iy, iw, ih : INTEGER;
BEGIN
  IF v.iconified THEN
    v.Redraw^(x, y, w, h); (* lîschen *);
    icon := Objc.GetPtr(Rsrc.GetAddr(BILD), 0);
    Wind.GetXYWH(v.handle, Wind.WORK, ix, iy, iw, ih);
    icon.X := ix + (iw - icon.W) DIV 2;
    icon.Y := iy + (ih - icon.H) DIV 2;
    Objc.Draw(icon, 0, 100, x, y, w, h);
  ELSE
    (* Offset Kontrolle *)
    update := FALSE;
    IF (v.xOff + v.w > v.dw) & (v.xOff > 0) THEN
      Off := SHORT(v.dw) - v.w;
      IF Off < 0 THEN Off := 0 END;
      v.SetOffset (Off, v.yOff);
      update := TRUE;
    END;
    IF (v.yOff + v.h > v.dh) & (v.yOff > 0) THEN
      Off := SHORT(v.dh) - v.h;
      IF Off < 0 THEN Off := 0 END;
      v.SetOffset (v.xOff, Off);
      update := TRUE;
    END;
  
    IF update THEN
      v.RedrawAll;
      RETURN;
    END;
  
    VScreen.update[0] := SHORT(v.xOff) + x - v.x; (* Updatebereich *)
    VScreen.update[1] := SHORT(v.yOff) + y - v.y;
    VScreen.update[2] := VScreen.update[0] + w - 1;
    VScreen.update[3] := VScreen.update[1] + h - 1;
  
    VScreen.visible[0] := SHORT(v.xOff); (* gesamtes Fenster *)
    VScreen.visible[1] := SHORT(v.yOff);
    VScreen.visible[2] := SHORT(v.xOff) + v.w - 1;
    IF VScreen.visible[2] >= VScreen.width THEN
      VScreen.visible[2] := VScreen.width - 1
    END;
    VScreen.visible[3] := SHORT(v.yOff) + v.h - 1;
    IF VScreen.visible[3] >= VScreen.height THEN
      VScreen.visible[3] := VScreen.height - 1
    END;
  
    VScreen.Update(-1, -1);
  END;
END Redraw;


PROCEDURE Spiel();
VAR t0, t, c, t2 : LONGINT;
    obj : LinkedList.Element;

    handle : INTEGER;
    Shift : SET; Scan : INTEGER; Ascii : CHAR;
    aus, hilf : ARRAY 128 OF CHAR;

  PROCEDURE DoObject (obj : LinkedList.Element);
  BEGIN
    IF obj(Point).Do () THEN
      objects.Remove(obj);
      DEC(nobj);
    END;
    VScreen.Update(myShip.sx, myShip.sy);
  END DoObject;

BEGIN
  debug := TRUE;
  Gravitation := FALSE;

  IF win.iconified THEN
    win.UnIconified (0,0,0,0);
  END;
  win.Open;

    Graf.ChangeMouse( Graf.BEE);
    NEW(myLevel); myLevel.Init();
    Graf.ChangeMouse( Graf.ARROW);
    Graf.HideMouse();

    Wind.Update(Wind.BEGUPD);

    HitCount := 0;
    t := 0; c := 0;
    t := Timer.Get(); t0 := t;
    AccStep := 0; BetaStep := 0;
    ende := NoHit;

    REPEAT

      INC(c);
(*
      t2 := Timer.Get();
      dt := (t2 - t) * fixcomma DIV 2; t := t2;
*)
      t2 := Timer.Get();
      INC(dt, (t2 - t) * fixcomma DIV 32); t := t2;
      DEC(dt, dt DIV 16); (* Tiefpass *)

      AccStep := dt DIV 64;
      BetaStep := 2 * dt;
      IF Gravitation THEN
        GravStep := dt DIV 512;
      ELSE
        GravStep := 0;
      END;

      objects.Do (DoObject);

      IF IO.KeyPressed() THEN
        IO.ReadKey (Shift, Scan, Ascii);
        IF Ascii = CHR(27) THEN
          ende := HitEsc; (* ESC *)
        END;
        IF Ascii = "d" THEN debug := TRUE END;
      END;

    UNTIL ende # NoHit;

    t := Timer.Get() - t0;

    CASE ende OF
      HitEsc : aus := " ESCAPE! Zeit = ";
    | HitShip : aus := " HIT!  Zeit = ";
    | HitExit : aus := " EXIT! Zeit = ";
    END;
    NumStr.LCardTo (t DIV 200, 10, hilf); Strings.Append(hilf, aus);
    Strings.AppendC (".", aus);
    NumStr.LCardTo (t MOD 200 DIV 2, 10, hilf); Strings.Append(hilf, aus);
    Strings.Append (" s, ", aus);
    Strings.Append (" Zyklus = ", aus);
    NumStr.LCardTo (t DIV (c DIV 5), 10, hilf); Strings.Append(hilf, aus);
    Strings.Append (" ms", aus);

    Graf.ShowMouse();
    win.SetInfo(aus);

  Wind.Update(Wind.ENDUPD);
  win.SetOffset(VScreen.visible[0], VScreen.visible[1]);

END Spiel;

PROCEDURE Info;
BEGIN
  InfoDialog.Open;
END Info;

PROCEDURE Ende;
BEGIN
  win.Remove;
  GemApp.exit := TRUE;
END Ende;

BEGIN
  IF ~Sys.Loader THEN (* if running as stand alone program *)
    Run;
    Task.Exit(0); (* needed for clean up; will not return *)
  END;
END Thrust.
