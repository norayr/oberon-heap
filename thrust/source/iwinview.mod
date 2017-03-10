MODULE IWinView; (* AK 7.10.96 *)

(*
 Subclasses WinView for Iconify (MTOS, MagiC)
 *)

IMPORT
  Evnt, Wind, WinView,
  AES, Appl, SYSTEM;

TYPE IViewer* = POINTER TO IViewDesc;
     IViewDesc* = RECORD(WinView.ViewDesc);
                    iconified- : BOOLEAN;
                  END;

VAR Msg : ARRAY 8 OF INTEGER; (* fÅr Redrawmessage *)
    desk* : RECORD x, y, w, h : INTEGER; END;

PROCEDURE (v : IViewer) Initialize*;
BEGIN
  v.Initialize^;
  v.iconified := FALSE;
  v.elements := {Wind.TITLE, Wind.INFOLINE, Wind.CLOSER, Wind.FULLER, Wind.MOVER,
                 Wind.SIZER, Wind.UPARROW, Wind.DNARROW, Wind.VSLIDER,
                 Wind.LFARROW, Wind.RTARROW, Wind.HSLIDER, Wind.SMALLER};
  Wind.Calc( Wind.WORKSP, v.elements, desk.x, desk.y, desk.w, desk.h, v.fx, v.fy, v.fw, v.fh);
  v.x := v.fx; v.y := v.fy; v.w := v.fw; v.h := v.fh;
END Initialize;

PROCEDURE (v : IViewer) SetInfo* (info : ARRAY OF CHAR);
(* Neue Funktion *)
BEGIN
  COPY(info, v.info);
  IF v.opened THEN
    Wind.SetAddress( v.handle, Wind.INFO, SYSTEM.ADR( v.info) );
  END;
END SetInfo;

PROCEDURE (v : IViewer) Moved* (newX, newY : INTEGER);
VAR x, y, w, h : INTEGER;
BEGIN
  IF v.iconified THEN
    Wind.GetXYWH(v.handle, Wind.CURR, x, y, w, h);
    Wind.SetCurr(v.handle, newX, newY, w, h);
  ELSE
    v.Moved^(newX, newY);
  END;
END Moved;

PROCEDURE(v : IViewer) Open*;
(* Correction of creation with Full Size (AK) *)
BEGIN
  Wind.Calc(Wind.WHOLE, v.elements, v.fx, v.fy, v.fw, v.fh, v.fx, v.fy, v.fw, v.fh);
  v.Open^;
  Wind.Calc(Wind.WORK, v.elements, v.fx, v.fy, v.fw, v.fh, v.fx, v.fy, v.fw, v.fh);
END Open;


PROCEDURE (v : IViewer) RedrawAll*;
BEGIN
  v.UpdateRect(desk.x, desk.y, desk.w, desk.h);
END RedrawAll;

PROCEDURE (v : IViewer) Iconified* (x, y, w, h : INTEGER);
BEGIN
  Wind.Iconify(v.handle, x, y, w, h);
  v.iconified := TRUE;
END Iconified;

PROCEDURE (v : IViewer) UnIconified* (x, y, w, h : INTEGER);
BEGIN
  IF (x = 0) & (y = 0) & (w = 0) & (h = 0) THEN
    Wind.Calc(Wind.WHOLE, v.elements, v.x, v.y, v.w, v.h, x, y, w, h);
  END;
  Wind.UnIconify(v.handle, x, y, w, h);
  v.iconified := FALSE;
END UnIconified;

PROCEDURE  (v : IViewer) IconifiedAll* (x, y, w, h : INTEGER);
(* to be overloaded... *)
BEGIN
  v.Iconified (x, y, w, h);
END IconifiedAll;


PROCEDURE (v : IViewer) HandleMsgEvent* (VAR msgBuf : Evnt.msgbuf) : BOOLEAN;
BEGIN
  CASE msgBuf[0] OF
    Evnt.WMICONIFY :
      v.Iconified (msgBuf[4], msgBuf[5], msgBuf[6], msgBuf[7]);
  | Evnt.WMUNICONIFY :
      v.UnIconified(msgBuf[4], msgBuf[5], msgBuf[6], msgBuf[7]);
  | Evnt.WMALLICONIFY :
      v.IconifiedAll(msgBuf[4], msgBuf[5], msgBuf[6], msgBuf[7]);
  ELSE
    RETURN v.HandleMsgEvent^(msgBuf);
  END;
  RETURN TRUE;
END HandleMsgEvent;

PROCEDURE Init;
BEGIN
  Wind.GetXYWH (0, Wind.WORK, desk.x, desk.y, desk.w, desk.h);
END Init;

BEGIN
  Init;
END IWinView.