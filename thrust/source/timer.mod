MODULE Timer;

IMPORT XBIOS;

VAR Time : POINTER TO RECORD Hz200 : LONGINT END;
    GlobTime : LONGINT;

(*$-K Kein Stackcheck *)
PROCEDURE Get2();
BEGIN
  GlobTime := Time.Hz200;
END Get2;
(*$=K *)

PROCEDURE Get* () : LONGINT;
BEGIN
  XBIOS.Supexec(Get2);
  RETURN GlobTime;
END Get;

BEGIN
  Time := 4BAH;
END Timer.

