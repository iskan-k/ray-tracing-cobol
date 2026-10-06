      *================================================================
      * RAYTRACE.COB - A REAL-TIME RAY TRACER WRITTEN IN COBOL.
      *
      * FOUR SPHERES (CHROME, RUBY, SAPPHIRE, GOLD) ORBIT AND BOUNCE
      * OVER A FRESNEL-REFLECTIVE CHECKERBOARD. EVERY PIXEL IS A RAY:
      * RECURSIVE MIRROR REFLECTIONS (UP TO 5 BOUNCES), SOFT-EDGED
      * SHADOWS, PHONG HIGHLIGHTS, DISTANCE FOG, SKY WITH SUN GLOW,
      * GAMMA-CORRECTED 24-BIT COLOR.
      *
      * BUILD:  cobc -x -O2 -fno-binary-truncate -o raytrace
      *              raytrace.cob
      *
      * LIVE:   ./raytrace live [COLS] [ROWS] [SECONDS]
      *         RENDERS INTO THE TERMINAL USING TRUECOLOR HALF-BLOCK
      *         CHARACTERS (TWO PIXELS PER CELL). DEFAULT 100 X 30.
      *
      * VIDEO:  ./raytrace video W H FIRST LAST > OUT.RGB  (MAX 1080P)
      *         WRITES RAW RGB24 FRAMES (30 FPS TIMELINE) TO STDOUT
      *         FOR FFMPEG. SEE RENDER.SH.
      *
      * ALL PER-PIXEL MATH IS FIXED POINT (1.0 = 4096) IN BINARY
      * FIELDS. GNUCOBOL'S FLOATING POINT AND SQRT ARE TOO SLOW FOR
      * THE INNER LOOP, SO SQUARE ROOTS COME FROM A LOOKUP TABLE AND
      * CAMERA RAYS ARE NORMALIZED ONCE AT STARTUP.
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAYTRACE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78 FX                       VALUE 4096.
       78 N-SPH                    VALUE 4.
       78 MAX-DEPTH                VALUE 5.
       78 MAX-PIX                  VALUE 2073600.
       78 SQ-MAX                   VALUE 8191.
       78 T-BIG                    VALUE 999999999.
       78 T-FAR                    VALUE 327680.
       78 T-EPS                    VALUE 20.

      *---------------------------------------------------------------
      * COMMAND LINE / CONTROL
       01 W-ARGC                   PIC 9(4)  COMP-5.
       01 W-ARG                    PIC X(40).
       01 W-MODE                   PIC X(10) VALUE "live".
          88 MODE-VIDEO            VALUE "video".
       01 W-COLS                   PIC 9(5)  COMP-5 VALUE 100.
       01 W-ROWS                   PIC 9(5)  COMP-5 VALUE 30.
       01 W-SECS                   PIC 9(5)  COMP-5 VALUE 60.
       01 W-FIRST                  PIC 9(7)  COMP-5 VALUE 0.
       01 W-LAST                   PIC 9(7)  COMP-5 VALUE 0.
       01 W-FRAME                  PIC 9(7)  COMP-5 VALUE 0.
       01 IMG-W                    PIC 9(5)  COMP-5.
       01 IMG-H                    PIC 9(5)  COMP-5.
       01 SCR-X                    PIC 9(5)  COMP-5.
       01 SCR-Y                    PIC 9(5)  COMP-5.
       01 C-ROW                    PIC 9(5)  COMP-5.
       01 PIX                      PIC 9(7)  COMP-5.
       01 K                        PIC 9(7)  COMP-5.
       01 SK                       PIC 9(2)  COMP-5.

      * TIMING
       01 W-CLOCK                  PIC 9(8).
       01 W-CLOCK-R REDEFINES W-CLOCK.
          05 CLK-HH                PIC 99.
          05 CLK-MM                PIC 99.
          05 CLK-SS                PIC 99.
          05 CLK-CC                PIC 99.
       01 T-START                  COMP-2.
       01 T-NOW                    COMP-2.
       01 SCENE-T                  COMP-2.
       01 W-FPS                    COMP-2.
       01 FPS-OUT                  PIC ZZ9.9.
       01 FRAME-OUT                PIC Z(6)9.

      *---------------------------------------------------------------
      * LOOKUP TABLES
       01 SQ-TABLE.
          05 SQ                    PIC S9(9) COMP-5
                                   OCCURS 8192 TIMES.
       01 GAM-TABLE.
          05 GAM                   PIC S9(4) COMP-5
                                   OCCURS 4097 TIMES.
       01 BYTE-TAB                 PIC X(256).

      * CAMERA-SPACE UNIT RAY FOR EVERY PIXEL
       01 CAM-RAYS.
          05 CAM-RAY OCCURS 2073600 TIMES.
             10 CAM-U              PIC S9(9) COMP-5.
             10 CAM-V              PIC S9(9) COMP-5.
             10 CAM-W              PIC S9(9) COMP-5.

      *---------------------------------------------------------------
      * SCENE
       01 SPHERES.
          05 SPH OCCURS 4 TIMES.
             10 SPH-X              PIC S9(9) COMP-5.
             10 SPH-Y              PIC S9(9) COMP-5.
             10 SPH-Z              PIC S9(9) COMP-5.
             10 SPH-R              PIC S9(9) COMP-5.
             10 SPH-R2             PIC S9(9) COMP-5.
             10 SPH-AR             PIC S9(9) COMP-5.
             10 SPH-AG             PIC S9(9) COMP-5.
             10 SPH-AB             PIC S9(9) COMP-5.
             10 SPH-FR             PIC S9(9) COMP-5.
             10 SPH-FG             PIC S9(9) COMP-5.
             10 SPH-FB             PIC S9(9) COMP-5.
             10 SPH-SP             PIC S9(9) COMP-5.
      *      CAMERA-RELATIVE TERMS, PRECOMPUTED EACH FRAME
             10 SPH-OCX            PIC S9(9) COMP-5.
             10 SPH-OCY            PIC S9(9) COMP-5.
             10 SPH-OCZ            PIC S9(9) COMP-5.
             10 SPH-OCC            PIC S9(9) COMP-5.

       01 CAMERA.
          05 CAMX                  PIC S9(9) COMP-5.
          05 CAMY                  PIC S9(9) COMP-5.
          05 CAMZ                  PIC S9(9) COMP-5.
          05 RTX                   PIC S9(9) COMP-5.
          05 RTY                   PIC S9(9) COMP-5.
          05 RTZ                   PIC S9(9) COMP-5.
          05 UPX                   PIC S9(9) COMP-5.
          05 UPY                   PIC S9(9) COMP-5.
          05 UPZ                   PIC S9(9) COMP-5.
          05 FWX                   PIC S9(9) COMP-5.
          05 FWY                   PIC S9(9) COMP-5.
          05 FWZ                   PIC S9(9) COMP-5.

       01 SUN.
          05 LTX                   PIC S9(9) COMP-5.
          05 LTY                   PIC S9(9) COMP-5.
          05 LTZ                   PIC S9(9) COMP-5.

      * FLOATING-POINT SCRATCH FOR PER-FRAME SETUP ONLY
       01 FLT.
          05 F-A                   COMP-2.
          05 F-B                   COMP-2.
          05 F-C                   COMP-2.
          05 F-U                   COMP-2.
          05 F-V                   COMP-2.
          05 F-L                   COMP-2.
          05 F-TAN                 COMP-2 VALUE 0.52.
          05 F-PX                  COMP-2.
          05 F-PY                  COMP-2.
          05 F-PZ                  COMP-2.
          05 F-FX                  COMP-2.
          05 F-FY                  COMP-2.
          05 F-FZ                  COMP-2.
          05 F-RX                  COMP-2.
          05 F-RZ                  COMP-2.

      *---------------------------------------------------------------
      * PER-RAY STATE (ALL FIXED POINT)
       01 RAY.
          05 ORX                   PIC S9(9) COMP-5.
          05 ORY                   PIC S9(9) COMP-5.
          05 ORZ                   PIC S9(9) COMP-5.
          05 DRX                   PIC S9(9) COMP-5.
          05 DRY                   PIC S9(9) COMP-5.
          05 DRZ                   PIC S9(9) COMP-5.
          05 HIT-K                 PIC S9(4) COMP-5.
          05 TMIN                  PIC S9(9) COMP-5.
          05 TC                    PIC S9(9) COMP-5.
          05 OCX                   PIC S9(9) COMP-5.
          05 OCY                   PIC S9(9) COMP-5.
          05 OCZ                   PIC S9(9) COMP-5.
          05 BB                    PIC S9(9) COMP-5.
          05 CCV                   PIC S9(9) COMP-5.
          05 DISC                  PIC S9(9) COMP-5.
          05 QX                    PIC S9(9) COMP-5.
          05 QY                    PIC S9(9) COMP-5.
          05 QZ                    PIC S9(9) COMP-5.
          05 D2                    PIC S9(9) COMP-5.
          05 PX                    PIC S9(9) COMP-5.
          05 PY                    PIC S9(9) COMP-5.
          05 PZ                    PIC S9(9) COMP-5.
          05 NX                    PIC S9(9) COMP-5.
          05 NY                    PIC S9(9) COMP-5.
          05 NZ                    PIC S9(9) COMP-5.
          05 RFX                   PIC S9(9) COMP-5.
          05 RFY                   PIC S9(9) COMP-5.
          05 RFZ                   PIC S9(9) COMP-5.
          05 DN                    PIC S9(9) COMP-5.
          05 DIFF                  PIC S9(9) COMP-5.
          05 SHAD                  PIC S9(9) COMP-5.
          05 DS                    PIC S9(9) COMP-5.
          05 SPEC                  PIC S9(9) COMP-5.
          05 SPECK                 PIC S9(9) COMP-5.
          05 AMB                   PIC S9(9) COMP-5.
          05 FOG                   PIC S9(9) COMP-5.
          05 SKT                   PIC S9(9) COMP-5.
          05 GL                    PIC S9(9) COMP-5.
          05 IX                    PIC S9(9) COMP-5.
          05 IZ                    PIC S9(9) COMP-5.
          05 PAR-Q                 PIC S9(9) COMP-5.
          05 PAR-R                 PIC S9(9) COMP-5.
          05 ALB-R                 PIC S9(9) COMP-5.
          05 ALB-G                 PIC S9(9) COMP-5.
          05 ALB-B                 PIC S9(9) COMP-5.
          05 REF-R                 PIC S9(9) COMP-5.
          05 REF-G                 PIC S9(9) COMP-5.
          05 REF-B                 PIC S9(9) COMP-5.
          05 LOC-R                 PIC S9(9) COMP-5.
          05 LOC-G                 PIC S9(9) COMP-5.
          05 LOC-B                 PIC S9(9) COMP-5.
          05 ACC-R                 PIC S9(9) COMP-5.
          05 ACC-G                 PIC S9(9) COMP-5.
          05 ACC-B                 PIC S9(9) COMP-5.
          05 WGT-R                 PIC S9(9) COMP-5.
          05 WGT-G                 PIC S9(9) COMP-5.
          05 WGT-B                 PIC S9(9) COMP-5.
          05 W-DEPTH               PIC S9(4) COMP-5.
          05 W-PRIMARY             PIC 9.
          05 W-DONE                PIC 9.
          05 OUT-R                 PIC S9(4) COMP-5.
          05 OUT-G                 PIC S9(4) COMP-5.
          05 OUT-B                 PIC S9(4) COMP-5.

      *---------------------------------------------------------------
      * OUTPUT
       01 FRAME-BUF                PIC X(6300000).
       01 FPTR                     PIC 9(8)  COMP-5.
       01 ESC                      PIC X     VALUE X"1B".
       01 LF                       PIC X     VALUE X"0A".
       01 HALF-BLOCK               PIC X(3)  VALUE X"E29680".
       01 FG-SEQ.
          05 FILLER                PIC X     VALUE X"1B".
          05 FILLER                PIC X(6)  VALUE "[38;2;".
          05 FG-R                  PIC 999.
          05 FILLER                PIC X     VALUE ";".
          05 FG-G                  PIC 999.
          05 FILLER                PIC X     VALUE ";".
          05 FG-B                  PIC 999.
          05 FILLER                PIC X     VALUE "m".
       01 BG-SEQ.
          05 FILLER                PIC X     VALUE X"1B".
          05 FILLER                PIC X(6)  VALUE "[48;2;".
          05 BG-R                  PIC 999.
          05 FILLER                PIC X     VALUE ";".
          05 BG-G                  PIC 999.
          05 FILLER                PIC X     VALUE ";".
          05 BG-B                  PIC 999.
          05 FILLER                PIC X     VALUE "m".
       01 ROW-PIXELS.
          05 TOP-PIX OCCURS 1000 TIMES.
             10 TOP-R              PIC S9(4) COMP-5.
             10 TOP-G              PIC S9(4) COMP-5.
             10 TOP-B              PIC S9(4) COMP-5.
       01 LAST-FG                  PIC S9(9) COMP-5.
       01 LAST-BG                  PIC S9(9) COMP-5.
       01 CUR-FG                   PIC S9(9) COMP-5.
       01 CUR-BG                   PIC S9(9) COMP-5.
       01 STATUS-LINE              PIC X(80).

       PROCEDURE DIVISION.
      *================================================================
       MAIN-PARA.
           PERFORM PARSE-ARGS
           PERFORM INIT-TABLES
           PERFORM INIT-CAMERA-RAYS
           IF MODE-VIDEO
               PERFORM VARYING W-FRAME FROM W-FIRST BY 1
                       UNTIL W-FRAME > W-LAST
                   COMPUTE SCENE-T = W-FRAME / 30
                   PERFORM RENDER-VIDEO-FRAME
               END-PERFORM
           ELSE
               PERFORM LIVE-LOOP
           END-IF
           STOP RUN.

      *================================================================
       PARSE-ARGS.
           ACCEPT W-ARGC FROM ARGUMENT-NUMBER
           IF W-ARGC >= 1
               ACCEPT W-ARG FROM ARGUMENT-VALUE
               MOVE W-ARG TO W-MODE
           END-IF
           IF MODE-VIDEO
               MOVE 640 TO W-COLS
               MOVE 360 TO W-ROWS
               MOVE 299 TO W-LAST
               IF W-ARGC >= 2
                   ACCEPT W-ARG FROM ARGUMENT-VALUE
                   COMPUTE W-COLS = FUNCTION NUMVAL(W-ARG)
               END-IF
               IF W-ARGC >= 3
                   ACCEPT W-ARG FROM ARGUMENT-VALUE
                   COMPUTE W-ROWS = FUNCTION NUMVAL(W-ARG)
               END-IF
               IF W-ARGC >= 4
                   ACCEPT W-ARG FROM ARGUMENT-VALUE
                   COMPUTE W-FIRST = FUNCTION NUMVAL(W-ARG)
               END-IF
               IF W-ARGC >= 5
                   ACCEPT W-ARG FROM ARGUMENT-VALUE
                   COMPUTE W-LAST = FUNCTION NUMVAL(W-ARG)
               END-IF
               MOVE W-COLS TO IMG-W
               MOVE W-ROWS TO IMG-H
           ELSE
               IF W-ARGC >= 2
                   ACCEPT W-ARG FROM ARGUMENT-VALUE
                   COMPUTE W-COLS = FUNCTION NUMVAL(W-ARG)
               END-IF
               IF W-ARGC >= 3
                   ACCEPT W-ARG FROM ARGUMENT-VALUE
                   COMPUTE W-ROWS = FUNCTION NUMVAL(W-ARG)
               END-IF
               IF W-ARGC >= 4
                   ACCEPT W-ARG FROM ARGUMENT-VALUE
                   COMPUTE W-SECS = FUNCTION NUMVAL(W-ARG)
               END-IF
               MOVE W-COLS TO IMG-W
               COMPUTE IMG-H = W-ROWS * 2
           END-IF
           IF IMG-W * IMG-H > MAX-PIX
              OR (NOT MODE-VIDEO AND IMG-W > 1000)
               DISPLAY "RAYTRACE: IMAGE TOO LARGE (MAX 1920X1080)"
                   UPON SYSERR
               STOP RUN
           END-IF.

      *================================================================
       INIT-TABLES.
      *    SQ(K+1) = SQRT(K / FX) IN FIXED POINT = 64 * SQRT(K)
           PERFORM VARYING K FROM 0 BY 1 UNTIL K > SQ-MAX
               COMPUTE SQ(K + 1) ROUNDED = 64 * FUNCTION SQRT(K)
           END-PERFORM
      *    GAMMA 2.2: LINEAR 0..4096 -> SRGB 0..255
           PERFORM VARYING K FROM 0 BY 1 UNTIL K > FX
               COMPUTE F-A = K / FX
               COMPUTE GAM(K + 1) ROUNDED =
                   255 * F-A ** (1 / 2.2)
           END-PERFORM
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 256
               MOVE FUNCTION CHAR(K) TO BYTE-TAB(K:1)
           END-PERFORM
      *    SUN DIRECTION (TOWARD THE LIGHT), LOW FOR LONG SHADOWS
           COMPUTE F-L = FUNCTION SQRT(0.55 ** 2 + 0.50 ** 2
                                       + 0.45 ** 2)
           COMPUTE LTX ROUNDED = -0.55 / F-L * FX
           COMPUTE LTY ROUNDED =  0.50 / F-L * FX
           COMPUTE LTZ ROUNDED =  0.45 / F-L * FX.

      *================================================================
      * UNIT RAY THROUGH EACH PIXEL CENTER IN CAMERA SPACE. ROTATING
      * INTO WORLD SPACE LATER PRESERVES LENGTH, SO NO PER-FRAME SQRT.
       INIT-CAMERA-RAYS.
           PERFORM VARYING SCR-Y FROM 0 BY 1 UNTIL SCR-Y >= IMG-H
               COMPUTE F-V = (1 - (2 * SCR-Y + 1) / IMG-H) * F-TAN
               PERFORM VARYING SCR-X FROM 0 BY 1
                       UNTIL SCR-X >= IMG-W
                   COMPUTE F-U = ((2 * SCR-X + 1) / IMG-W - 1)
                                 * F-TAN * IMG-W / IMG-H
                   COMPUTE F-L = FUNCTION SQRT(F-U * F-U
                                 + F-V * F-V + 1)
                   COMPUTE PIX = SCR-Y * IMG-W + SCR-X + 1
                   COMPUTE CAM-U(PIX) ROUNDED = F-U / F-L * FX
                   COMPUTE CAM-V(PIX) ROUNDED = F-V / F-L * FX
                   COMPUTE CAM-W(PIX) ROUNDED = 1 / F-L * FX
               END-PERFORM
           END-PERFORM.

      *================================================================
       LIVE-LOOP.
           PERFORM READ-CLOCK
           MOVE T-NOW TO T-START
           DISPLAY ESC "[2J" WITH NO ADVANCING
           MOVE 0 TO SCENE-T
           PERFORM UNTIL SCENE-T > W-SECS
               PERFORM RENDER-LIVE-FRAME
               ADD 1 TO W-FRAME
               PERFORM READ-CLOCK
               COMPUTE SCENE-T = T-NOW - T-START
               IF SCENE-T < 0
                   ADD 86400 TO SCENE-T
               END-IF
           END-PERFORM
           DISPLAY ESC "[0m".

       READ-CLOCK.
           ACCEPT W-CLOCK FROM TIME
           COMPUTE T-NOW = CLK-HH * 3600 + CLK-MM * 60 + CLK-SS
                           + CLK-CC / 100.

      *================================================================
      * PER-FRAME SCENE ANIMATION (FLOATING POINT, ~40 OPS PER FRAME)
       UPDATE-SCENE.
      *    1: CHROME CENTERPIECE
           MOVE 0 TO SPH-X(1) SPH-Z(1)
           COMPUTE SPH-Y(1) ROUNDED = 1.25 * FX
           COMPUTE SPH-R(1) ROUNDED = 1.25 * FX
           MOVE 3850 TO SPH-AR(1) SPH-AG(1)
           MOVE 4000 TO SPH-AB(1)
           MOVE 3200 TO SPH-FR(1) SPH-FG(1)
           MOVE 3400 TO SPH-FB(1)
           MOVE 4096 TO SPH-SP(1)
      *    2: RUBY, ORBITS AND BOUNCES
           COMPUTE F-A = SCENE-T * 0.9
           COMPUTE SPH-X(2) ROUNDED = 2.6 * FUNCTION COS(F-A) * FX
           COMPUTE SPH-Z(2) ROUNDED = 2.6 * FUNCTION SIN(F-A) * FX
           COMPUTE SPH-Y(2) ROUNDED = (0.7 + 1.3 *
               FUNCTION ABS(FUNCTION SIN(SCENE-T * 2.3))) * FX
           COMPUTE SPH-R(2) ROUNDED = 0.7 * FX
           MOVE 3600 TO SPH-AR(2)
           MOVE  330 TO SPH-AG(2)
           MOVE  300 TO SPH-AB(2)
           MOVE  700 TO SPH-FR(2) SPH-FG(2) SPH-FB(2)
           MOVE 3000 TO SPH-SP(2)
      *    3: SAPPHIRE, WIDER ORBIT THE OTHER WAY
           COMPUTE F-A = 2.1 - SCENE-T * 0.55
           COMPUTE SPH-X(3) ROUNDED = 4.0 * FUNCTION COS(F-A) * FX
           COMPUTE SPH-Z(3) ROUNDED = 4.0 * FUNCTION SIN(F-A) * FX
           COMPUTE SPH-Y(3) ROUNDED = (0.6 + 0.9 *
               FUNCTION ABS(FUNCTION SIN(SCENE-T * 1.7 + 1))) * FX
           COMPUTE SPH-R(3) ROUNDED = 0.6 * FX
           MOVE  300 TO SPH-AR(3)
           MOVE 1300 TO SPH-AG(3)
           MOVE 3800 TO SPH-AB(3)
           MOVE  900 TO SPH-FR(3) SPH-FG(3) SPH-FB(3)
           MOVE 3000 TO SPH-SP(3)
      *    4: GOLD, HOVERING HIGH AND FAST (TINTED METAL REFLECTION)
           COMPUTE F-A = 4.0 + SCENE-T * 1.4
           COMPUTE SPH-X(4) ROUNDED = 2.0 * FUNCTION COS(F-A) * FX
           COMPUTE SPH-Z(4) ROUNDED = 2.0 * FUNCTION SIN(F-A) * FX
           COMPUTE SPH-Y(4) ROUNDED = (2.9 + 0.35 *
               FUNCTION SIN(SCENE-T * 3.1)) * FX
           COMPUTE SPH-R(4) ROUNDED = 0.42 * FX
           MOVE 4000 TO SPH-AR(4)
           MOVE 2900 TO SPH-AG(4)
           MOVE 1000 TO SPH-AB(4)
           MOVE 3500 TO SPH-FR(4)
           MOVE 2500 TO SPH-FG(4)
           MOVE  800 TO SPH-FB(4)
           MOVE 4096 TO SPH-SP(4)

      *    CAMERA ORBITS, BOBS, LOOKS AT THE CHROME SPHERE
           COMPUTE F-A  = SCENE-T * 0.22
           COMPUTE F-PX = 6.8 * FUNCTION SIN(F-A)
           COMPUTE F-PZ = -6.8 * FUNCTION COS(F-A)
           COMPUTE F-PY = 1.9 + 0.7 * FUNCTION SIN(SCENE-T * 0.37)
           COMPUTE F-FX = 0 - F-PX
           COMPUTE F-FY = 1.1 - F-PY
           COMPUTE F-FZ = 0 - F-PZ
           COMPUTE F-L = FUNCTION SQRT(F-FX * F-FX + F-FY * F-FY
                                       + F-FZ * F-FZ)
           COMPUTE F-FX = F-FX / F-L
           COMPUTE F-FY = F-FY / F-L
           COMPUTE F-FZ = F-FZ / F-L
      *    RIGHT = WORLD-UP X FORWARD, UP = FORWARD X RIGHT
           COMPUTE F-L = FUNCTION SQRT(F-FZ * F-FZ + F-FX * F-FX)
           COMPUTE F-RX = F-FZ / F-L
           COMPUTE F-RZ = 0 - F-FX / F-L
           COMPUTE CAMX ROUNDED = F-PX * FX
           COMPUTE CAMY ROUNDED = F-PY * FX
           COMPUTE CAMZ ROUNDED = F-PZ * FX
           COMPUTE FWX ROUNDED = F-FX * FX
           COMPUTE FWY ROUNDED = F-FY * FX
           COMPUTE FWZ ROUNDED = F-FZ * FX
           COMPUTE RTX ROUNDED = F-RX * FX
           MOVE 0 TO RTY
           COMPUTE RTZ ROUNDED = F-RZ * FX
           COMPUTE UPX ROUNDED = (F-FY * F-RZ) * FX
           COMPUTE UPY ROUNDED = (F-FZ * F-RX - F-FX * F-RZ) * FX
           COMPUTE UPZ ROUNDED = (0 - F-FY * F-RX) * FX

      *    TERMS THAT DEPEND ONLY ON THE CAMERA, FOR PRIMARY RAYS
           PERFORM VARYING SK FROM 1 BY 1 UNTIL SK > N-SPH
               COMPUTE SPH-R2(SK) = SPH-R(SK) * SPH-R(SK) / FX
               COMPUTE SPH-OCX(SK) = CAMX - SPH-X(SK)
               COMPUTE SPH-OCY(SK) = CAMY - SPH-Y(SK)
               COMPUTE SPH-OCZ(SK) = CAMZ - SPH-Z(SK)
               COMPUTE SPH-OCC(SK) = (SPH-OCX(SK) * SPH-OCX(SK)
                   + SPH-OCY(SK) * SPH-OCY(SK)
                   + SPH-OCZ(SK) * SPH-OCZ(SK)) / FX - SPH-R2(SK)
           END-PERFORM.

      *================================================================
       RENDER-VIDEO-FRAME.
           PERFORM UPDATE-SCENE
           MOVE 1 TO FPTR
           PERFORM VARYING SCR-Y FROM 0 BY 1 UNTIL SCR-Y >= IMG-H
               PERFORM VARYING SCR-X FROM 0 BY 1
                       UNTIL SCR-X >= IMG-W
                   PERFORM TRACE-PIXEL
                   MOVE BYTE-TAB(OUT-R + 1:1) TO FRAME-BUF(FPTR:1)
                   MOVE BYTE-TAB(OUT-G + 1:1)
                       TO FRAME-BUF(FPTR + 1:1)
                   MOVE BYTE-TAB(OUT-B + 1:1)
                       TO FRAME-BUF(FPTR + 2:1)
                   ADD 3 TO FPTR
               END-PERFORM
           END-PERFORM
           DISPLAY FRAME-BUF(1:FPTR - 1) WITH NO ADVANCING.

      *================================================================
      * EACH TERMINAL CELL IS AN UPPER-HALF BLOCK: FOREGROUND = TOP
      * PIXEL, BACKGROUND = BOTTOM PIXEL. COLOR CODES ARE ONLY SENT
      * WHEN THEY CHANGE.
       RENDER-LIVE-FRAME.
           PERFORM UPDATE-SCENE
           MOVE 1 TO FPTR
           STRING ESC "[H" DELIMITED BY SIZE
               INTO FRAME-BUF WITH POINTER FPTR
           PERFORM VARYING C-ROW FROM 0 BY 1 UNTIL C-ROW >= W-ROWS
               COMPUTE SCR-Y = C-ROW * 2
               PERFORM VARYING SCR-X FROM 0 BY 1
                       UNTIL SCR-X >= IMG-W
                   PERFORM TRACE-PIXEL
                   MOVE OUT-R TO TOP-R(SCR-X + 1)
                   MOVE OUT-G TO TOP-G(SCR-X + 1)
                   MOVE OUT-B TO TOP-B(SCR-X + 1)
               END-PERFORM
               ADD 1 TO SCR-Y
               MOVE -1 TO LAST-FG LAST-BG
               PERFORM VARYING SCR-X FROM 0 BY 1
                       UNTIL SCR-X >= IMG-W
                   PERFORM TRACE-PIXEL
                   COMPUTE CUR-FG = TOP-R(SCR-X + 1) * 65536
                       + TOP-G(SCR-X + 1) * 256 + TOP-B(SCR-X + 1)
                   COMPUTE CUR-BG = OUT-R * 65536 + OUT-G * 256
                       + OUT-B
                   IF CUR-FG NOT = LAST-FG
                       MOVE CUR-FG TO LAST-FG
                       MOVE TOP-R(SCR-X + 1) TO FG-R
                       MOVE TOP-G(SCR-X + 1) TO FG-G
                       MOVE TOP-B(SCR-X + 1) TO FG-B
                       MOVE FG-SEQ TO FRAME-BUF(FPTR:19)
                       ADD 19 TO FPTR
                   END-IF
                   IF CUR-BG NOT = LAST-BG
                       MOVE CUR-BG TO LAST-BG
                       MOVE OUT-R TO BG-R
                       MOVE OUT-G TO BG-G
                       MOVE OUT-B TO BG-B
                       MOVE BG-SEQ TO FRAME-BUF(FPTR:19)
                       ADD 19 TO FPTR
                   END-IF
                   MOVE HALF-BLOCK TO FRAME-BUF(FPTR:3)
                   ADD 3 TO FPTR
               END-PERFORM
               STRING ESC "[0m" LF DELIMITED BY SIZE
                   INTO FRAME-BUF WITH POINTER FPTR
           END-PERFORM
           IF SCENE-T > 0
               COMPUTE W-FPS = W-FRAME / SCENE-T
           END-IF
           MOVE W-FPS TO FPS-OUT
           MOVE W-FRAME TO FRAME-OUT
           MOVE SPACES TO STATUS-LINE
           STRING " COBOL RAY TRACER  |  FRAME" FRAME-OUT
                  "  |" FPS-OUT " FPS"
               DELIMITED BY SIZE INTO STATUS-LINE
           STRING STATUS-LINE DELIMITED BY SIZE
               INTO FRAME-BUF WITH POINTER FPTR
           DISPLAY FRAME-BUF(1:FPTR - 1) WITH NO ADVANCING.

      *================================================================
      * FOLLOW ONE PIXEL'S RAY THROUGH UP TO MAX-DEPTH MIRROR
      * BOUNCES. NO RECURSION IN COBOL, SO THE REFLECTION TREE IS
      * UNROLLED INTO A LOOP CARRYING A PER-CHANNEL THROUGHPUT WEIGHT.
       TRACE-PIXEL.
           COMPUTE PIX = SCR-Y * IMG-W + SCR-X + 1
           MOVE CAMX TO ORX
           MOVE CAMY TO ORY
           MOVE CAMZ TO ORZ
           COMPUTE DRX = (RTX * CAM-U(PIX) + UPX * CAM-V(PIX)
                          + FWX * CAM-W(PIX)) / FX
           COMPUTE DRY = (UPY * CAM-V(PIX) + FWY * CAM-W(PIX)) / FX
           COMPUTE DRZ = (RTZ * CAM-U(PIX) + UPZ * CAM-V(PIX)
                          + FWZ * CAM-W(PIX)) / FX
           MOVE 0 TO ACC-R ACC-G ACC-B W-DEPTH W-DONE
           MOVE FX TO WGT-R WGT-G WGT-B
           MOVE 1 TO W-PRIMARY
           PERFORM UNTIL W-DONE = 1
               PERFORM INTERSECT
               IF HIT-K = 0
                   PERFORM SKY-COLOR
                   COMPUTE ACC-R = ACC-R + WGT-R * LOC-R / FX
                   COMPUTE ACC-G = ACC-G + WGT-G * LOC-G / FX
                   COMPUTE ACC-B = ACC-B + WGT-B * LOC-B / FX
                   MOVE 1 TO W-DONE
               ELSE
                   PERFORM SHADE-HIT
                   IF W-DEPTH >= MAX-DEPTH
                       MOVE 0 TO REF-R REF-G REF-B
                   END-IF
                   COMPUTE ACC-R = ACC-R + WGT-R * (FX - REF-R)
                                   * LOC-R / 16777216
                   COMPUTE ACC-G = ACC-G + WGT-G * (FX - REF-G)
                                   * LOC-G / 16777216
                   COMPUTE ACC-B = ACC-B + WGT-B * (FX - REF-B)
                                   * LOC-B / 16777216
                   COMPUTE WGT-R = WGT-R * REF-R / FX
                   COMPUTE WGT-G = WGT-G * REF-G / FX
                   COMPUTE WGT-B = WGT-B * REF-B / FX
                   IF WGT-R < 60 AND WGT-G < 60 AND WGT-B < 60
                       MOVE 1 TO W-DONE
                   ELSE
      *                CONTINUE ALONG THE MIRROR DIRECTION
                       COMPUTE ORX = PX + NX / 100
                       COMPUTE ORY = PY + NY / 100
                       COMPUTE ORZ = PZ + NZ / 100
                       MOVE RFX TO DRX
                       MOVE RFY TO DRY
                       MOVE RFZ TO DRZ
                       MOVE 0 TO W-PRIMARY
                       ADD 1 TO W-DEPTH
                   END-IF
               END-IF
           END-PERFORM
           IF ACC-R > FX MOVE FX TO ACC-R END-IF
           IF ACC-G > FX MOVE FX TO ACC-G END-IF
           IF ACC-B > FX MOVE FX TO ACC-B END-IF
           IF ACC-R < 0 MOVE 0 TO ACC-R END-IF
           IF ACC-G < 0 MOVE 0 TO ACC-G END-IF
           IF ACC-B < 0 MOVE 0 TO ACC-B END-IF
           MOVE GAM(ACC-R + 1) TO OUT-R
           MOVE GAM(ACC-G + 1) TO OUT-G
           MOVE GAM(ACC-B + 1) TO OUT-B.

      *================================================================
      * NEAREST HIT ALONG (OR, DR). SETS HIT-K (0 = SKY, 9 = FLOOR).
      * RAY-SPHERE: B = OC.D, C = OC.OC - R^2, T = -B - SQRT(B^2-C).
      * B >= 0 MEANS THE SPHERE IS BEHIND US, SO IT IS SKIPPED EARLY.
      * B^2 - C CANCELS TWO LARGE NUMBERS, SO IN FIXED POINT IT IS
      * ONLY A CHEAP REJECT TEST. SURVIVORS USE THE STABLE FORM
      * R^2 - |OC - B*D|^2, OR SILHOUETTES COME OUT FUZZY.
       INTERSECT.
           MOVE T-BIG TO TMIN
           MOVE 0 TO HIT-K
           PERFORM VARYING SK FROM 1 BY 1 UNTIL SK > N-SPH
               IF W-PRIMARY = 1
                   MOVE SPH-OCX(SK) TO OCX
                   MOVE SPH-OCY(SK) TO OCY
                   MOVE SPH-OCZ(SK) TO OCZ
               ELSE
                   COMPUTE OCX = ORX - SPH-X(SK)
                   COMPUTE OCY = ORY - SPH-Y(SK)
                   COMPUTE OCZ = ORZ - SPH-Z(SK)
               END-IF
               COMPUTE BB = (OCX * DRX + OCY * DRY + OCZ * DRZ) / FX
               IF BB < 0
                   IF W-PRIMARY = 1
                       MOVE SPH-OCC(SK) TO CCV
                   ELSE
                       COMPUTE CCV = (OCX * OCX + OCY * OCY
                                      + OCZ * OCZ) / FX - SPH-R2(SK)
                   END-IF
                   COMPUTE DISC = BB * BB / FX - CCV
      *            ROUNDING ERROR GROWS WITH C, SO THE REJECT
      *            MARGIN DOES TOO. BOUNCED RAYS ARE LESS EXACT
      *            THAN CAMERA RAYS AND GET A FIXED WIDE MARGIN.
                   IF W-PRIMARY = 1
                       IF DISC * 256 + CCV > -8192
                           PERFORM STABLE-DISC
                       END-IF
                   ELSE
                       IF DISC > -4096
                           PERFORM STABLE-DISC
                       END-IF
                   END-IF
                   IF DISC > 0
                       IF DISC > SQ-MAX
                           MOVE SQ-MAX TO DISC
                       END-IF
                       COMPUTE TC = 0 - BB - SQ(DISC + 1)
                       IF TC > T-EPS AND TC < TMIN
                           MOVE TC TO TMIN
                           MOVE SK TO HIT-K
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF DRY < -2 AND ORY > 0
               COMPUTE TC = 0 - ORY * FX / DRY
               IF TC < TMIN AND TC < T-FAR
                   MOVE TC TO TMIN
                   MOVE 9 TO HIT-K
               END-IF
           END-IF.

       STABLE-DISC.
           COMPUTE QX = OCX - BB * DRX / FX
           COMPUTE QY = OCY - BB * DRY / FX
           COMPUTE QZ = OCZ - BB * DRZ / FX
           COMPUTE DISC = SPH-R2(SK)
               - (QX * QX + QY * QY + QZ * QZ) / FX.

      *================================================================
      * LOCAL COLOR AT THE HIT, PLUS THE MIRROR DIRECTION AND
      * PER-CHANNEL REFLECTANCE FOR THE NEXT BOUNCE.
       SHADE-HIT.
           COMPUTE PX = ORX + DRX * TMIN / FX
           COMPUTE PY = ORY + DRY * TMIN / FX
           COMPUTE PZ = ORZ + DRZ * TMIN / FX
           IF HIT-K = 9
               MOVE 0  TO NX NZ PY
               MOVE FX TO NY
      *        CHECKERBOARD: PARITY OF FLOOR(X) + FLOOR(Z)
               COMPUTE IX = (PX + 4096000) / FX
               COMPUTE IZ = (PZ + 4096000) / FX
               COMPUTE PAR-Q = IX + IZ
               DIVIDE PAR-Q BY 2 GIVING IX REMAINDER PAR-R
               IF PAR-R = 0
                   MOVE 3000 TO ALB-R
                   MOVE 2800 TO ALB-G
                   MOVE 2450 TO ALB-B
               ELSE
                   MOVE  240 TO ALB-R
                   MOVE  270 TO ALB-G
                   MOVE  340 TO ALB-B
               END-IF
      *        SCHLICK-STYLE FRESNEL: SHINIER AT GRAZING ANGLES
               COMPUTE GL = FX + DRY
               COMPUTE REF-R = 220 + GL * GL / FX * GL / FX * 0.55
      *        FOG: FADE DISTANT FLOOR (AND ITS REFLECTION) TO HAZE
               COMPUTE D2  = TMIN * TMIN / FX
               COMPUTE FOG = D2 * FX / (D2 + 1600 * FX)
               COMPUTE REF-R = REF-R * (FX - FOG) / FX
               MOVE REF-R TO REF-G REF-B
               MOVE 1400 TO SPECK
           ELSE
               COMPUTE NX = (PX - SPH-X(HIT-K)) * FX / SPH-R(HIT-K)
               COMPUTE NY = (PY - SPH-Y(HIT-K)) * FX / SPH-R(HIT-K)
               COMPUTE NZ = (PZ - SPH-Z(HIT-K)) * FX / SPH-R(HIT-K)
               MOVE SPH-AR(HIT-K) TO ALB-R
               MOVE SPH-AG(HIT-K) TO ALB-G
               MOVE SPH-AB(HIT-K) TO ALB-B
               MOVE SPH-FR(HIT-K) TO REF-R
               MOVE SPH-FG(HIT-K) TO REF-G
               MOVE SPH-FB(HIT-K) TO REF-B
               MOVE SPH-SP(HIT-K) TO SPECK
               MOVE 0 TO FOG
           END-IF

      *    MIRROR DIRECTION R = D - 2 (D.N) N
           COMPUTE DN  = (DRX * NX + DRY * NY + DRZ * NZ) / FX
           COMPUTE RFX = DRX - 2 * DN * NX / FX
           COMPUTE RFY = DRY - 2 * DN * NY / FX
           COMPUTE RFZ = DRZ - 2 * DN * NZ / FX

      *    LAMBERT DIFFUSE WITH SHADOW RAY TOWARD THE SUN
           COMPUTE DIFF = (NX * LTX + NY * LTY + NZ * LTZ) / FX
           MOVE 0 TO SHAD SPEC
           IF DIFF > 0
               PERFORM SHADOW-TEST
           ELSE
               MOVE 0 TO DIFF
           END-IF
           COMPUTE DS = DIFF * SHAD / FX

      *    PHONG HIGHLIGHT: (R.L)^32 BY FIVE SQUARINGS
           IF SHAD > 0
               COMPUTE GL = (RFX * LTX + RFY * LTY + RFZ * LTZ) / FX
               IF GL > 0
                   COMPUTE GL = GL * GL / FX
                   COMPUTE GL = GL * GL / FX
                   COMPUTE GL = GL * GL / FX
                   COMPUTE GL = GL * GL / FX
                   COMPUTE GL = GL * GL / FX
                   COMPUTE SPEC = GL * SPECK / FX * SHAD / FX * 1.4
               END-IF
           END-IF

      *    HEMISPHERE AMBIENT: BRIGHTER ON UPWARD-FACING SURFACES
           COMPUTE AMB = (FX + NY) * 0.085
      *    SUN IS WARM WHITE, SKY FILL IS COOL BLUE
           COMPUTE LOC-R = ALB-R * (AMB * 0.62 + DS * 1.30) / FX
                           + SPEC
           COMPUTE LOC-G = ALB-G * (AMB * 0.72 + DS * 1.20) / FX
                           + SPEC
           COMPUTE LOC-B = ALB-B * (AMB * 0.95 + DS * 1.02) / FX
                           + SPEC * 0.9
           IF FOG > 0
               COMPUTE LOC-R = LOC-R + (3500 - LOC-R) * FOG / FX
               COMPUTE LOC-G = LOC-G + (3250 - LOC-G) * FOG / FX
               COMPUTE LOC-B = LOC-B + (3150 - LOC-B) * FOG / FX
           END-IF.

      *================================================================
      * SHADOW RAY FROM P TOWARD THE SUN. ONLY A YES/NO TEST IS
      * NEEDED, SO NO SQUARE ROOT: COMPARE THE RAY'S CLOSEST-APPROACH
      * DISTANCE^2 TO R^2. A SHORT BAND OUTSIDE R^2 GIVES A PENUMBRA.
       SHADOW-TEST.
           MOVE FX TO SHAD
           PERFORM VARYING SK FROM 1 BY 1 UNTIL SK > N-SPH
               IF SK NOT = HIT-K
                   COMPUTE OCX = PX - SPH-X(SK)
                   COMPUTE OCY = PY - SPH-Y(SK)
                   COMPUTE OCZ = PZ - SPH-Z(SK)
                   COMPUTE BB = (OCX * LTX + OCY * LTY + OCZ * LTZ)
                                / FX
                   IF BB < 0
                       COMPUTE D2 = (OCX * OCX + OCY * OCY
                                     + OCZ * OCZ - BB * BB) / FX
                       IF D2 < SPH-R2(SK)
                           MOVE 0 TO SHAD
                           EXIT PERFORM
                       END-IF
                       IF D2 < SPH-R2(SK) * 1.5
                           COMPUTE GL = (D2 - SPH-R2(SK)) * FX
                                        / (SPH-R2(SK) * 0.5)
                           IF GL < SHAD
                               MOVE GL TO SHAD
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

      *================================================================
      * SKY: HAZY HORIZON TO DEEP BLUE ZENITH, PLUS SUN DISK AND GLOW.
       SKY-COLOR.
           IF DRY < 0
               MOVE 0 TO SKT
           ELSE
               MOVE DRY TO SKT
           END-IF
           COMPUTE SKT = SKT * (2 * FX - SKT) / FX
           COMPUTE LOC-R = 3500 + (330  - 3500) * SKT / FX
           COMPUTE LOC-G = 3250 + (900  - 3250) * SKT / FX
           COMPUTE LOC-B = 3150 + (2900 - 3150) * SKT / FX
           COMPUTE GL = (DRX * LTX + DRY * LTY + DRZ * LTZ) / FX
           IF GL > 3600
               COMPUTE GL = (GL - 3600) * FX / 496
               COMPUTE GL = GL * GL / FX
               COMPUTE GL = GL * GL / FX
               COMPUTE LOC-R = LOC-R + GL * 0.9
               COMPUTE LOC-G = LOC-G + GL * 0.65
               COMPUTE LOC-B = LOC-B + GL * 0.35
           END-IF.
