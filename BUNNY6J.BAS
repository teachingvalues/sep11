'Bunny 5F
'Instructions:
'Press   Alt  R  S   to Start Game
'Press   Alt  F  X   to Exit QBasic
'Type    QUIT  to quit   (Y  to quit,   N  for new game)


'Automatic Answers:
'Press  "Enter"  until you die
'Press  "U"  for Undo, then continue is possible
'Press  "\" (backslash) to stop automatic answers


'Your Bunny Story:
'Your adventure is saved now in a file called  "BunnyDoc.txt"
'Look in current directory (QBasic Start Directory)
'You can also use Windows "Start, Find, Files, BunnyDoc.txt" Command






'Bunny 5E - Working on a story log


'Bunny 5a
'Early Death

'Bunny 4D  09/10/97
'possible family adventure
'barn yard vocabulary



'Bunny 4C  9/09/97
'WORD SUBSTITUTION


'Bunny 4B  9/09/97
  'Have the little loop now with the std input
  'Have control of the INPUT LOOP!!!
  'Got the lamp


'Bunny 4  1/24/97 (decoding, moving, hunting)


'USEFUL QBASIC KEYS
'  control break                  to stop program
'  control home                   to get to these notes
'  shift f5                       to run it again and again
'  alt F X                        to end program
'  apos, down arrow, left arrow,  to add comments

'FOUND CLUES 1/24/97
'  close coffin
'  look field (carrot field)

'ADDED ROOM DIRECTION DISPLAY 1/23/97
'  FOR j = 0 TO 5: PRINT MA(R, j); : NEXT:
'  where R = Room Number

'ADDED BLANK LINES TO THE CODE 1/23/97
'  to break it up into function sections

'ADDED COLOR SUBROUTINES AND FUNCTIONS 1/23/97
'  added printit(), First and ButFirst

'CORRECTED HELP COMMAND 1/22/97
'   shorter
'   now lists some commands that work

'SUGGESTIONS FOR NEXT VERSION (done)
'   add some color to the splash screen (start up screen)
'   add color to the location phrases

'TUTORIAL HEADER 1/22/97
'rework this tutorial section regularly
'put headers
'eventually move it into work


'ADDITIONAL PROGRAMMING NOTES
'set your computer up so you can quickly get to the program
'shortcut
'tab and arrow to the program (from the desktop in Win 95)
'have the directories without length and spaces
'cut and paste between versions


'added tutorial notes
'  and dated and signed them
'

'added COLOR 14: PRINT LO$: COLOR 1 to the print routines
'This should be a subroutine soon

'I will start doing saved versions
'and will include these notes at the start
'and at the start of each of these upgrades that I do.



DECLARE SUB RemoveColorNumbers (Sent$, CleanSent$)
DECLARE FUNCTION ButFirst$ (sentence$)
DECLARE SUB CategoryColor (Sent$)
DECLARE FUNCTION Cent$ (Sent$)
DECLARE SUB CenterIt (Sent$)
DECLARE SUB ColorIt1 ()
DECLARE SUB Printit2 (Sent$, Remaining!)
DECLARE FUNCTION First$ (Sent$)
DECLARE SUB sparkle ()
DECLARE SUB NewsInitScreen ()
DECLARE SUB PRINTit (Sent$)
DIM SHARED StoryLog


'option explicit

'SHELL "dir *.txt"
'OPEN "BunnyDoc.Txt" FOR OUTPUT AS #2
'StoryLog = 1: 'Flag to turn on StoryLog


'OPEN "C:\HHHH.TXT" FOR OUTPUT AS #1
'PRINT #1, "hI"
'CLOSE #1










OPEN "C:\HAM02.txt" FOR INPUT AS #1
FOR j = 1 TO 5
FOR i = 1 TO 15
IF NOT EOF(1) THEN LINE INPUT #1, a$

PRINTit a$
LINE INPUT ".."; z$
CLS
NEXT i
LINE INPUT "/ "; z$
NEXT j

CLOSE #1
LINE INPUT "> "; z$




END





10 KEY OFF: SCREEN 0: COLOR 7, 0: PALETTE: CLS : RANDOMIZE TIMER
20 NO = 9: R = 5: ND = 6: NR = 27: MOVE = 0: NI = 2
30 DIM R$(NR), OB$(NO), O2$(NO), OB(NO), MA(28, 6), c$(11)
35 CLS : GOSUB 27000: CLS
40 IF RND > .5 THEN PRINT "I'll be back in a sec..." ELSE PRINT "Please wait..."
50 GOSUB 25000: GOSUB 26000
55 VIEW PRINT 1 TO 25: CLS
60 COLOR 0, 7: LOCATE 25, 41: PRINT "Score:"; : COLOR 7, 0: VIEW PRINT 1 TO 24: LOCATE 1, 1
90 GOSUB 700
91 IF FFF THEN FFF = 0: GOTO 820
92 GOSUB 600: GOSUB 300
100 IF VAMP THEN VAMPT = VAMPT + 1: IF VAMPT = 2 THEN PRINTit ("The 1 vampire decides to fang you."): GOTO 810

102 IF R = 25 AND NOT DBUN THEN RABTI = RABTI + 1 ELSE RABTI = 0
104 IF R = 25 AND NOT DBUN THEN IF RABTI = 3 THEN PRINTit ("The vampire bunny nibbles a little on your 1 leg."): GOTO 810

108 IF c$(1) <> "" THEN cm$ = c$(1): BF = 0: GOTO 126



109 cm$ = "":

IF inkeyFlag = 1 THEN PRINT : LINE INPUT ">"; cm$: GOTO Label109
IF StartFlag = 0 THEN
        COLOR 15: PRINT : PRINT "Press the <Enter> for automatic Play, <\> to play yourself":
        PRINT : StartFlag = 1: COLOR 7
END IF

'Here is the Main READ Look for the AI COMMANDS

READ cm$
  IF cm$ = "EndOfData" THEN
    RESTORE 9999
    LINE INPUT ">"; cm$:
  ELSE
    COLOR 15: PRINT : PRINT cm$;
'inkeyLabel:
'    IF INKEY$ = "" THEN GOTO inkeyLabel:
    LINE INPUT "", z$: IF z$ = "\" OR cm$ = "\" THEN inkeyFlag = 1
    PRINT : COLOR 7
  END IF

Label109:
IF cm$ = "" THEN PRINT "What's that?": GOTO 109
IF StoryLog = 1 THEN PRINT #2, : PRINT #2, ">" + cm$: PRINT #2,

110 FOR i = 1 TO LEN(cm$): G$ = MID$(cm$, i, 1): IF ASC(G$) > 96 AND ASC(G$) < 123 THEN G = ASC(G$) - 32: MID$(cm$, i) = CHR$(G)
111 NEXT

114 FOR i = 1 TO LEN(cm$)
115 IF MID$(cm$, i, 4) = "THE " THEN G$ = LEFT$(cm$, i - 1): GG$ = MID$(cm$, i + 4): cm$ = G$ + GG$: GOTO 115 ELSE 116
116 NEXT: c = 0

117 z = LEN(cm$): L = 0: FOR i = 1 TO z
118 L = L + 1
119 IF MID$(cm$, L, 3) = "AND" THEN c = c + 1: c$(c) = LEFT$(cm$, L - 2): G$ = MID$(cm$, L + 4): cm$ = G$: L = 1
120 IF MID$(cm$, L, 4) = "THEN" THEN c = c + 1: c$(c) = LEFT$(cm$, L - 2): G$ = MID$(cm$, L + 5): cm$ = G$: L = 1
121 IF MID$(cm$, L, 1) = "." THEN IF LEN(cm$) <> L THEN c = c + 1: c$(c) = LEFT$(cm$, L - 1): G$ = MID$(cm$, L + 1): cm$ = G$: L = 1
122 IF MID$(cm$, L, 2) = "  " THEN G$ = LEFT$(cm$, L - 1): GG$ = MID$(cm$, L + 3): cm$ = G$ + GG$: GOTO 122
123 IF LEFT$(cm$, 1) = " " THEN cm$ = RIGHT$(cm$, LEN(cm$) - 1)
124 IF RIGHT$(cm$, 1) = "." THEN cm$ = LEFT$(cm$, LEN(cm$) - 1)
125 NEXT: c$(c + 1) = cm$: IF c$(1) <> "" THEN cm$ = c$(1)
126 BF = 0: FOR i = 2 TO 10: c$(i - 1) = c$(i): c$(i) = "": NEXT

200 c = 0: V$ = "": n$ = "": W1$ = "": W2$ = ""
202 c = c + 1: IF c > LEN(cm$) THEN 208
204 w$ = MID$(cm$, c, 1): IF w$ = " " THEN 208
206 V$ = V$ + w$: GOTO 202
208 c = c + 1: IF c > LEN(cm$) THEN 214
210 w$ = MID$(cm$, c, 1): IF w$ = " " THEN 214
212 n$ = n$ + w$: GOTO 208
214 c = c + 1: IF c > LEN(cm$) THEN 220
216 w$ = MID$(cm$, c, 1): IF w$ = " " THEN 220
218 W1$ = W1$ + w$: GOTO 214
220 c = c + 1: IF c > LEN(cm$) THEN 226
222 w$ = MID$(cm$, c, 1): IF w$ = " " THEN 226
224 W2$ = W2$ + w$: GOTO 220
226 IF V$ = "" THEN PRINTit "Come again?": GOTO 109

227 IF LEN(V$) > 5 THEN V$ = LEFT$(V$, 5)
228 IF LEN(n$) > 5 THEN n$ = LEFT$(n$, 5)
229 IF LEN(W1$) > 5 THEN W1$ = LEFT$(W1$, 5)
230 IF LEN(W2$) > 5 THEN W2$ = LEFT$(W2$, 5)
234 IF V$ = "G" OR V$ = "AGAIN" THEN cm$ = VVV$: V$ = V1$: n$ = VV$: W1$ = V2$: W2$ = V3$
235 IF n$ = "IT" THEN n$ = VV$
236 VV$ = n$: VVV$ = cm$: V1$ = V$: V2$ = W1$: V3$ = W2$

240 IF n$ = "SILVE" THEN n$ = "CROSS"
250 IF n$ = "SCRAP" OR n$ = "SHEET" THEN n$ = "PAPER "
255 IF W1$ = "SCRAP" OR W1$ = "SHEET" THEN W1$ = "PAPER"
260 IF n$ = "QUILL" THEN n$ = "PEN"
262 IF W1$ = "QUILL" THEN W1$ = "PEN"
265 IF n$ = "GRAY" THEN n$ = "OOZE"
266 IF n$ = "WOODE" THEN n$ = "CRANK"
267 IF n$ = "WEBS" THEN n$ = "COBWE"
268 IF W2$ = "GRAY" THEN W2$ = "OOZE"
269 IF W1$ = "SILVE" THEN S1$ = "CROSS"

290 MOVE = MOVE + 1: GOSUB 300: GOTO 320
300 a = CSRLIN: VIEW PRINT 1 TO 25:
   LOCATE 25, 1: COLOR 0, 7: PRINT LO$ + SPACE$(40 - LEN(LO$));
305 IF Score > 9 THEN s = 12 ELSE s = 11
309 IF MOVE > 999 THEN
    V = 9
    ELSEIF MOVE > 99 THEN V = 10
    ELSEIF MOVE > 9 THEN V = 11
    ELSE V = 12
    END IF
310 LOCATE 25, 47: PRINT STR$(Score) + SPACE$(s);
315 LOCATE 25, 60: PRINT "Moves:"; MOVE; SPACE$(V - 1); :
    VIEW PRINT 1 TO 24: LOCATE a, 1: COLOR 7, 0: RETURN
320 IF R < 11 AND R <> 7 THEN 340
325 IF OB(4) = -1 OR OB(4) = R THEN 340
327 IF NOT LAMP THEN 330
328 IF OB(1) = -1 OR OB(1) = R THEN 340

330 IF cm$ <> "LIGHT LAMP" AND cm$ <> "TURN ON LAMP" THEN PRINTit "You are in total darkness.": GOSUB 300
FOR temp = 1 TO 11: c$(temp) = "": NEXT: GOTO 109

340 IF cm$ = "TALK INTO WELL" OR cm$ = "TALK TO WELL" OR cm$ = "TALK WELL" THEN IF R = 4 THEN PRINTit "Echo...echo....echo......echo...": GOTO 100
400 IF V$ = "PUT" AND n$ = "ON" THEN V$ = "WEAR": n$ = W1$
402 IF V$ = "GET" AND n$ = "ON" THEN V$ = "WEAR": n$ = W1$
404 IF V$ = "TAKE" AND n$ = "OFF" THEN V$ = "UNWEA": n$ = W1$
405 IF V$ = "GET" AND n$ = "OFF" THEN V$ = "UNWEA": n$ = W1$

407 IF V$ = "THROW" AND n$ = "WATER" THEN V$ = "POUR"
408 IF V$ = "THROW" AND n$ = "BUCKE" THEN V$ = "POUR"
409 IF VAMP AND V$ = "POUR" THEN IF n$ = "WATER" OR n$ = "BUCKE" THEN W1$ = "ON": W2$ = "VAMPI"

410 IF V$ = "TAKE" AND W1$ = "OFF" THEN V$ = "UNWEA"
420 IF V$ = "GET" AND W1$ = "OFF" THEN V$ = "UNWEA"
590 GOTO 2000

600 FOR i = 0 TO NO - 1: IF OB(i) = R THEN PRINTit "You see ": PRINTit (OB$(i)):  ' Print" here."
610 NEXT: RETURN

700 IF R < 11 AND R <> 7 THEN 710
701 IF OB(4) = -1 OR OB(4) = R THEN 710
702 IF NOT LAMP THEN 704
703 IF OB(1) = -1 OR OB(1) = R THEN 710

704 LO$ = "Darkness": GOSUB 300: RETURN

710 IF R = 100 THEN LO$ = "Outside Graveyard": GOSUB 300: GOTO 25400

715 ON R GOSUB 40000, 40060, 40080, 40100, 40140, 40160, 40180, 40200, 40220, 40250, 40270, 40290, 40300, 40310, 40330, 40350, 40360, 40370, 40390, 40410, 40420, 40430, 40440, 40450, 40470, 40490, 40510
720 RETURN

750 IF NOT LAMP THEN 770
760 IF OB(1) = -1 OR OB(1) = R THEN RETURN
770 LO$ = "Darkness": GOSUB 300: PRINTit "Uh-oh, now you can't see!": RETURN

800 FOR temp = 1 TO 11: c$(temp) = "": NEXT: MOVE = MOVE - 1: GOSUB 300: RETURN
810 PRINTit "          ***** You have died *****": GOSUB 25420: GOTO 3440
820 PRINTit "          ***** You have died *****": GOSUB 25420: PRINTit "New game or undo(y/n/u)?"
830 i$ = INKEY$: IF i$ <> "Y" AND i$ <> "y" AND i$ <> "N" AND i$ <> "n" AND i$ <> "U" AND i$ <> "u" THEN 830
840 IF i$ = "Y" OR i$ = "y" THEN RUN ELSE IF i$ = "U" OR i$ = "u" THEN GOSUB 800: GOTO 90
'850 COLOR 7, 0: CLS : KEY ON: END
850 COLOR 7, 0: CLS : KEY ON: SYSTEM

1000 IF NO = 0 THEN RETURN
1010 FOR i = 0 TO NO - 1
1020 IF O2$(i) = n$ THEN FL = 1: RO = OB(i): GOTO 1050
1030 NEXT
1040 FL = 0: RETURN
1050 RETURN






2000 IF V$ <> "GO" AND V$ <> "WALK" AND V$ <> "RUN" AND V$ <> "CRAWL" AND V$ <> "N" AND V$ <> "S" AND V$ <> "CLIMB" AND V$ <> "E" AND V$ <> "W" AND V$ <> "U" AND V$ <> "D" THEN 2500

2010 IF n$ = "NORTH" OR V$ = "N" THEN DI = 0: GOTO 2400
2020 IF n$ = "SOUTH" OR V$ = "S" THEN DI = 1: GOTO 2400
2030 IF n$ = "EAST" OR V$ = "E" THEN DI = 2: GOTO 2400
2040 IF n$ = "WEST" OR V$ = "W" THEN DI = 3: GOTO 2400
2050 IF n$ = "UP" OR n$ = "UPWAR" OR V$ = "U" THEN DI = 4: GOTO 2400
2060 IF n$ = "DOWN" OR n$ = "DOWNW" OR V$ = "D" THEN DI = 5: GOTO 2400

2070 IF R = 1 AND n$ = "COFFI" THEN PRINTit ("Are you crazy?"): GOTO 100
2080 IF R = 3 THEN IF n$ = "STREA" OR n$ = "RIVER" THEN V$ = "S": GOTO 2000
2090 IF R = 6 THEN IF n$ = "STREA" OR n$ = "RIVER" THEN V$ = "N": GOTO 2000

2100 IF R = 9 AND n$ = "HOLE" THEN V$ = "D": GOTO 2000
2110 IF R = 4 AND n$ = "WELL" THEN V$ = "D": GOTO 2000
2120 IF R = 7 AND n$ = "CREVI" THEN V$ = "S": GOTO 2000
2130 IF R = 8 AND n$ = "HOLE" THEN V$ = "D": GOTO 2000

2135 IF R = 7 AND n$ = "ROPE" THEN DI = 4: GOTO 2400
2136 IF R = 4 AND n$ = "ROPE" AND ROPE THEN DI = 5: GOTO 2400
2140 IF R = 27 AND n$ = "HOLE" THEN V$ = "U": GOTO 2000
2145 IF R = 14 AND n$ = "HOLE" THEN V$ = "U": GOTO 2000
2148 IF R = 2 AND n$ = "HOLE" THEN V$ = "E": GOTO 2000
2200 IF R = 3 AND n$ = "HOLE" THEN V$ = "W": GOTO 2000

2390 PRINTit ("You can't go that way!"): GOSUB 800: GOTO 109

2400 IF MA(R, DI) > 0 AND MA(R, DI) < 128 THEN R = MA(R, DI): GOTO 90
2402 IF MA(R, DI) = 203 THEN PRINTit ("The 1 ooze blocks your path."): GOTO 100
2404 IF MA(R, DI) = 204 THEN PRINTit ("The 1 bunny won't let you!"): GOTO 100
2405 IF MA(R, DI) = 200 THEN PRINTit ("As you enter the stream, your mouth gets 3 a little water in it.  Suddenly you    have convulsions, choke, and drown."): GOTO 820

2408 IF MA(R, DI) = 201 AND NOT ROPE THEN 2390
2409 IF MA(R, DI) = 201 AND NOT LAMP THEN PRINTit ("It's too dark down there."): GOTO 100
2410 IF MA(R, DI) = 201 THEN IF OB(1) <> 7 AND OB(1) <> -1 THEN PRINTit ("It's too dark down there."): GOTO 100
2411 IF MA(R, DI) = 240 AND NI > 1 THEN PRINTit ("You're too heavy to climb up the well."): GOTO 100
2412 IF MA(R, DI) = 240 THEN PRINTit ("You climb up the rope."): R = 4: GOTO 90
2413 IF MA(R, DI) = 201 AND NI > 1 THEN PRINTit ("As you attempt to climb down the rope, it snaps, and you plunge down the well."): GOTO 820
2415 IF MA(R, DI) = 201 THEN PRINTit ("You make your way down the rope."): R = 7: GOTO 90

2419 IF MA(R, DI) = 700 THEN PRINTit ("The vampire, unfortunately, gives you a 2 nasty bite."): GOTO 810
2420 IF MA(R, DI) = 314 AND NOT WEAC THEN PRINTit ("The skeleton pushes you away from the hole."): GOTO 100
2430 IF MA(R, DI) = 314 THEN PRINTit ("The skeleton allows you to enter."): R = 14: GOTO 90
2480 GOTO 2390







2500 IF V$ <> "GET" AND V$ <> "TAKE" THEN 2580
2510 ZH = 0: GOSUB 1000: IF n$ = "WATER" THEN IF OB(2) = -1 THEN V$ = "FILL": n$ = "BUCKE": GOTO 2000
2520 IF FL = 0 THEN PRINTit ("You can't get that!"): GOSUB 800: GOTO 109
2530 IF RO = -1 THEN PRINTit ("You already have that!"): GOSUB 800: GOTO 109
2550 IF RO <> R THEN PRINTit ("You can't get that!"): GOSUB 800: GOTO 109
2560 IF NI = 5 THEN PRINTit ("You carrying too much!"): GOSUB 800: GOTO 109

2565 IF NOT CROS AND i = 3 THEN CROS = -1: Score = Score + 10: GOSUB 300
2566 IF NOT CARP AND i = 0 THEN CARP = -1: Score = Score + 2: GOSUB 300
2570 NI = NI + 1: OB(i) = -1: PRINTit ("Taken."): GOTO 100






2580 IF V$ = "HINT" OR V$ = "HELP" THEN PRINTit ("Type 1 n,s,e,w,u,d,help,look,get,read,inven"): GOTO 100
2581 IF V$ = "TURN" AND W1$ = "OFF" THEN V$ = "UNLIG"
2582 IF V$ = "TURN" AND W1$ = "ON" THEN V$ = "LIGHT"
2585 IF V$ = "TURN" AND n$ = "OFF" THEN V$ = "UNLIG": n$ = W1$
2586 IF V$ = "TURN" AND n$ = "ON" THEN V$ = "LIGHT": n$ = W1$




2588 IF V$ = "PUT" AND n$ = "DOWN" THEN V$ = "DROP": n$ = W1$: GOTO 2500
2590 IF V$ = "PICK" AND n$ = "UP" THEN V$ = "TAKE": n$ = W1$: GOTO 2500



2600 IF V$ <> "DROP" THEN 2700
2605 IF W1$ <> "" THEN V$ = "PUT": GOTO 2000
2610 GOSUB 1000
2620 IF FL = 0 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
2635 IF RO = -2 THEN RO = -1
2640 IF RO <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
2650 NI = NI - 1: OB(i) = R: PRINTit ("Dropped."): GOTO 100






2700 IF V$ <> "INVEN" AND V$ <> "I" THEN 2800
2710 FL = 0: PRINTit ("You have:")
2715 IF WEAC THEN PRINTit (" a 2 silver cross (being worn)"): FL = 1
2716 IF BUCK AND OB(2) = -1 THEN PRINTit (" a 1 bucket (with water)"): FL = 1
2717 IF LAMP AND OB(1) = -1 THEN PRINTit (" a 1 lamp (providing light)"): FL = 1
2718 IF OB(4) = -1 THEN PRINTit (" a 1 torch (providing light)"): FL = 1

2720 FOR i = 0 TO NO - 1
2725 IF WEAC AND i = 3 THEN 2740
2726 IF BUCK AND i = 2 AND OB(i) = -1 THEN GOTO 2740
2727 IF LAMP AND i = 1 AND OB(i) = -1 THEN GOTO 2740
2728 IF i = 4 AND OB(i) = -1 THEN 2740
2730 IF OB(i) = -1 THEN PRINTit " ": PRINTit (OB$(i)): FL = 1
2740 NEXT

2745 IF FL = 0 THEN PRINTit (" nothing")
2760 GOTO 100






2800 IF V$ <> "LOOK" AND V$ <> "L" THEN 2900
2810 IF n$ <> "" THEN 2901
2820 GOTO 90

2900 IF V$ <> "EXAMI" AND V$ <> "SEARC" THEN 3400
2901 IF n$ = "FIELD" AND R <> 5 THEN PRINTit ("There isn't a field here!"): GOSUB 800: GOTO 109

2904 IF n$ = "GROUN" OR n$ = "FLOOR" OR n$ = "FIELD" THEN IF R = 5 AND NOT CARS THEN CARS = -1: PRINTit ("You find a carrot."): OB(0) = 5: GOTO 100
2906 IF n$ = "GROUN" OR n$ = "FLOOR" OR n$ = "FIELD" THEN PRINTit ("You see nothing unusual."): GOTO 100

2908 IF n$ = "COFFI" AND R = 1 THEN PRINTit ("It is carved of marble."): GOTO 100
2909 IF n$ = "SKELE" AND R = 9 THEN PRINTit ("It looks pretty bony."): GOTO 100

3000 IF n$ = "WELL" AND R = 4 THEN PRINTit ("It has a pole connected to some rope and a wooden crank.  The well is dark and  empty."): GOTO 100
3001 IF n$ = "ROPE" THEN IF R = 4 OR R = 7 THEN PRINTit ("It doesn't look very sturdy."): GOTO 100
3002 IF n$ = "WELL" AND R = 7 THEN PRINTit ("It is full of cracks."): GOTO 100

3004 IF n$ = "OOZE" AND R = 19 AND NOT DGRA THEN PRINTit ("It is big, ugly, and slimy."): GOTO 100
3005 IF n$ = "COBWE" AND NOT BCOB THEN PRINTit ("The are blocking the east passage."): GOTO 100

3006 IF n$ = "LEVER" AND R = 26 THEN PRINTit ("It is rusty, but useable. "): GOTO 100
3008 IF n$ = "BUNNY" AND NOT DBUN AND R = 25 THEN PRINTit ("It looks extremely vicious for its small size."): GOTO 100
3009 IF n$ = "VAMPI" AND VAMP THEN PRINTit ("It looks at you with an unnerving gaze."): GOTO 100
3010 IF n$ = "TOMBS" OR n$ = "GRAVE" THEN IF R < 11 AND R <> 7 THEN PRINTit ("Many are 3 old and rotted, and some which are blank make you shiver."): GOTO 100

3011 IF n$ = "CRANK" AND R = 4 THEN PRINTit ("It is cracking, but usable."): GOTO 100
3012 IF n$ = "STREA" OR n$ = "RIVER" THEN IF R = 6 OR R = 3 THEN PRINTit ("Though it has 2 no life in it, it looks shallow enough to 1 cross."): GOTO 100
3013 IF n$ = "POLE" AND R = 4 THEN PRINTit ("It has many holes in it."): GOTO 100
3014 IF n$ = "MIRRO" AND R = 24 THEN IF BRMI THEN PRINTit ("It is broken into shards, and there is a 1 passage behind it.") ELSE PRINTit ("It is not well polished.")
3015 IF n$ = "MIRRO" AND R = 24 THEN GOTO 100
3016 IF n$ = "WALLS" OR n$ = "WALL" THEN PRINTit ("There are enough of them."): GOTO 100
3200 GOSUB 1000

3202 IF RO <> R AND RO <> -1 AND OB(i) <> (R - 128) THEN PRINTit ("That's not here!"): GOSUB 800: GOTO 109
3204 IF n$ = "CARRO" THEN PRINTit ("It looks nice and juicy."): GOTO 100
3208 IF n$ = "LAMP" THEN IF LAMP THEN PRINTit ("The lamp lets out a light glow.") ELSE PRINTit ("The 1 lamp has been turned off.")
3210 IF n$ = "LAMP" THEN 100
3215 IF n$ = "BUCKE" THEN IF BUCK THEN PRINTit ("The 1 bucket is filled with water.") ELSE PRINTit ("The bucket is empty.")
3216 IF n$ = "BUCKE" THEN 100

3218 IF n$ = "CROSS" THEN PRINTit ("It is rough and tarnished."): GOTO 100

3300 IF n$ = "PAPER" THEN PRINTit ("It has some writing on it."): GOTO 100
3310 IF n$ = "KEY" THEN PRINTit ("It is fairly shiny."): GOTO 100
3312 IF n$ = "TORCH" THEN PRINTit ("It is burning fairly slowly."): GOTO 100
3314 IF n$ = "PEN" THEN PRINTit ("It does not have any ink."): GOTO 100
3316 IF n$ = "APPLE" THEN PRINTit ("It looks a little too wormy to be edible."): GOTO 100
3318 PRINTit ("You see nothing unusual."): GOTO 100







3400 IF V$ <> "QUIT" AND V$ <> "Q" AND V$ <> "END" THEN 3500
3405 GOSUB 25420
3410 INPUT "Are you sure you want to quit <Y/N>"; QU$
3420 IF QU$ = "N" OR QU$ = "n" THEN GOSUB 800: GOTO 109
3430 IF QU$ <> "Y" AND QU$ <> "y" THEN 3410
3440 PRINTit "Play again <Y/N>?"
3450 i$ = INKEY$: IF i$ <> "Y" AND i$ <> "y" AND i$ <> "N" AND i$ <> "n" THEN 3450
3455 IF i$ = "Y" OR i$ = "y" THEN RUN ELSE COLOR 7, 0: CLS : KEY ON: END






3500 IF V$ <> "ENTER" THEN 3510
3505 V$ = "GO": GOTO 2000


3510 IF V$ <> "JUMP" AND V$ <> "LEAP" THEN 3640
3520 IF n$ = "" THEN PRINTit ("1 Wheeeeee!"): GOTO 100
3530 IF n$ <> "OVER" THEN PRINTit ("That won't do any good."): GOSUB 800: GOTO 109

3535 IF R = 19 AND NOT DGRA AND W1$ = "OOZE" OR W1$ = "GREY" THEN PRINTit ("The ooze expands, forcing you to postpone your jump."): GOSUB 800: GOTO 109
3538 IF R = 25 AND NOT DBUN AND W1$ = "BUNNY" OR W1$ = "RABBI" THEN PRINTit ("The bunny manages to bite your leg."): GOTO 820

3600 IF W1$ <> "STREA" AND W1$ <> "RIVER" THEN PRINTit ("That won't do anything!"): GOTO 100
3610 IF R <> 3 AND R <> 6 THEN PRINTit ("That won't do anything!"): GOTO 100
3620 IF R = 3 THEN R = 6 ELSE R = 3
3630 PRINTit ("You leap over the stream."): GOTO 90








3640 IF V$ <> "READ" THEN 3680
3650 IF n$ <> "PAPER" THEN PRINTit ("You can't do that!"): GOSUB 800: GOTO 109
3660 IF OB(6) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
3670 PRINTit ("It says, `Pushing is better than pulling'."): GOTO 100







3680 IF V$ <> "LIGHT" THEN 3730
3685 IF n$ = "COBWE" THEN 3697
3690 GOSUB 1000: IF FL = 0 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109

3692 IF OB(i) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
3695 IF n$ <> "TORCH" AND n$ <> "COBWE" AND n$ <> "LAMP" THEN PRINTit ("You can't do that!"): GOSUB 800: GOTO 109
3697 IF n$ = "COBWE" AND R = 23 AND BCOB = 0 THEN PRINTit ("The cobwebs light and burn into dust."): Score = Score + 5: GOSUB 300: BCOB = -1: MA(23, 2) = 24: GOTO 100
3699 IF n$ = "COBWE" THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3700 IF n$ = "TORCH" AND OB(4) = -1 THEN PRINTit ("It's already lit!"): GOTO 100
3710 IF n$ = "LAMP" AND OB(1) = -1 AND LAMP THEN PRINTit ("It's already lit!"): GOTO 100

3720 IF n$ = "LAMP" AND NOT LAMP THEN PRINTit ("The lamp lets out a warm glow."): LAMP = -1: IF LO$ = "Darkness" THEN 90 ELSE 100







3730 IF V$ <> "LOWER" THEN 3780
3740 IF n$ <> "ROPE" AND n$ <> "WELL" THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3750 IF R <> 4 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3760 IF ROPE THEN PRINTit ("It already is!"): GOSUB 800: GOTO 109
3770 ROPE = -1: PRINTit ("The rope goes down into the well."): GOTO 100



3780 IF V$ <> "RAISE" AND V$ <> "LIFT" THEN 3830
3790 IF n$ <> "ROPE" AND n$ <> "WELL" THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3800 IF R <> 4 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3810 IF NOT ROPE THEN PRINTit ("It already is!"): GOSUB 800: GOTO 109
3820 ROPE = 0: PRINTit ("The rope rises from the well."): GOTO 100







3830 IF V$ <> "FILL" THEN 3880
3840 IF n$ <> "BUCKE" THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109 ELSE IF OB(2) <> -1 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3850 IF R <> 3 AND R <> 6 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3860 IF BUCK THEN PRINTit ("It already is full!"): GOSUB 800: GOTO 109
3870 PRINTit ("The bucket fills with water."): BUCK = -1: GOTO 100







3880 IF V$ <> "UNLIG" THEN 3940
3890 GOSUB 1000: IF FL = 0 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
3892 IF OB(i) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
3900 IF n$ <> "TORCH" AND n$ <> "LAMP" THEN PRINTit ("You can't do that!"): GOSUB 800: GOTO 109
3910 IF n$ = "TORCH" AND OB(4) = -1 THEN PRINTit ("It refuses to go out."): GOTO 100
3920 IF n$ = "LAMP" AND OB(1) = -1 AND NOT LAMP THEN PRINTit ("It already is!"): GOTO 100
3930 IF n$ = "LAMP" AND OB(1) = -1 THEN PRINTit ("The lamp turns off with a click."): LAMP = 0: GOTO 100






3940 IF V$ <> "OPEN" THEN 4000
3950 IF n$ <> "COFFI" THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3960 IF R <> 1 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
3970 IF OCOF THEN PRINTit ("It's already open!"): GOSUB 800: GOTO 109
3980 IF DVAM THEN PRINTit ("The coffin opens."): OCOF = -1: GOTO 100
3990 OCOF = -1: PRINTit ("As you open the coffin, 2 a vampire reaches out!"): VAMP = -1: MA(1, 1) = 700: GOTO 100






4000 IF V$ <> "CLOSE" THEN 4100
4010 IF n$ <> "COFFI" THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
4020 IF R <> 1 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
4030 IF NOT OCOF THEN PRINTit ("It's not open!"): GOSUB 800: GOTO 109
4032 IF VAMP THEN PRINTit ("The coffin slams shut in the vampire's face."): VAMPT = 0: VAMP = 0: OCOF = 0: MA(1, 1) = 4: GOTO 100
4034 PRINTit ("The coffin closes."): OCOF = 0: GOTO 100






4100 IF V$ <> "POUR" AND V$ <> "EMPTY" AND V$ <> "SPLAS" THEN 4130
4110 IF n$ <> "WATER" AND n$ <> "BUCKE" THEN PRINTit ("You can't pour that!"): GOSUB 800: GOTO 109
4112 IF OB(2) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
4114 IF NOT BUCK THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
4116 IF W2$ = "" THEN BUCK = 0: PRINTit ("You empty the bucket."): GOTO 100

4118 IF W2$ = "OOZE" OR W2$ = "GREY" AND R = 19 AND DGRE = 0 THEN PRINTit ("The ooze dramatically grows, splattering you with a deadly force."): GOTO 820
4120 IF W2$ = "RABBI" OR W2$ = "BUNNY" AND R = 24 AND DBUN = 0 THEN PRINTit ("The bunny moves, avoiding the splash of water."): BUCK = 0: GOTO 100

4122 IF W2$ = "APPLE" THEN IF OB(8) = -1 OR OB(8) = R THEN PRINTit ("The apple is splashed with water."): BUCK = 0: APSO = -1: GOTO 100
4123 IF W2$ = "CARRO" THEN IF OB(0) = -1 OR OB(0) = R THEN PRINTit ("The carrot is splashed with water."): Score = Score + 10: GOSUB 300: BUCK = 0: CASO = -1: GOTO 100

4124 IF W2$ = "SKELE" AND R = 9 THEN PRINTit ("The water flies through the skeltal body."): BUCK = 0: GOTO 100

4125 IF W2$ = "VAMPI" AND VAMP THEN PRINTit ("The vampire screams and gently dissolves in the stream of water."): MA(1, 1) = 4: BUCK = 0: VAMP = 0: OB(3) = 1: GOTO 100

4128 PRINTit ("I'm not so sure about that."): GOSUB 800: GOTO 109







4130 IF V$ <> "WEAR" THEN 4150
4140 IF n$ <> "CROSS" THEN PRINTit ("You can't do that!"): GOSUB 800: GOTO 109
4142 IF OB(3) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
4144 IF WEAC THEN PRINTit ("You are already wearing the cross!"): GOSUB 800: GOTO 109
4146 PRINTit ("It fits rather nice."): WEAC = -1: GOTO 100
4150 IF V$ = "PUT" AND W1$ = "ON" THEN IF n$ = "CROSS" THEN V$ = "WEAR": GOTO 4130
4152 IF V$ = "GET" AND W1$ = "ON" THEN IF n$ = "CROSS" THEN V$ = "WEAR": GOTO 4130
4154 IF V$ = "GET" AND W1$ = "OFF" THEN V$ = "UNWEAR": GOTO 2000
4156 IF V$ = "TAKE" AND W1$ = "OFF" THEN V$ = "UNWEAR": GOTO 2000






4200 IF V$ <> "PUT" THEN 4310
4205 IF W1$ <> "ON" AND W1$ <> "ONTO" THEN 4220

4211 IF n$ = "WATER" OR n$ = "BUCKE" THEN V$ = "POUR": GOTO 2000
4212 IF n$ <> "TORCH" THEN 4217
4213 IF OB(4) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
4215 IF W2$ = "GREY" OR W2$ = "OOZE" THEN IF R = 19 AND DGRE = 0 THEN DGRE = -1: MA(19, 3) = 18: OB(4) = 22: NI = NI - 1: PRINTit ("The ooze dissolves the torch, lighting it instantly.  The ooze melts away into  nothing."): GOSUB 750: GOTO 100

4216 IF W2$ = "COBWE" AND R = 23 AND NOT BCOB THEN PRINTit ("The cobwebs light and burn away into dust."): BCOB = -1: MA(23, 2) = 24: Score = Score + 5: GOSUB 300: GOTO 100
4217 PRINTit ("That probably won't do much."): GOSUB 800: GOTO 109
4220 IF W1$ <> "IN" AND W1$ <> "INTO" THEN 4300
4221 IF n$ = "WATER" AND W2$ = "BUCKE" THEN V$ = "FILL": n$ = "BUCKE": GOTO 2000

4222 IF W2$ = "OOZE" OR W2$ = "GREY" THEN 4215
4224 IF W2$ = "COBWE" THEN 4215
4230 IF W2$ <> "WELL" THEN 4242
4235 IF R <> 4 THEN PRINTit ("That's not here!"): GOSUB 800: GOTO 109
4238 GOSUB 1000: IF FL = 0 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109

4239 IF RO <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
4240 PRINTit ("Enuooooooooauuuuoooooclunk!"): OB(i) = 7: NI = NI - 1: GOTO 100
4242 IF W2$ <> "STREA" AND W2$ <> "WATER" AND W2$ <> "BUCKE" AND W2$ <> "RIVER" THEN 4300
4244 IF W2$ = "BUCKE" THEN IF ((OB(2) <> -1) AND (OB(2) <> R)) THEN PRINTit ("You can't see any bucket here!"): GOSUB 800: GOTO 109
4245 IF W2$ = "BUCKE" AND NOT BUCK AND n$ = "WATER" THEN V$ = "GET": GOTO 2000

4246 IF W2$ = "BUCKE" AND NOT BUCK THEN PRINTit ("Unfortunately, that's not going to be very helpful."): GOSUB 800: GOTO 109
4248 IF W2$ = "WATER" THEN IF R = 3 OR R = 6 THEN 4250
4249 IF W2$ = "WATER" THEN IF OB(2) = -1 OR BUCK THEN 4250 ELSE PRINTit ("That's not here!"): GOSUB 800: GOTO 100
4250 IF W2$ = "STREA" OR W2$ = "RIVER" THEN IF R <> 3 AND R <> 6 THEN PRINTit ("You don't see any!"): GOSUB 800: GOTO 109
4255 GOSUB 1000: IF FL = 0 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
4258 IF OB(i) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109

4260 IF n$ = "CARRO" THEN IF R = 3 OR R = 6 THEN PRINTit ("The carrot soaks up the water."): Score = Score + 10: GOSUB 300: CASO = -1: GOTO 100
4262 IF n$ = "CARRO" AND OB(2) = -1 AND BUCK THEN PRINTit ("The carrot soaks up the water."): Score = Score + 10: GOSUB 300: CASO = -1: GOTO 100
4265 IF n$ = "APPLE" THEN IF R = 3 OR R = 6 THEN PRINTit ("The apple soaks up the water."): APSO = -1: GOTO 100
4268 IF n$ = "APPLE" AND OB(2) = -1 AND BUCK THEN PRINTit ("The apple soaks up the water."): APSO = -1: GOTO 100

4300 PRINTit ("That's not going to do much for this adventure."): GOSUB 800: GOTO 109






4310 IF V$ <> "PUSH" THEN 4350
4315 IF n$ = "COFFI" AND R = 1 THEN PRINTit ("It is much too heavy."): GOTO 100
4317 IF n$ = "MIRRO" AND R = 24 AND NOT BRMI THEN PRINTit ("It is attached to the wall."): GOTO 100
4319 IF n$ = "THRON" AND R = 25 THEN PRINTit ("It is bolted to the wall."): GOTO 100
4320 IF n$ <> "LEVER" OR R <> 26 THEN PRINTit ("You don't need to do that."): GOSUB 800: GOTO 109
4330 IF PLEV THEN PRINTit ("Nothing happens."): GOTO 100
4340 PRINTit ("You hear a loud `clunk' to the south."): MA(8, 5) = 27: PLEV = -1: MA(27, 4) = 8: GOTO 100






4350 IF V$ <> "PULL" THEN 4375
4355 IF n$ <> "LEVER" THEN 4315
4360 IF R <> 26 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109

4370 PRINTit ("The roof caves in on you."): GOTO 820






4375 IF V$ <> "TURN" THEN 4400
4380 IF n$ <> "CRANK" THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
4390 IF R <> 4 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 109
4395 IF ROPE THEN PRINTit ("The rope rises from the well."): ROPE = 0: GOTO 100
4398 PRINTit ("The pole turns, and the rope goes down into the well."): ROPE = -1: GOTO 100






4400 IF V$ = "WAIT" OR V$ = "Z" THEN PRINTit ("Time passes..."): GOTO 100






4410 IF V$ <> "EAT" AND V$ <> "TASTE" THEN 4450
4415 GOSUB 1000: IF FL = 0 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 100
4418 IF OB(i) <> -1 THEN PRINTit ("You can't do that now!"): GOSUB 800: GOTO 100
4420 IF i <> 0 AND i <> 8 THEN PRINTit ("You can't eat that!"): GOSUB 800: GOTO 100

4430 IF i = 8 AND APSO THEN PRINTit ("You take a bite of the apple and feel a strange shiver running through the body.You gasp for breath as you clutch your throut.  You fall down, the apple rolling")
4432 IF i = 8 AND APSO THEN PRINTit ("out of your limp hand."): GOTO 820
4435 IF i = 0 AND CASO THEN PRINTit ("You munch a little out of the carrot and suddenly feel your lungs bursting.  You collapse on the ground, carrot clutched in your cool hand."): GOTO 820
4440 IF i = 8 THEN PRINTit ("You take a bite out of the apple and spit it out."): GOTO 100

4442 PRINTit ("Mmmmmnn...the carrot is delicous.  You gobble the whole thing up."): OB(0) = 0: GOTO 100







4450 IF V$ = "SOAK" AND W1$ = "" THEN V$ = "PUT": W1$ = "IN": W2$ = "WATER": GOTO 2000
4452 IF V$ = "SOAK" THEN V$ = "PUT": GOTO 2000






4460 IF V$ <> "BREAK" AND V$ <> "SMASH" THEN 4490
4465 IF n$ <> "MIRRO" THEN PRINTit ("There's either no point or you don't have the object."): GOSUB 800: GOTO 109
4470 IF R <> 24 THEN PRINTit ("There's either no point or you don't have the object."): GOSUB 800: GOTO 109

4480 IF BRMI THEN PRINTit ("It's already broken!"): GOSUB 800: GOTO 109
4485 BRMI = -1: PRINTit ("The mirror breaks into little pieces, revealing a passage to the east."): Score = Score + 8: GOSUB 300: MA(24, 2) = 25: GOTO 100






4490 IF V$ <> "GIVE" THEN 4520
4492 IF W1$ = "TO" THEN W1$ = n$: n$ = W2$
4495 IF n$ <> "BUNNY" THEN 4510

4498 IF R <> 25 THEN PRINTit ("I don't see a bunny--do you?"): GOSUB 800: GOTO 109
4499 IF DBUN THEN PRINTit ("I don't see a bunny--do you?"): GOSUB 800: GOTO 109
4500 n$ = W1$: GOSUB 1000: IF FL = 0 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109
4502 IF OB(i) <> -1 THEN PRINTit ("You don't have that!"): GOSUB 800: GOTO 109

4505 IF n$ <> "CARRO" THEN PRINTit ("The bunny refuses your offer."): GOSUB 800: GOTO 109
4507 IF NOT CASO THEN PRINTit ("The bunny gobbles up the carrot but is not satisfied."): OB(0) = 0: GOTO 100
4509 IF CASO THEN PRINTit ("The bunny gobbles the carrot up, makes a startled gasp, and dies.  The bunny    disappears into a black fog."): Score = Score + 10: GOSUB 300: DBUN = -1: GOTO 100

4510 PRINTit ("I'm not sure it will accept your offer."): GOSUB 800: GOTO 109






4520 IF V$ <> "SIT" THEN 4540
4530 IF n$ = "ON" THEN n$ = W1$
4532 IF n$ <> "THRON" THEN PRINTit ("That probably won't work."): GOSUB 800: GOTO 109
4534 IF R <> 25 THEN PRINTit ("That probably won't work."): GOSUB 800: GOTO 109

4535 IF STHR THEN PRINTit ("You sit on the throne and you get blown away by a....hold it....that was another adventure.  Nothing happens so you stand up again."): GOTO 100
4538 PRINTit ("As you sit on the throne, a passage opens to the east!  Since the throne is     uncomfortable, you stand up again."): Score = Score + 2: STHR = -1: GOSUB 300: MA(25, 2) = 100: GOTO 100







4540 IF V$ = "BURN" THEN V$ = "LIGHT": GOTO 2000



4542 IF V$ = "NORTH" THEN V$ = "N": GOTO 2000
4544 IF V$ = "SOUTH" THEN V$ = "S": GOTO 2000
4546 IF V$ = "EAST" THEN V$ = "E": GOTO 2000
4547 IF V$ = "WEST" THEN V$ = "W": GOTO 2000
4548 IF V$ = "UP" THEN V$ = "U": GOTO 2000
4549 IF V$ = "DOWN" THEN V$ = "D": GOTO 2000





4550 IF V$ <> "UNWEA" THEN 4565
4555 IF n$ <> "CROSS" THEN PRINTit ("That doesn't make sense!"): GOSUB 800: GOTO 109
4556 IF NOT WEAC THEN PRINTit ("You're not wearing it!"): GOSUB 800: GOTO 109
4560 PRINTit ("You slide the cross off your neck."): WEAC = 0: GOTO 100





4565 IF V$ = "DESTR" AND n$ = "MIRRO" THEN V$ = "BREAK": GOTO 2000
4570 IF V$ <> "DESTR" AND V$ <> "KILL" AND V$ <> "ATTAC" THEN 4590
4575 IF n$ = "SKELE" AND R = 9 THEN PRINTit ("The skeleton repels your attack."): GOTO 100
4580 IF n$ = "VAMPI" AND VAMP THEN PRINTit ("The vampire ignores your attack."): GOTO 100
4585 IF n$ = "OOZE" AND NOT DGRE AND R = 19 THEN PRINTit ("The ooze lets out a splatter with killing force."): GOTO 820
4588 IF n$ = "BUNNY" AND NOT DBUN AND R = 25 THEN PRINTit ("The bunny bites you in defense."): GOTO 820
4589 PRINTit ("That's not important to this adventure."): GOSUB 800: GOTO 109







4590 IF V$ = "TALK" THEN PRINTit ("That won't do much."): GOSUB 800: GOTO 109





4595 IF V$ <> "DRINK" AND V$ <> "SIP" THEN 4620
4600 IF n$ <> "WATER" THEN PRINTit ("That won't do much."): GOSUB 800: GOTO 109
4605 IF R = 3 OR R = 6 THEN 4615
4608 IF OB(2) = -1 OR OB(2) = R THEN IF BUCK THEN 4615

4610 PRINTit ("That won't do much."): GOSUB 800: GOTO 109
4615 PRINTit ("You sip the water, and feel very sick.  You lungs turn inside out, your breath  stops, and you fall on the floor, dead."): GOTO 820
4620 IF V$ = "SCORE" THEN GOSUB 25420: GOSUB 800: GOTO 108

10000 PRINTit ("Sorry, but I don't know that verb."): GOSUB 800: GOTO 109








25000 IF NR = 0 THEN RETURN

25030 FOR i = 1 TO NR
25040 FOR j = 0 TO ND - 1
25050 READ MA(i, j): NEXT: NEXT: RETURN
                        '              n s e w u d
25100 DATA 0,4,0,0,0,0: 'ROOM 1          s
25110 DATA 0,0,3,0,0,0: 'ROOM 2            e
25120 DATA 0,200,0,2,0,0: 'ROOM 3        x   w
25130 DATA 1,8,5,0,0,201: 'ROOM 4      n s e     x
25140 DATA 0,8,6,4,0,0: 'ROOM 5          s e w
25150 DATA 200,9,10,5,0,0: 'ROOM 6     x s e w
25160 DATA 0,11,0,0,240,0: 'ROOM 7      s      x
25170 DATA 5,0,9,4,0,0: 'ROOM 8        n
25180 DATA 6,0,10,8,0,314: 'ROOM 9     n  e w  x
25190 DATA 6,0,0,9,0,0: 'ROOM 10       n    w
25200 DATA 7,0,0,0,0,0: 'ROOM 11       n
25210 DATA 0,0,0,0,0,0: 'ROOM 12
25220 DATA 16,12,17,17,0,0: 'ROOM 13   n s e w
25230 DATA 0,0,15,0,9,0: 'ROOM 14          e   u
25240 DATA 15,19,15,14,0,0: 'ROOM 15   n
25250 DATA 13,22,20,21,0,0: 'ROOM 16   n
25260 DATA 13,20,16,16,0,0: 'ROOM 17   n
25270 DATA 0,0,19,17,0,0: 'ROOM 18
25280 DATA 15,0,0,203,0,0: 'ROOM 19    n
25290 DATA 13,17,17,16,0,0: 'ROOM 20   n
25300 DATA 16,23,13,20,0,0: 'ROOM 21   n
25310 DATA 17,21,20,16,0,0: 'ROOM 22   n s e w
25320 DATA 21,26,0,0,0,0: 'ROOM 23     n s
25330 DATA 0,0,0,23,0,0: 'ROOM 24          e
25340 DATA 0,0,0,24,0,0: 'ROOM 25          e
25350 DATA 23,27,0,0,0,0: 'ROOM 26     n s
25360 DATA 26,0,0,0,0,0: 'ROOM 27      n

25400 PRINTit ("You go through the 1 tunnel and end up outside the graveyard.  When you realize   where you are, you hurry along the path back into 1 town, overcome with joy,")
25405 PRINTit ("shouting the news.  People rush out of their houses to greet you with a 2 hero's  welcome.")
25410 GOSUB 25420: GOTO 3440
25420 ScoreString$ = "#" + RIGHT$(STR$(Score), LEN(STR$(Score)) - 1): 'PRINT ScoreString$: STOP
25421 PRINTit "You have scored " + ScoreString$ + " out of #60 points."
25422 IF Score < 11 THEN RANK$ = "2 Novice Adventurer": GOTO 25490
25430 IF Score < 21 THEN RANK$ = "2 Graveyard Explorer": GOTO 25490
25440 IF Score < 31 THEN RANK$ = "1 Adventurer": GOTO 25490
25450 IF Score < 41 THEN RANK$ = "2 Monster Basher": GOTO 25490
25460 IF Score < 51 THEN RANK$ = "2 Expert Adventurer": GOTO 25490
25470 IF Score < 60 THEN RANK$ = "1 Exterminator": GOTO 25490
25480 RANK$ = "1 Bunny Slayer"
25490 PRINTit "You have reached the rank of " + RANK$: RETURN

26000 IF NO = 0 THEN RETURN
26010 FOR i = 0 TO NO - 1
26020 READ OB$(i), O2$(i), OB(i): NEXT: RETURN

26100 DATA "a 1 carrot",CARRO,0
26110 DATA "a 1 lamp",LAMP,2
26120 DATA "a 1 bucket",BUCKE,11
26130 DATA "a 2 silver cross",CROSS,0
26140 DATA "a 1 torch",TORCH,14
26150 DATA "a 1 key",KEY,18
26160 DATA "a 3 scrap of paper",PAPER,8
26170 DATA "a 2 quill pen",PEN,-1
26180 DATA "an 1 apple",APPLE,-1

27000 CC$ = CHR$(34)


27051 PRINTit ("")
27052 CenterIt ("The 1 Night of the 2 Vampire Bunnies")
27053 PRINTit ("")
27054 PRINTit ("")
27055 CenterIt ("By 2 Jason Dyer")
27056 PRINTit ("")
27057 PRINTit ("")

27010 CALL PRINTit("    The 1 voice still echoes in your head. Remember--if the bunny is not dead by midnight, the countryside will be ruled by 1 evil. ")
27020 PRINTit ("    You shiver as you think of the people dead in the street with bite marks on their neck.  The cattle dead by the hundreds.  The stories of a rampaging")
27030 PRINTit ("2 vampire bunny.  A chill runs up your spine.")
27040 PRINTit ("    And lastly, you remember the strangeness of the 1 graveyard the last time you saw it.  The 2 misty fog surrounding it.  And now you're in the 1 graveyard, alone,")
27050 PRINTit ("armed only with your 1 apple and your 2 trusty quill pen.")
27061 PRINTit ("")
27062 PRINTit ("")
27063 PRINTit ("")
27064 PRINTit ("")
27069 CenterIt ("Press any key to continue")




'27064 LOCATE 10, 24: PrintIt ("The 1 Night of the 2 Vampire Bunnies")
'27066 LOCATE 14, 31: PrintIt ("Made by 2 Jason Dyer")
'27068 LOCATE 23, 28: PrintIt ("Press any key to continue")
27070 i$ = INKEY$: IF i$ = "" THEN 27070
27080 CLS : RETURN







40000 LO$ = "At 1 Coffin (1)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40010 PRINTit ("You are at a 1 coffin in a graveyard, with walls to the north, east, and west.  A"): MES$ = "blue 1 mist hangs over the graveyard.  "
40015 IF VAMP THEN PRINTit (MES$ + "The 1 coffin is open with a vampire in it!"): RETURN
40020 IF OCOF THEN PRINTit (MES$ + "The 1 coffin is open.") ELSE PRINTit (MES$ + "The coffin is closed.")
40030 IF NOT OCOF THEN RETURN
40040 FOR i = 0 TO 8: IF OB(i) = .1 THEN PRINTit "There is " + OB$(i) + " in the coffin."
40050 NEXT: RETURN







40060 LO$ = "1 Graveyard (2)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40070 PRINTit ("You are in a graveyard surrounded by walls.  There is fog here so dense you can barely see two meters in front of you.  A 1 hole in the east wall allows exit from  this gloomy place."): RETURN

40080 LO$ = "1 Graveyard (3)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$): IF NOT JRI THEN JRI = -1: Score = Score + 3: GOSUB 300
40090 PRINTit ("You are in a graveyard with a 1 stream to the south.  Walls surround the other    directions, but a 1 hole in the west wall allows passage.  It is very cool here."): RETURN

40100 LO$ = "1 Graveyard (4)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40110 PRINTit ("You are in a 2 dark graveyard.  A wall covered with moss is to the west, 2 a coffin is in sight to the north, and an 2 empty well lies in the middle of all the fog."): IF ROPE THEN PRINTit ("A 1 rope leads down the damp well.")
40120 RETURN







40140 LO$ = "2 Carrot Field (5)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40150 PRINTit ("You are in a 2 carrot field in the middle of a graveyard.  Fog is all around you. A 1 wall is to the north, too high to climb."): RETURN

40160 LO$ = "1 Graveyard (6)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40170 PRINTit ("You are in a 2 foggy graveyard.  A 1 Stream is to the 1 north, a 1 field can be seen to the 1 west, and all the rest only fog can be seen.  The 1 stream looks shallow      enough to cross."): RETURN






40180 LO$ = "3 Bottom of Well (7)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$): IF NOT WELL THEN WELL = -1: Score = Score + 5: GOSUB 300
40190 PRINTit ("You are at the bottom of an empty well.  The surrounding cracks are rather big, and a 1 crevice to the south is big enough for you to go through.  A 1 rope hangs   down allowing you to climb up."): RETURN

40200 LO$ = "1 Graveyard (8)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40205 IF PLEV THEN PRINTit ("You are in a 2 walled graveyard.  A black wall is to the south.  A 2 gaping hole is in the middle of all the gravestones."): RETURN
40210 PRINTit ("You are in a 2 walled graveyard.  A black wall is to the south.  Many tombstones  are in this area--so many, in fact, you have to climb over some of them."): RETURN







40220 LO$ = "At 1 Hole (9)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40230 PRINTit ("You are in a 2 gloomy graveyard, with a wall to the south, and plenty of fog all  around you.  A large hole surrounded by gravestones leads down into gloom.")
40240 IF WEAC THEN PRINTit ("A 1 skeleton that was guarding the hole faces toward you, but when he sees the    cross you are wearing, recoils in terror."): RETURN
40245 PRINTit ("A 1 skeleton that was guarding the hole faces towards you, and holds his stand,   not allowing you to you into the hole."): RETURN

40250 LO$ = "1 Graveyard (10)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40260 PRINTit ("You are at the 1 corner of a walled graveyard.  Walls connect to the south and    east, and mist hangs over the sky like a prowler.  A chill runs through your    back."): RETURN






40270 LO$ = "1 Crevice (11)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40280 PRINTit ("You are in a large crevice in a well.  Small amounts of water trickle down the  sides.  Exit can be reached to the north."): RETURN

40290 LO$ = "1 Maze (12)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit ("You have a nervousness about this 1 passage, but you trudge on, and end up getting killed in a cave-in."): R = 13: FFF = -1: RETURN

40300 LO$ = "1 Maze (13)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit ("You are in a 2 dark maze."): RETURN

40310 LO$ = "2 Dark Tunnel (14)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40320 PRINTit ("You are in a very dark 1 tunnel.  The tunnel heads to the east, while a 1 hole in   the ceiling allows exit from the darkness."): RETURN

40330 LO$ = "2 Dark Tunnel (15)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40340 PRINTit ("You are in a tunnel of extreme darkness.  You can barely make out exits to all  directions except 1 up and 1 down."): RETURN

40350 LO$ = "1 Maze (16)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit ("You are in a damp maze."): RETURN

40360 LO$ = "1 Maze (17)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit ("You are in a dank maze."): RETURN






40370 LO$ = "2 Long Corridor (18)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40375 IF NOT SCORELONG THEN SCORELONG = -1: Score = Score + 5: GOSUB 300
40380 PRINTit ("You are in a very 2 long tunnel streaching endlessly to the east and west.  Your  feet ache as you walk down it."): RETURN

40390 LO$ = "2 Slimy Room (19)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40400 PRINTit ("You are in a 2 medium-size room covered with slime from ceiling to floor.  Your   feet stick to the slippery ground."): IF NOT DGRE THEN PRINTit ("Some grey ooze guards the westward corridor.")
40405 RETURN

40410 LO$ = "Maze (20)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit ("You are in a dreary maze."): RETURN

40420 LO$ = "Maze (21)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit ("You are in a maze."): RETURN

40430 LO$ = "Maze (22)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit ("You are in a devlish maze."): RETURN






40440 LO$ = "2 Torch Room (23)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40445 PRINTit ("You are in a 3 large marble room with torchholders everywhere.  However, pretty"): IF BCOB THEN PRINTit ("much all of them are empty.  You can leave to the north, south, or east."): RETURN
40447 PRINTit ("much all of them are empty.  You can leave to then south and north, but the eastpassage is blocked by 1 cobwebs."): RETURN






40450 LO$ = "2 Dead End (24)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40460 IF BRMI THEN PRINTit ("You are in a seeminly dead end, with the only exit to the west, but a 2 shattered mirror shows a passage to the east."): RETURN
40465 PRINTit ("You are in a dead end, with a large mirror covering the east wall, and an exit  to the west."): RETURN






40470 LO$ = "2 Bunny Room (25)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40472 IF NOT DBUN THEN PRINTit ("You are in a large chamber that is sparsely furnished except for a 1 throne bolted to the east wall.  Sitting next to the throne is a bunny which looks very much")
40475 IF NOT DBUN THEN PRINTit ("like the vampire bunny you are looking for.  A passage exits west from this veryunfriendly place."): RETURN
40480 IF NOT STHR THEN PRINTit ("You are in a 2 large chamber that is sparsely furnished except for a throne bolted to the east wall.  A passage exits west from this barren place."): RETURN
40485 PRINTit ("You are in a 2 large chamber that is sparsely furnished except for a throne boltedto the east wall.  Two passages allow exit -- one to the east, one to the west."): RETURN






40490 LO$ = "2 Cramped Corridor (26)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40500 PRINTit ("You are in a very 2 cramped corridor going north/south.  A 1 lever is set into the  west wall.  The lever seems to be pretty worn from use."): RETURN






40510 LO$ = "2 Sandy Room (27)": FOR j = 0 TO 5: PRINT MA(R, j); : NEXT: PRINTit (LO$)
40520 IF NOT PLEV THEN PRINTit ("You are in a extremely sandy room with the only exit going north.  You feel a   little nervous that the ceiling will cave in on you."): RETURN
40530 PRINTit ("You are in a very 2 sandy room with an exit to the north and a 1 hole in the roof.  You feel a little nervous that the ceiling will cave in on you."): RETURN

REM *****************************************************************
DATA
DATA help
DATA
DATA look
DATA
DATA inventory
DATA
DATA look wall
DATA
DATA east
DATA look stream
DATA west
DATA w
DATA north
DATA look coffin
DATA open coffin
DATA close coffin
DATA south
DATA e
DATA look field
DATA get carrot
DATA e
DATA s
DATA w
DATA get paper
DATA read paper
DATA w
DATA n
DATA open coffin
DATA close coffin
DATA s
DATA e
DATA e
DATA go stream
DATA jump over stream
DATA go hole
DATA GET lamp
DATA e
DATA jump over stream
DATA W
DATA W
DATA LOOK WELL
DATA TURN CRANK
DATA down
DATA light lamp
DATA down
DATA inventory
DATA DROP PEN
DATA DROP APPLE
DATA DROP CARROT
DATA DROP PAPER
DATA D
DATA S
DATA GET BUCKET
DATA N
DATA U
DATA

DATA DROP LAMP
DATA U
DATA DROP BUCKET
DATA D
DATA GET LAMP
DATA U
DATA DROP LAMP
DATA GET BUCKET
DATA E
DATA E
DATA GET WATER
DATA W
DATA W
DATA GET CARROT
DATA SOAK CARROT
DATA n
DATA open coffin
DATA pour water
DATA look coffin
DATA look
DATA GET cross
DATA s
DATA get lamp
DATA e
DATA e
DATA s
DATA wear cross
DATA look
DATA d
DATA GET torch
DATA e
DATA s
DATA inven
DATA PUT torch on ooze
DATA look
DATA w
DATA get key
DATA e
DATA w
DATA w
DATA n
DATA s

DATA \
9999 '
DATA EndOfData

FUNCTION ButFirst$ (sentence$)
REM returns the remainder of the sentence with
REM first word (and trailing space) removed    03/1/95

temp = INSTR(sentence$, " ")
IF temp = 0 THEN ButFirst$ = "": EXIT FUNCTION
ButFirst$ = RIGHT$(sentence$, LEN(sentence$) - temp)

END FUNCTION

SUB CategoryColor (Sent$)

IF MID$(Sent$, 2, 1) = "C" THEN
        COLOR 11: REM Turq
        IF FileName$ = "LPT1:" THEN
                LPRINT CHR$(27); CHR$(114); CHR$(2); :  REM Blue
        END IF
END IF

IF MID$(Sent$, 2, 1) = "M" THEN
        COLOR 9:
        IF FileName$ = "LPT1:" THEN
          LPRINT CHR$(27); CHR$(114); CHR$(2); : REM Blue
        END IF
END IF
IF MID$(Sent$, 2, 1) = "P" THEN
        COLOR 13:
        IF FileName$ = "LPT1:" THEN
          LPRINT CHR$(27); CHR$(114); CHR$(3); : REM Purple
        END IF
END IF

REM IF MID$(Sent$, 2, 1) = "P" THEN COLOR 12:LPRINT CHR$(27); CHR$(114); CHR$(1);: REM Red
IF MID$(Sent$, 2, 1) = "O" THEN
        COLOR 10:
        IF FileName$ = "LPT1:" THEN
          LPRINT CHR$(27); CHR$(114); CHR$(6); : REM Green
        END IF
END IF
IF MID$(Sent$, 2, 1) = "A" THEN
        COLOR 11:
        IF FileName$ = "LPT1:" THEN
          LPRINT CHR$(27); CHR$(114); CHR$(2); : REM Turqoise
        END IF
END IF
REM IF MID$(Sent$, 2, 1) = "P" THEN COLOR 13: REM Purple
REM IF MID$(Sent$, 2, 1) = "O" THEN COLOR 15: REM Bright white
REM IF MID$(Sent$, 2, 1) = "C" THEN COLOR 14: REM Yellow
REM IF MID$(Sent$, 2, 1) = "M" THEN COLOR 9
REM IF MID$(Sent$, 2, 1) = "P" THEN COLOR 13: REM Purple
REM IF MID$(Sent$, 2, 1) = "O" THEN COLOR 12


Sent$ = RIGHT$(Sent$, LEN(Sent$) - 1)


END SUB

FUNCTION Cent$ (Sent$)
REM Adds Centering Spaces to indent line

IndentCount = INT((80 - LEN(Sent$)) / 2)
Cent$ = STRING$(IndentCount, " ") + Sent$

END FUNCTION

SUB CenterIt (Sent$)
REM CALL ColorIt1

IndentCount = INT((80 - LEN(Sent$)) / 2)
PRINTit STRING$(IndentCount, " ") + Sent$

END SUB

SUB ColorIt1
RANDOMIZE TIMER:
temp = INT(RND * 15) + 1
IF temp = 1 THEN temp = 9
IF temp = 2 THEN temp = 10
IF temp = 3 THEN temp = 11
IF temp = 4 THEN temp = 12
IF temp = 5 THEN temp = 13
IF temp = 6 THEN temp = 14
IF temp = 7 THEN temp = 15
IF temp = 8 THEN temp = 15

COLOR temp

END SUB

SUB DisplayDirectory
REM Show Current Directory Contents on the Screen
REM Wait until the user presses a key

CLS


SHELL "dir"

END SUB

SUB DisplayMenu
REM Displays Some Command Instructions

CLS : LOCATE 5, 1

CALL Printit2(Cent("Menu of 2 Todays Commands"), 0)
PRINT

COLOR 7
Mspace = 13

CALL Printit2(SPACE$(Mspace) + "Press 1 <ENTER> for Newsletter Screen Display", 0)
PRINT
CALL Printit2(SPACE$(Mspace) + "Press 1 <F>  for additional 3 Copy to a File", 0)
CALL Printit2(SPACE$(Mspace) + "Press 1 <D>  for Display of 2 Current Directory", 0)
CALL Printit2(SPACE$(Mspace) + "Press 1 <W>  to Write your 4 Comments to a File", 0)
PRINT
CALL Printit2(SPACE$(Mspace) + "Press 1 <M>  to Display this 1 Menu", 0)
CALL Printit2(SPACE$(Mspace) + "Press 1 <H>  to Display 2 Todays Headlines", 0)


REM PRINT "   Press   <L>  for additional Copy to Line Printer "

END SUB

SUB DribbleIt
REM Copy (Dribble) all output to screen and to a file

NoExtra = 0: REM Yes, now there is extra

PRINT : COLOR 6
PRINT "File Name  (less that 8 Characters, no spaces, no periods) "
LINE INPUT "or press  <ENTER> to use STD_OUT.BAS ", FileName$
COLOR 7
IF FileName$ = "" THEN FileName$ = "STD_OUT"
FileName$ = FileName$ + ".BAS"

IF NoExtra = 0 THEN OPEN FileName$ FOR OUTPUT AS #1

END SUB

FUNCTION First$ (Sent$)
REM Does not include error checking

temp = INSTR(Sent$, " ")
IF temp = 0 THEN First$ = Sent$: EXIT FUNCTION
First$ = LEFT$(Sent$, temp - 1)

END FUNCTION

SUB HeadlineStuff

CLS

LOCATE 24, 1: COLOR 15
CALL Printit2(Cent("  Press  1 <ENTER>  to Continue"), 0)


LOCATE 4, 1
REM COLOR 14
REM   CALL CenterIt("A.I. NewsLetter Display Program")
REM CALL TodaysHeadlines
              
              
               COLOR 11
               PRINTit "     A.I. Educator"
               PRINTit "     2340 Whitney Avenue"
               PRINTit "     Hamden, CT  06518"
               PRINTit "     (203) 248-7764"
               PRINT : PRINT : PRINT : PRINT : PRINT : COLOR 26
               PRINT "                                       You": COLOR 14
               PRINT "                                       Your Address"
               PRINT "                                       Your City"



CALL sparkle





END SUB

SUB HMSStuff
DIM HMS$(20)

REM Label8:
HMS$(0) = "1 Marty Section"
HMS$(1) = ""
HMS$(2) = "Run 2 entire PTD"
HMS$(3) = "  Clinical Notes"
HMS$(4) = "  Superbills"
HMS$(5) = "  1 Clean Up"
HMS$(6) = "  Hands on Therapy"
HMS$(7) = ""
HMS$(8) = "2 Insurance forms"
HMS$(9) = "  Blue Cross"
HMS$(10) = ""
HMS$(11) = "1 Atty Calls"
HMS$(12) = "  faxing, mailing"
HMS$(13) = "1 Appeals Calls"
HMS$(14) = "3 Follow up calls - daily"
HMS$(15) = "Released Patients Follow up calls"
HMS$(16) = ""





END SUB

SUB junk


END SUB

SUB NewsInitScreen
REM Set up the View Ports, Needs Work to separate functions

VIEW PRINT
CLS

COLOR 10
LOCATE 11, 1
CALL CenterIt("Welcome to Today   " + DATE$)

LOCATE 1, 1: REM CALL ColorIt1
COLOR 3
CALL CenterIt("A.I. NewsLetter Display Program")

LOCATE 24, 1: COLOR 15
CALL Printit2(Cent("2 Shift <Q> to Quit, 1 <H> for Headlines, 1 <M> for Menu, 1 <ENTER> to continue"), 0)
REM CALL CenterIt1("Type Help for more instructions")

VIEW PRINT 3 TO 22

END SUB

SUB OldThuNotes



REM Labela:
REM Thu "1 Neal
REM Thu "
REM Thu "This is a 3 current working version of 1 News1
REM Thu "It has a 2 directory display subroutine and 2 Writing Powers
REM Thu "
REM Thu "This version still uses the 3 numbers for color
REM Thu "and it has the 5 second letter for category display
REM Thu "
REM Thu "
REM Thu "I had to 4 remove the existing newsletter since I had
REM Thu "filled up the main module part of basic (hooray we have limits)
REM Thu "
REM Thu "
REM Thu "You have to play around to 5 load the new writing stuff
REM Thu "back to basic, but it is easy
REM Thu "
REM Thu "2 save original program
REM Thu "2 load new writing file
REM Thu "edit it
REM Thu "1 copy it to clipboard
REM Thu "reload original program
REM Thu "1 paste in new material in proper place in original program
REM Thu "run new program
REM Thu "1 save new program
REM Thu "
REM Thu "
REM Thu "These are 2 first drafts, they barely work
REM Thu "There is lots of room for improvement
REM Thu "
REM Thu "We can now easily make them 3 more user friendly
REM Thu "and more 2 pedagogically sound
REM Thu "
REM Thu "
REM Thu "3 all for now
REM Thu "
REM Thu "Bob
REM Thu "
REM Thu "



REM labelb:
REM Thu "2 Tues. Notes
REM Thu "
REM Thu "I want people to have their 3 own file area
REM Thu "and their 3 own comment files to work with.
REM Thu "I will help them add material
REM Thu "to the basic program later at first,
REM Thu "but they can certainly review
REM Thu "it in whatever word processor they have.
REM Thu "these are 2 individual responses to the current subjects
REM Thu "
REM Thu "
REM Thu "How can we have the 5 multiple versions of the program from the
REM Thu "multiple programmers be kept in some sort of order.
REM Thu "We have the 4 Microsoft Daily build program
REM Thu "where everyones work has to 4 work with other peoples.
REM Thu "We can assign things to different people
REM Thu "and 4 keep things in subroutines and see if that will solve it.
REM Thu "
REM Thu "
REM Thu "there is a nice feature here
REM Thu "where these nice 4 long five part paragraphs can be
REM Thu "5 broken down into their phrases and rethought
REM Thu "about so that the many complex ideas
REM Thu "in the paragraphs can be
REM Thu "rethought and corrected.
REM Thu "for a while you only have to read
REM Thu "the full paragraphs
REM Thu "to look for 2 complete thoughts
REM Thu "
REM Thu "
REM Thu "A 2 colored phrase or two should also tell you
REM Thu "that the paragraph
REM Thu "is in some sort of final form.
REM Thu "
REM Thu "
REM Thu "We will need 1 tutorials for all
REM Thu "of this stuff
REM Thu "People will have to have all the pain of seeing
REM Thu "multiple versions happen,
REM Thu "but the 5 main programs should stabilize soon
REM Thu "and will be useful for all the AI work to come
REM Thu "
REM Thu "
REM Labelc:
REM Thu "This is now working
REM Thu "and I think that the 4 conversion to the subroutine was helpful.
REM Thu "it clarified some of the logic,
REM Thu "and made the code less sloppy.
REM Thu "I am content now to let people
REM Thu "3 add subroutine intelligence
REM Thu "and preserve for now
REM Thu "the 3 control code sequence.
REM Thu "The 2 control code can be dealt with separately later as well.
REM Thu "
REM Thu "
REM Thu "We are now all 4 working on a word (or here a letter) to add some
REM Thu "intelligence and make things better.
REM Thu "A lot of the initial work
REM Thu "will be just 6 making the qbasic world more comfortable and powerful
REM Thu "We have to live with no undo in the edit mode and that will
REM Thu "aggravate everyone at some time.
REM Thu "Lets celebrate the things we have
REM Thu "
REM Thu "
REM Thu "INPUT area (Preliminary notes)
REM Thu "
REM Thu "I want a 4 view port for input and I want the input
REM Thu "to be added to a file
REM Thu "I think that I will have to have the writing to a file
REM Thu "and that will be OK
REM Thu "I will have multiple versions of the file (for a while, rhyme)
REM Thu "
REM Thu "
REM Thu "Once I have files,
REM Thu "I will have the 6 can not find the folder problem
REM Thu "and it will be hard to distribute,
REM Thu "because of the extra stuff
REM Thu "
REM Thu "



REM labeld:
REM Thu "I can have all developing versions
REM Thu "right in the qbasic folder
REM Thu "and move versions around
REM Thu "(up into working area,
REM Thu "then back into
REM Thu "a folder for later use.
REM Thu "I guess I will try that
REM Thu "
REM Thu "
REM Thu "I also think that I will work with 2 small files so I want to test the
REM Thu "dir. command from Q basic    shell dir
REM Thu "
REM Thu "
REM Thu "this is a test of the 3 new Writing system
REM Thu "It seems to work already for the five line entry
REM Thu "and we talk about end bug and the lprint bug
REM Thu "and we can then load the material and edit it right in basic
REM Thu "and then add it back to the program as it stands
REM Thu "
REM Thu "
REM Thu "And we are talking about the 2 write area
REM Thu "and it is working right now with the small (one line screens)
REM Thu "I will be able to sort of write in paragraphs
REM Thu "and I will use the people, program and progress format
REM Thu "or the problem, procedure and plan format.
REM Thu "
REM Thu "
REM Thu "I can write about the 2 cave stuff even to the wrong screen
REM Thu "I hope to add rems to the Aztec game so we can progress on that
REM Thu "we will work together to solving it and that is important
REM Thu "and we can show each other how smart we are, by the content
REM Thu "of our comments
REM Thu "
REM Thu "



REM Labele:
REM Thu "the program can keep the 3 lines reasonably short so that the
REM Thu "wrap around problem will not be too big.  And it can
REM Thu "add apostrophes with each line so that that problem is over
REM Thu "and we can end on two blank lines so that problem is over
REM Thu "as well.  then we can merge these lines right back to basic
REM Thu "
REM Thu "
REM Thu "I think that I can try to print the 2 heading line that is
REM Thu "on the screen.  It should be available as the A$ from the data
REM Thu "so we will have the headers for each paragraph.  Then editing
REM Thu "them right on the screen will allow things to be expanded and
REM Thu "improved.  Perhaps I can always have a header line in the array
REM Thu "
REM Thu "
REM Thu "And I can 4 type here a lot
REM Thu "and the carriage return is working I hope
REM Thu "and the screen will scroll as I type
REM Thu "so we loose the top line
REM Thu "but I think that I can type a lot now
REM Thu "
REM Thu "
REM Thu "I have the many 2 full paragraphs since we are in a loop
REM Thu "and I can have the full screen display for these notes as we
REM Thu "the backspace does not work over a line of course
REM Thu "but I can type numbers in the lines if I want for later coloring
REM Thu "
REM Thu "
REM Thu "and I can now 5 load this thing to basic since it sits there in
REM Thu "the proper directory.  I think that it will be perhaps the best
REM Thu "way to write letters and to make one paragraph additions to the
REM Thu "program and the discussions that we are doing.  I think that I
REM Thu "want the header line up one, or at least in a different color
REM Thu "
REM Thu "



REM Labelf:
REM Thu "3 Monday Note Subjects
REM thu "
REM Thu "Back One Screen
REM Thu "
REM Thu "COLOR Printouts
REM Thu "
REM Thu "LPRINT Bug
REM Thu "
REM Thu "White Paper Piles
REM Thu "
REM Thu "Kieren Check
REM Thu "
REM Thu "News Headlines Hierarchy
REM Thu "
REM Thu "
REM Thu "End Number Bug
REM Thu "
REM Thu "Exercise to sit to exercise
REM Thu "
REM Thu "Glossary
REM Thu "
REM Thu "Word bold to Numbers
REM Thu "
REM Thu "



REM Labelg:
REM Thu "3 Sunday Note Subjects
REM Thu "
REM Thu "Zone Radio Scripts
REM Thu "
REM Thu "Zone News1 Sections
REM Thu "
REM Thu "SCREEN Printouts
REM Thu "
REM Thu "Party conversations
REM Thu "
REM Thu "Whitney Museum drop off
REM Thu "
REM Thu "
REM Thu "Spelling Checking
REM Thu "
REM Thu "Fifty Labeled Sections
REM Thu "
REM Thu "Cave
REM Thu "
REM Thu "Story
REM Thu "
REM Thu "Deadlines
REM Thu "
REM Thu "
REM Thu "Landscape Yellows notes
REM Thu "
REM Thu "Artificial Intelligence

REM Thu "
REM Thu "
REM Thu "2 Writing Habits
REM Thu "  tricks
REM Thu "  for others
REM Thu "  with others
REM Thu "  Writing Zone
REM Thu "
REM Thu "




REM Labelh:
REM Thu "3 Saturday Note Subjects
REM Thu "
REM Thu "P1 People    1P RGH
REM Thu "
REM Thu "P2 Problem   1W Writing
REM Thu "
REM Thu "P3 Program   2W Full Article
REM Thu "
REM Thu "P4 Procedure 2W Move Yellows
REM Thu "
REM Thu "P5 Progress  2C Computer Help
REM Thu "
REM Thu "P6 Plans     2W Writing Formats
REM Thu "
REM Thu "




REM labeli:
REM Thu "P1 People    1P Anxiety
REM Thu "             1P Deadline
REM Thu "             2C Final Form
REM Thu "
REM Thu "P2 Problem   1W News
REM Thu "             1C Output
REM Thu "             6P Self Help Mode Son or Daughter
REM Thu "
REM Thu "P3 Program   1C Label
REM Thu "             1C Headlines
REM Thu "             3W For This Week
REM Thu "             1W Features
REM Thu "             1 COLOR
REM Thu "
REM Thu "
REM Thu "P4 Procedure 1C Disks
REM Thu "             1C Printouts
REM Thu "             2W Advertisement Copy
REM Thu "             2C Catalog Entry
REM Thu "
REM Thu "P5 Progress  1P Diary Of Week
REM Thu "
REM Thu "P6 Plans     3W Weekly Time Slots
REM Thu "             3W Monthly Time Slot
REM Thu "
REM thu "



REM labelj:
REM Thu "P1 People    2P Seattle Problem
REM Thu "
REM Thu "P2 Problem   2P Jen Problem
REM Thu "
REM Thu "P3 Program   2P Condo Problem
REM Thu "
REM Thu "P4 Procedure 2O Charts Problem
REM Thu "
REM Thu "P5 Progress  2P Taxes Problem
REM Thu "
REM Thu "P6 Plans     2P Stock Problem
REM Thu "
REM Thu "





REM labelK:
REM Thu "P1 People    6P Dr. H  Need To Do Both
REM Thu "
REM Thu "P2 Problem   1P Exercise
REM Thu "             1W Writing
REM Thu "
REM Thu "P3 Program   2W Flat Sheet
REM Thu "             2W 1/2 Fold
REM Thu "             1C Yellows
REM Thu "
REM Thu "P4 Procedure P1-P6
REM Thu "             1C Categories
REM Thu "
REM Thu "P5 Progress  1W 5 Articles
REM Thu "
REM Thu "P6 Plans     3P Share This A M
REM Thu "
REM Thu "




REM labell:
REM Thu "P1 People    2P First Experiences
REM Thu "
REM Thu "P2 Problem   1P Confidence
REM Thu "
REM Thu "P3 Program   1W News
REM Thu "
REM Thu "P4 Procedure 2C Edit Labels
REM Thu "            2C Return-Return
REM Thu "
REM Thu "P5 Progress
REM Thu "
REM Thu "P6 Plans


REM Thu "
REM Thu "
REM Thu "label1:
REM Thu "P1 People    1P Neal
REM Thu "
REM Thu "P2 Problem   2C Programmer Development
REM Thu "
REM Thu "P3 Program   1C Loop
REM Thu "             1C Case
REM Thu "             1C Sub
REM Thu "
REM Thu "P4 Procedure 1C Remaining
REM Thu "
REM Thu "P5 Progress
REM Thu "
REM Thu "P6 Plans
REM Thu "
REM Thu "
REM Thu "label1:
REM Thu "P1 People    P1 Gene
REM Thu "             1M Customer
REM Thu "
REM Thu "P2 Problem   2C Programmer Develop
REM Thu "P3 Program   2C Recursion Patterns
REM Thu "
REM Thu "P4 Procedure 2C Down And Up
REM Thu "             1C Construction
REM Thu "
REM Thu "P5 Progress  3C triangle Was Sent
REM Thu "
REM Thu "P6 Plans     1P Call
REM Thu "
REM Thu "
REM Thu "label1:
REM Thu "P1 People
REM Thu "
REM Thu "P2 Problem   3O Office Report Problem
REM Thu "
REM Thu "P3 Program
REM Thu "
REM Thu "P4 Procedure
REM Thu "
REM Thu "P5 Progress
REM Thu "
REM Thu "P6 Plans
REM Thu "
REM Thu "
REM Thu "This is Sat AM
REM Thu "
REM Thu "Here are some full story articles
REM Thu "
REM Thu "label1:
REM Thu "P1 People
REM Thu "
REM Thu "P2 Problem
REM Thu "
REM Thu "P3 Program
REM Thu "
REM Thu "P4 Procedure
REM Thu "
REM Thu "P5 Progress
REM Thu "
REM Thu "P6 Plans
REM Thu "
REM Thu "
REM Thu "P1
REM Thu "P2
REM Thu "P3
REM Thu "P4
REM Thu "P5
REM Thu "P6
REM Thu "
REM Thu "




REM Labeln:
REM Thu "What is on the disk this week
REM Thu "
REM Thu "we want a new feature where we control color based on subject
REM Thu "we will use a  space 2 M space code
REM Thu "later we will have 3 or 4 character codes
REM Thu "we can make the change right in printing
REM Thu "
REM Thu "we will use the new basic command mid$(a$,2,1)
REM Thu "which looks at the second character of the 2 character pair
REM Thu "
REM Thu "
REM Thu "We need Thursday notes
REM Thu "fri notes
REM Thu "sat notes
REM Thu "even it they are just a diary list
REM Thu "
REM Thu "we will have the yellows typed up a bit each day
REM Thu "perhaps we will have 7 days worth of material sun to sat
REM Thu "
REM Thu "we can use the label1 - label9 system so that the newest first typing
REM Thu "reads chronologically(sequentially)
REM Thu "(note the basic response) to the parentheses in the above line
REM Thu "
REM Thu "
REM Thu "and now perhaps we can throw some of the stupid yellows away
REM Thu "
REM Thu "

REM Labelo:
REM Thu "This is 2 Fri. Am
REM Thu "
REM Thu "We have the new
REM Thu "people
REM Thu "problem
REM Thu "program
REM Thu "procedures
REM Thu "progress
REM Thu "plan
REM Thu "format for articles
REM Thu "
REM Thu "tell a full story with each of your note systems
REM Thu "
REM Thu "
REM Thu "This is 2 thu. am
REM Thu "
REM Thu "6/6/96
REM Thu "new printer
REM Thu "for portable
REM Thu "
REM Thu "PRINT envelopes
REM Thu "PRINT lab
REM Thu "PRINT colors
REM Thu "
REM Thu "new computer for Judy
REM Thu "
REM Thu "take pictures
REM Thu "
REM Thu "print envelopes
REM Thu "on office color printer
REM Thu "
REM Thu "on cannon color printer
REM Thu "
REM Thu "from basic
REM Thu "
REM Thu "from word
REM Thu "
REM Thu "headers for the news
REM Thu "
REM Thu "dot matrix printing
REM Thu "on envelopes
REM Thu "
REM Thu "children word processor
REM Thu "
REM Thu "2 Eleanor printer
REM Thu "
REM Thu "DATA statements
REM Thu "basic program
REM Thu "
REM Thu "headers after labels
REM Thu "hierarchy label system
REM Thu "
REM Thu "




REM labelp:
REM Thu "PRINT invoices
REM Thu "include newspaper
REM Thu "description of project
REM Thu "
REM Thu "create receipt sheets
REM Thu "for the month
REM Thu "for the year
REM Thu "for the past five years
REM Thu "
REM Thu "start showing the future
REM Thu "you have been preparing for
REM Thu "
REM Thu "dictate header subjects
REM Thu "
REM Thu "keep newsletter in sequence
REM Thu "preview your material
REM Thu "
REM Thu "rework your material
REM Thu "
REM Thu "




REM Labelr:
REM Thu "2 wed am
REM Thu "
REM Thu "run newsletter stuff into 1 word7
REM Thu "use words word wrap
REM Thu "use words global replace of carriage return
REM Thu "Use word spell check
REM Thu "
REM Thu "explore 2 qbasic size
REM Thu "test the size of the main module as data vs. rem
REM Thu "place 3 rem data statements in subroutines
REM Thu "
REM Thu "use this program now to 4 manage the yellow stickys
REM Thu "to write letters and capture phone numbers
REM Thu "to organize personal and professional and computer projects
REM Thu "and organize people
REM Thu "
REM Thu "use the program as a 2 database display
REM Thu "
REM Thu "get 3 single letter commands for different newsletter sections
REM Thu "P for personal
REM Thu "F financial
REM Thu "telephone
REM Thu "Numbers for the different sections of the newsletter
REM Thu "
REM Thu "the program allow everyone to personalize the data files immediately
REM Thu "for their own use
REM Thu "for their own business
REM Thu "
REM Thu "use this program as a semi-intelligent display
REM Thu "to handle the 2 condo problems
REM Thu "
REM Thu "to handle your 1 mail
REM Thu "give your mail two or three passes over three days
REM Thu "
REM Thu "handle your 2 check book
REM Thu "handle projects while programming or typing
REM Thu "
REM Thu "have one computer 1 news file per week
REM Thu "finish the week financially
REM Thu "finish the weeks mail
REM Thu "finish the weeks audio tape notes
REM Thu "
REM Thu "begin 3 next weeks notes after adequate size notes for this week
REM Thu "explain file directories
REM Thu "for copy command in dos and windows
REM Thu "
REM Thu "set up personal areas for people at the office
REM Thu "use a single letter for access
REM Thu "use alt s f find for editing
REM Thu "

REM labels:
REM Thu "make comments on the style of these beh pre
REM Thu "
REM Thu "create a special words subroutine
REM Thu "for sect and send
REM Thu "
REM Thu "create a special letters subroutine for restore
REM Thu "create a newsletter system for management
REM Thu "create a management newsletter system
REM Thu "
REM Thu "create a newsletter management system
REM Thu "distribute single sheets of paper
REM Thu "containing the cover, the introduction the highlights
REM Thu "of your newsletter magazine
REM Thu "
REM Thu "provide several pages of depth in the newsletter management system itself
REM Thu "PRINT the newsletter management system to hard copy as well
REM Thu "format the full news zine in word7
REM Thu "
REM Thu "discuss the airline arrangements for the Seattle trip
REM Thu "discuss all aspects of the Seattle trip
REM Thu "write a check to Kieren
REM Thu "discuss and display your weekly budget
REM Thu "display your monthly budget
REM Thu "display and discuss the taxes projects
REM Thu "
REM Thu "convert the bill paying data form your older check writing program
REM Thu "display your daily exercise schedule
REM Thu "display your daily food menu
REM Thu "
REM Thu "use the n l m program as your personal secretary
REM Thu "display a weekly schedule of standard events through out the week
REM Thu "have a secretary print the schedule daily
REM Thu "to remind you of important standard parts of your life
REM Thu "
REM Thu "manage your life by these priorities
REM Thu "
REM Thu "use the newsletter management system to document your activities
REM Thu "and their priorities
REM Thu "
REM Thu "set up the prodigy system on the portable for distribution
REM Thu "of the new management
REM Thu "SYSTEM
REM Thu "
REM Thu "use the newsletter m system to set priorities for AI educator
REM Thu "have others use the system to
REM Thu "
REM Thu "stimulate
REM Thu "prompt
REM Thu "motivate
REM Thu "control
REM Thu "manage
REM Thu "you
REM Thu "
REM Thu "type this tape as it is to a section of the newsletter
REM Thu "label this tape for today as wed notes
REM Thu "
REM Thu "talk a bit
REM Thu "type a bit
REM Thu "think a bit
REM Thu "travel a bit
REM Thu "teach a bit
REM Thu "torment a bit
REM Thu "tease a bit
REM Thu "telephone a bit
REM Thu "tape a bit
REM Thu "

REM Thu "end early am notes
REM Thu "
REM Thu "


END SUB

SUB OldTuesNotes


REM tues "Labelt:
REM TUES "late Tues. nights
REM TUES "people can control the program with enter or q as single characters
REM TUES "people get a new title and headers and footer to the screen segments
REM TUES "newsletter windows should have headlines
REM TUES "and 10-15 lines underneath
REM TUES "my current notes should be remarked out for now
REM TUES "
REM TUES "The program should be transparent to what is being said
REM TUES "do that  soon
REM TUES "
REM TUES "the program saves data to the line printer or a file
REM TUES "with extra intelligence for line printers and file extensions
REM TUES "we will sue this for actually printing the newsletter data in word7
REM TUES "there are some story paragraphs already set
REM TUES "lets try converting them into the newsletter format
REM TUES "
REM TUES "ask questions
REM TUES "they shall be answered.
REM TUES "
REM TUES "there is too much material here as usual
REM TUES "so cut it down
REM TUES "
REM TUES "end of Tues. eve notes
REM TUES "
REM TUES "


REM tues "Labelu:
REM TUES "2 New Screen after 3 Two Empty Lines
REM TUES "
REM TUES "I think that I will try to talk through the 4 design of the program
REM TUES "I may move stuff down here and work on it
REM TUES "I just need a counter for the blank lines
REM TUES "How can I make such a thing?
REM TUES "How do I know when I have a blank line?
REM TUES "
REM TUES "I can have the program print the 4 length of the lines it displays
REM TUES "it can display at the end of each line it prints
REM TUES "it can especially display only if the length is less that three
REM TUES "so I can get used to seeing the data and what is being displayed
REM TUES "Should this be a separate subroutine, or just a line
REM TUES "
REM TUES "
REM TUES "Should I have it so that it can be 4 turned on and off
REM TUES "should all our additions to the little program be on switches
REM TUES "Well a simple if statement before calling the printIt worked
REM TUES "and the answer is that the length is 0.
REM TUES "So I will make some kind of counter, which will exit the do at three
REM TUES "
REM TUES "And so I was 1 successful, see the code in news0c.bas
REM TUES "a simple counter was placed to count length 0 lines
REM TUES "it was reset to 0 if the line was longer
REM TUES "it was reset at the end of for loop
REM TUES "I went through and edited all the notes based on this simple addition
REM TUES "
REM TUES "
REM TUES "Printing to 2 Line Printer and 1 Display
REM TUES "
REM TUES "Now I need to have the screen printing go also to a file
REM TUES "I will experiment with print #1 with #1 being the SCRN:
REM TUES "we will see if that works.  Ah, yes, it does
REM TUES "Just use a$ = LPT1: or A$ = SCRN: in the open statement
REM TUES "open a$ for output as #1.  Now does the color Command still work
REM TUES "

REM TUES "OK what does 1 inkey do with the enter key
REM TUES "We can possibly tell about the l key
REM TUES "We can use F for a file name
REM TUES "but I want to use  ENTER for the standard Display
REM TUES "Lets see if we can do a test routine for the enter key
REM TUES "
REM TUES "
REM TUES "That was easy, the 1 ENTER key returns chr$(13)
REM TUES "So I have a little Introduction that asks for L for Line Printer Display
REM TUES "And I soon will have and  F  that asks for a file name
REM TUES "I may put in a Counter for the line Printer that prints 5 paragraphs
REM TUES "or a total of Fifty Five lines.  See Working version in Notes0D.Bas
REM TUES "
REM TUES "
REM TUES "And 4 Printing to a File and 2 the Display
REM TUES "
REM TUES "All right, I will try to 4 print to a file for a while (rhymes)
REM TUES "I wonder what I will do with the New Page or the CLS section
REM TUES "I think that I will just ignore it, and let the word processor paginate
REM TUES "It looks easy, like feeding candy to a baby, or something
REM TUES "I think I have it without even going through several versions
REM TUES "
REM TUES "Well, all right, it was easy, but I think that I want 4 Screen display as well.
REM TUES "I can have print there all the time, or I can have print #1, and #2
REM TUES "The right way is probably to have a flag to indicate that we are doing
REM TUES "something special, and print #1 only if that flag is on (or off).
REM TUES "With some work (and the NoExtra Flag) it works in Notes0E.bas
REM TUES "
REM TUES "


REM TUES "5 Neal Call, Tues. June Four
REM TUES "
REM TUES "Keep putting the 5 recent stuff on the top
REM TUES "keep working through the old stuff and improving it
REM TUES "making it more and more symmetrical
REM TUES "making it say something
REM TUES "keeping all similar material together
REM TUES "
REM TUES "
REM TUES "you are going to 5 keep reading and reading it
REM TUES "so that it will get better and better all the time
REM TUES "
REM TUES "4 Good Calorie Diet notes
REM TUES "we can have a newsletter just for the concepts
REM TUES "we are teaching her
REM TUES "
REM TUES "3 Smart Exercise notes
REM TUES "We can include all the material and notes
REM TUES "from the radio script project
REM TUES "
REM TUES "
REM TUES "Neal can 4 dictate over the phone
REM TUES "I will type
REM TUES "talk slowly
REM TUES "think deeply
REM TUES "it will come to you in news
REM TUES "

REM TUES "
REM TUES "We can use the
REM TUES "2 RESTORE command so that the notes can have proper sections
REM TUES "so that each subject can be displayed all by itself
REM TUES "
REM TUES "say pause
REM TUES "
REM TUES "I am set to 3 record our discussion now
REM TUES "we have missed deadlines and so we have to be focused and serious
REM TUES "I have reset deadline but that makes me very nervous
REM TUES "we do not have any more chances
REM TUES "
REM TUES "we have to discuss and be clear about
REM TUES "use of deadlines in our work
REM TUES "
REM TUES "

REM TUES "so what is your
REM TUES "3 response to newsletter news0
REM TUES "
REM TUES "The problem with the colors can be solved in the code

REM TUES "or you can 3 adjust screen brightness on your display tube
REM TUES "

REM TUES "We can have colors change from the top to the
REM TUES "middle and to the bottom of screen
REM TUES "
REM TUES "4 subroutines
REM TUES "same you will use in triangle
REM TUES "
REM TUES "prim. is a primitive
REM TUES "global replace to remove
REM TUES "
REM TUES "
REM TUES "4 CD. conceptual dependence representation
REM TUES "not the CD on the computer
REM TUES "no music comes out
REM TUES "
REM TUES "Sam.
REM TUES "Logo.
REM TUES "
REM TUES "
REM TUES "3 first and butfirst
REM TUES "as subroutines
REM TUES "
REM TUES "2 COLOR randomization bothers Neal
REM TUES "but he now understands that in this program the number is
REM TUES "is the 3 number of words that you are going to color
REM TUES "
REM TUES "1 printit
REM TUES "is the main routine
REM TUES "news itself has code at the bottom
REM TUES "and calls printit
REM TUES "
REM TUES "
REM TUES "1 think about how it works
REM TUES "and study it
REM TUES "
REM TUES "so what are your 2 overall impressions
REM TUES "
REM TUES "this is the raw text from the 2 newsletter template
REM TUES "tricked it up with the editing in qbasic
REM TUES "displayed it
REM TUES "
REM TUES "
REM TUES "to make the news program in the old version you had to have
REM TUES " a 3 separate txt file
REM TUES "that separate file could get lost
REM TUES "or not be in the same directory as basic
REM TUES "
REM TUES "
REM TUES "we need a discussion of the 2 do loop
REM TUES "
REM TUES "we can use 3 g for glossary
REM TUES "
REM TUES "lots of different letters can be used in the 2 inkey part
REM TUES "
REM TUES "we can use 4 input#1 for the screen
REM TUES "
REM TUES "
Tuesday:
REM TUES "3 Tues. AM Notes
REM TUES "
REM TUES "2 Current problems with the edit and display system

REM TUES "
REM TUES "3 converts all apos, including those in cant
REM TUES "so avoid contractions, use can not instead
REM TUES "you need to do manual checks to avoid this
REM TUES "or just edit later
REM TUES "
REM TUES "Any number like Fifty Five is made less than ten (i.e. five)
REM TUES "
REM TUES "
REM TUES "SECT
REM TUES "do we want this 6 little program to be the standard
REM TUES "communicator in our world for a year
REM TUES "do we want 7 small increments in the intelligent subroutines
REM TUES "do we want people to live with these small problems
REM TUES "
REM TUES "perhaps yes, we do
REM TUES "
REM TUES "and we need to 3 type a lot
REM TUES "and use this a lot
REM TUES "and think in terms of sentence segments
REM TUES "for the long term development of the AI material
REM TUES "
REM TUES "soon we will also be in the 2 word6 world
REM TUES "and that will have different 2 problems and
REM TUES "1 pleasures
REM TUES "
REM TUES "
REM TUES "SECT
REM REM TUES "The program doesnt handle numbers like 2-4
REM REM TUES "perhaps 2 - 4 will be OK  .
REM TUES "
REM TUES "Use 1 REM to solve problems temporarily
REM TUES "or to make things as hidden text
REM TUES "
REM TUES "You can use 1 spaces in your editing material
REM TUES "and it will also be 5 hidden to the display process
REM TUES "
REM TUES "  We can possible soon have a discussion about
REM TUES "  the 1 format of beh pre
REM TUES "  and the format of these edited sentences
REM TUES "
REM TUES "  note that we can also use the 1 indenting
REM TUES "  and that is captured automatically as we type
REM TUES "

REM TUES "
REM TUES "  we have problems with 2 raw typing of word like
REM TUES "  1 SCREEN
REM TUES "  2 AND PLAY
REM TUES "  they are capitalized by the basic syntax checker
REM TUES "  if they appear near the front of a sentence
REM TUES "
REM TUES "
REM TUES "5 spaces in front of numbers and after them are used to color things
REM TUES "use word6 and not word six
REM TUES "
REM TUES "you also can not have a 5 number as the last character in a sentence
REM TUES "some of these problems can be handled with more programming
REM TUES "some of the problems will be handled by converting to parentheses
REM TUES "
REM TUES "
REM TUES "SECT
REM TUES "the numbers make it so we are 2 cross compatible with other
REM TUES "languages Using the numbers to bold face, etc.
REM TUES "Double quotes in the sentences mess up the read
REM TUES "
REM TUES "I am looking forward to 2 word wrap
REM TUES "I want to see things looking nicely on the screen
REM TUES "and I want to see the phrases looking nicely in the qbasic editor
REM TUES "
REM TUES "

Newsletter:
REM TUES "5 Newsletter dictation, Monday tape notes
REM TUES "1 News0a
REM TUES "
REM TUES "6/3/96
REM TUES "
REM TUES "
REM TUES "Section one 2 People issues
REM TUES "
REM TUES "3 dictate first thing in the am
REM TUES "2 eyes closed
REM TUES "in five p[paragraph sections
REM TUES "with the 2 newsletter format in mind
REM TUES "
REM TUES "I have to 4 design the word wrap things
REM TUES "I want to be able to have things in these phrases
REM TUES "and have the program combine them up to the periods.
REM TUES "
REM TUES "I also want things extracted out via the 1 periods
REM TUES "so there is no phrase wrap around across 2 period boundaries
REM TUES "
REM TUES "set up 4 radio script white sheets
REM TUES "for each of the five sections of the newsletter
REM TUES "and possibly each paragraph of the newsletter.
REM TUES "
REM TUES "
REM TUES "5 type up stuff for neal
REM TUES "
REM TUES "ask for the 4 beh pre deep thought
REM TUES "have 5 sentences in each of three sections
REM TUES "(five sections)
REM TUES "
REM TUES "go for 2 final versions now since you are past the deadline.
REM TUES "leave white space at the bottom of each of the five columns
REM TUES "if necessary
REM TUES "
REM TUES "go back and forth between 1 word as the word processor
REM TUES "and 1 qbasic as the quick display systems
REM TUES "
REM TUES "
REM TUES "2 have fun
REM TUES "2 play games
REM TUES "1 dream about the futures
REM TUES "make some money
REM TUES "
REM TUES "have 2 neal dictate a 4 summary of his tapes in beh pre forma
REM TUES "and I will type it or possible Eleanor will type it
REM TUES "to works
REM TUES "
REM TUES "End of section one
REM TUES "
REM TUES "
REM TUES "3 Section two parentheses
REM TUES "
REM TUES "Integrate an old simple programming environment
REM TUES "with a new powerful modern programming environment
REM TUES "qbasic and word 6 basic
REM TUES "discuss the human features
REM TUES "the psychology
REM TUES "discuss the conceptual features the ideas
REM TUES "a and discuss the important details the formalize of intelligence
REM TUES "as a computer program
REM TUES "
REM TUES "
REM TUES "Section two four
REM TUES "share the program and build on each others work
REM TUES "constantly improving and clarifying the code and the subs
REM TUES "
REM TUES "share the newsletter every day in written form
REM TUES "as a word six 7 newsletter and as a qbasic exe document
REM TUES "free
REM TUES "discuss your progress and report other peoples progress
REM TUES "in these areas
REM TUES "
REM TUES "end of section
REM TUES "
REM TUES "
REM TUES "typing can be accomplished to any word processor
REM TUES "pref. with word wrap
REM TUES "notepad
REM TUES "works
REM TUES "edit
REM TUES "basic
REM TUES "word
REM TUES "
REM TUES "your ideas can come out from this tape
REM TUES "
REM TUES "
REM TUES "3 overtyping in word allows the formatting to be absolute
REM TUES "the formatting can be preserved exactly
REM TUES "
REM TUES "We can also work by 5 converting existing words to nonsense by
REM TUES "add ascic characters
REM TUES "
REM TUES "text can be up loaded by email
REM TUES "via any Internet email system
REM TUES "
REM TUES "exe program can uploaded as well
REM TUES "zips can be uploaded AS well
REM TUES "we can use the Internet or the big systems such as
REM TUES "aol
REM TUES "prodigy
REM TUES "CompuServe
REM TUES "
REM TUES "
REM TUES "you can just 5 read the subsection of notes
REM TUES "global replaces in work
REM TUES "text out on the screen
REM TUES "you have lots of readme.txt files
REM TUES "and the program can display these
REM TUES "and you can includes parentheses]
REM TUES "and the program will read them
REM TUES "lets all write another editor
REM TUES "
REM TUES "
REM TUES "exe programs can have data in them
REM TUES "rather than reading the notes file
REM TUES "then you can just read the subsection of notes rather than the cods
REM TUES "
REM TUES "2 global replaces in word
REM TUES "allow the apos to be added
REM TUES "then change the apos to data
REM TUES "
REM TUES "word also uses the ^p
REM TUES "
REM TUES "
REM TUES "3 questions in answers
REM TUES "
REM TUES "INPUT a$
REM TUES "PRINT a$
REM TUES "
REM TUES "a$
REM TUES "
REM TUES "if then else
REM TUES "
REM TUES "progress
REM TUES "plans
REM TUES "procedures
REM TUES "
REM TUES "
REM TUES "3 news one four
REM TUES "will discuss the following subjects
REM TUES "
REM TUES "here is a crude discussion of the subject to come
REM TUES "current subjects to be discussed
REM TUES "
REM TUES "end of newsletter
REM TUES "
REM TUES "


Monday:
REM TUES "2 Monday notes
REM TUES "
REM TUES "we are looking for the elegant little programs
REM TUES "we are looking for the ideas well said
REM TUES "
REM TUES "we are looking for 2 classic quote
REM TUES "we are looking for 2 classic programs
REM TUES "that deserve place in immortality
REM TUES "and can serve as a 1 foundation upon which you can elaborate
REM TUES "yourself to come up with a little program well said
REM TUES "well written
REM TUES "a little bit of intelligence you are proud of
REM TUES "
REM TUES "
REM TUES "a lot of thought can go into an elegant simple clean clear program
REM TUES "and it well documented the program is a gem
REM TUES "rose by any name you want to call it
REM TUES "a joy forever
REM TUES "a treasure
REM TUES "a pearl
REM TUES "
REM TUES "
REM TUES "people
REM TUES "who we are and what we are doing
REM TUES "
REM TUES "
REM TUES "programs
REM TUES "the current state of the 2 AI pearls that are being developed
REM TUES "
REM TUES "
REM TUES "progress
REM TUES "2 brief stories of what has been accomplished this week
REM TUES "
REM TUES "procedures
REM TUES "parentheses
REM TUES "questions and answers
REM TUES "plans
REM TUES "
REM TUES "parentheses
REM TUES "the source of all intelligence
REM TUES "the real source of intelligence
REM TUES "intelligence revealed
REM TUES "added depth
REM TUES "
REM TUES "the hidden intelligence
REM TUES "explain it to me
REM TUES "
REM TUES "
REM TUES "2 daily newsletters in this format
REM TUES "discussing people
REM TUES "discussing programs
REM TUES "progress
REM TUES "
REM TUES "neal can add to any of those
REM TUES "
REM TUES "we will ask question
REM TUES "and so you will have of the five versions, something that is worth
REM TUES "putting out
REM TUES "
REM TUES "
REM TUES "and I think they all will be in q basic
REM TUES "
REM TUES "we need a build at the end of each day
REM TUES "which will include some text at the top of the program
REM TUES "
REM TUES "some notes right in the program of what you are doing
REM TUES "and how you are doing it
REM TUES "
REM TUES "I can extract all the Rem statements and have them as notes
REM TUES "
REM TUES "
REM TUES "rewrite news6 now
REM TUES "
REM TUES "looking for the pearls of wisdom
REM TUES "this little sheet of paper
REM TUES "this little newsletter sheet of paper
REM TUES "it is better than
REM TUES "vbdos
REM TUES "screen cam
REM TUES "etc.
REM TUES "it a valuable important sheet of paper
REM TUES "
REM TUES "vb4
REM TUES "windows Ninety Five
REM TUES "tviator
REM TUES "
REM TUES "
REM TUES "qbasic with some pearl
REM TUES "quiet tape recorder
REM TUES "and lots of human thought and input
REM TUES "
REM TUES "technology can not touch the people stuff
REM TUES "
REM TUES "
REM TUES "


REM TUES "More tape notes end of Monday
REM TUES "
REM TUES "newsletter
REM TUES "keep a focus on that
REM TUES "progress today
REM TUES "
REM TUES "data
REM TUES "apos added DATA to the front of all data
REM TUES "
REM TUES "remove reference to the REM TUES "
REM TUES "that is stripping from the first of the line
REM TUES "
REM TUES "
REM TUES "since we are using rems other places
REM TUES "type
REM TUES "compose your sentences
REM TUES "add apos
REM TUES "run program
REM TUES "or manually
REM TUES "go back and forth between data and apos
REM TUES "
REM TUES "get some word processor gimmick
REM TUES "
REM TUES "remove old parts that had to do with the old files
REM TUES "design finished
REM TUES "and perfected
REM TUES "
REM TUES "it is easier to ass numbers that to add double parentheses
REM TUES "
REM TUES "later the program can try to add the double parentheses
REM TUES "
REM TUES "
REM TUES "we are evolving the five states in organization
REM TUES "
REM TUES "get the people issues
REM TUES "processes
REM TUES "human processes
REM TUES "people process
REM TUES "
REM TUES "program
REM TUES ";down and printed
REM TUES "now the progress comments about he program
REM TUES "
REM TUES "
REM TUES "output?
REM TUES "
REM TUES "is that the input
REM TUES "
REM TUES "questions are the so whats
REM TUES "
REM TUES "procedures
REM TUES "keep finding the p works
REM TUES "
REM TUES "plan
REM TUES "dictate all five section
REM TUES "each day
REM TUES "extract the best of the sections
REM TUES "
REM TUES "get only two days
REM TUES "
REM TUES "
REM TUES "people
REM TUES "elements
REM TUES "have to type without word wraps
REM TUES "
REM TUES "add apos
REM TUES "
REM TUES "people can watch the color display
REM TUES "see it fast
REM TUES "random colors
REM TUES "make it hard to read
REM TUES "make it flashing
REM TUES "
REM TUES "
REM TUES "3 Sentences per Screenful
REM TUES "have twenty to twenty five sentences on a page
REM TUES "
REM TUES "twenty five seems nice AND symmetrical
REM TUES "
REM TUES "so that you get an input work and output
REM TUES "
REM TUES "the screens should come in a std thing
REM TUES "over type the data statements
REM TUES "
REM TUES "
REM TUES "get an 2 editing screen full in so you see a display
REM TUES "look for cls
REM TUES "
REM TUES "people can now sit with a screenful
REM TUES "a fist full of dollars
REM TUES "
REM TUES "type upper a
REM TUES "mid and lower screen full
REM TUES "so that colors do mean something
REM TUES "
REM TUES "
REM TUES "write 1 letters to people
REM TUES "tell stories about what you have thought about them
REM TUES "
REM TUES "get mockups for the letters
REM TUES "have mockup stored in the proper book
REM TUES "hard bound books (binders
REM TUES "
REM TUES "
REM TUES "three hole punched for our orders
REM TUES "
REM TUES "perhaps we can have these screenfuls of information
REM TUES "for each thing in the 1 office
REM TUES "read the newsletter in the office
REM TUES "
REM TUES "need to be fun
REM TUES "have 1 jokes
REM TUES "some humor
REM TUES "the eye of a potato as well
REM TUES "
REM TUES "have 3 three blank lines define a new screen full
REM TUES "have a counter in the display section to do that
REM TUES "maybe it will be easy
REM TUES "
REM TUES "
REM TUES "This is my 4 response to this program
REM TUES "
REM TUES "here is my answer to this question
REM TUES "signed neal
REM TUES "
REM TUES "Now go to the first line of your typing and put and apos
REM TUES "insert apostrophes in front of all lines and blank lines
REM TUES "then you can make a 2 global replace of the DATA quote for the apos
REM TUES "
REM TUES "
REM TUES "ultimate goal is to 2 respond intelligently to peoples responses
REM TUES "as in a game
REM TUES "
REM TUES "make the code in the games easier
REM TUES "
REM TUES "
REM TUES "here is a 1 story for you about my son
REM TUES "
REM TUES "pong
REM TUES "
REM TUES "break out game
REM TUES "off the wall with the ball game
REM TUES "
REM TUES "until I achieved the first break out
REM TUES "
REM TUES "watch actively
REM TUES "jaw dropped
REM TUES "beat at every game except golf
REM TUES "
REM TUES "saw what the activity level
REM TUES "wonderful thing will happen
REM TUES "as soon as he achieved the solution
REM TUES "
REM TUES "wonderful things happen
REM TUES "in our program
REM TUES "your words go out to the world
REM TUES "
REM TUES "
REM TUES "
REM TUES "4 how do I interact with the program
REM TUES "explanation above
REM TUES "
REM TUES "try a few lines
REM TUES "RUN it

REM TUES "and see what happens
REM TUES "interact with the program and show your answers on screen
REM TUES "
REM TUES "the program is not actually responding, yet
REM TUES "is just printing what I wrote
REM TUES "
REM TUES "the ultimate goal will be to 4 get some intelligent response
REM TUES "
REM TUES "
REM TUES "I had a 2 second question after the instructions were done
REM TUES "
REM TUES "so what
REM TUES "explanation in later steps in AI
REM TUES "we parse the sentence
REM TUES "we 5 come back with a response
REM TUES "that is the challenge for AI
REM TUES "
REM TUES "long temp picture that keeps people flowing in that direction
REM TUES "challenge to be more sophisticated
REM TUES "with AI
REM TUES "and qbasic
REM TUES "and word6 input
REM TUES "input
REM TUES "PRINT
REM TUES "and word print
REM TUES "
REM TUES "
REM TUES "section
REM TUES "
REM TUES "aside
REM TUES "story
REM TUES "illustrate the 4 difficulty in some programming
REM TUES "
REM TUES "4 star trek next generation
REM TUES "
REM TUES "no contractions in Data(s) vocabulary
REM TUES "story that related to what we have just talked about
REM TUES "add some human stuff
REM TUES "that is what makes it come alive
REM TUES "
REM TUES "they will remember that story
REM TUES "so make up some stories for us
REM TUES "make up more colorful examples


END SUB

SUB PressEnter
REM Routine To Allow Special Displays (with no Additional Single Keys)


VIEW PRINT 3 TO 25
COLOR 15: LOCATE 24, 1
PRINT SPACE$(80);
LOCATE 24, 1
CALL Printit2(Cent("Press  1 <ENTER>  to Continue"), 0)
REM   LOCATE 20, 1

PEQuitLoop:
a$ = INKEY$: IF a$ = "" THEN GOTO PEQuitLoop
REM   IF a$ = "q" OR a$ = "Q" THEN END:

CALL NewsInitScreen


END SUB

SUB PRINTit (Sent$)

'Input -
Sent1$ = Sent$
CALL RemoveColorNumbers(Sent1$, CleanSent$)

IF StoryLog = 1 THEN PRINT #2, CleanSent$:
'PRINT CleanSent$: t = t + 1: PRINT t

'Work - Print it with colors
CALL Printit2(Sent$, 0)

'Finish
'Reset the colors
COLOR 7:

END SUB

SUB Printit2 (Sent$, Remaining)

REM Prints a Sentence coloring groups of words preceeded by a number
   REM CALL Tut.PrintIt2(A$, 0)

'IF NoExtra = 0 THEN PRINT #1, CHR$(39);

PrintIt1TopOfLoop:
IF Sent$ = "" THEN GOTO PrintIt1ExitLoop

REM Check for Leading Apostrophe
IF LEFT$(Sent$, 1) = CHR$(39) THEN Sent$ = RIGHT$(Sent$, LEN(Sent$) - 1)

IF Sent$ = "" THEN
      PRINT
      IF NoExtra = 0 THEN PRINT #1, CHR$(39)
      GOTO PrintIt1TopOfLoop
END IF


IF Remaining < 1 THEN
   COLOR 7
   IF FileName$ = "LPT1:" THEN LPRINT CHR$(27); CHR$(114); CHR$(0); : REM black
   Remaining = 0
END IF

temp = ASC(LEFT$(Sent$, 1))
IF temp > 48 AND temp < 59 THEN
   Remaining = VAL(LEFT$(Sent$, 1))
   CALL ColorIt1
   IF MID$(Sent$, 2, 1) <> " " THEN CALL CategoryColor(Sent$)

   Sent$ = RIGHT$(Sent$, LEN(Sent$) - 2)
   GOTO PrintIt1TopOfLoop
END IF

REM IF Remaining >= 1 THEN CALL BrightColorIt
REM IF Remaining >= 1 THEN COLOR 15


PRINT First(Sent$); " "; : REM  PRINT Remaining;
'IF NoExtra = 0 THEN PRINT #1, First(Sent$); " "; : REM  PRINT Remaining;
Sent$ = ButFirst(Sent$): Remaining = Remaining - 1
GOTO PrintIt1TopOfLoop

PrintIt1ExitLoop:
PRINT
'IF NoExtra = 0 THEN PRINT #1,

END SUB

SUB RemoveColorNumbers (Sent$, CleanSent$)
'Remove Single digit color numbers and space
'Remove Leading '#' which you can use to retain other numbers


RCN10:
IF Sent$ = "" THEN GOTO RCN90

REM Check for Leading Apostrophe
REM Used for printing notes from QBasic
IF LEFT$(Sent$, 1) = CHR$(39) THEN Sent$ = RIGHT$(Sent$, LEN(Sent$) - 1)

RCN30:
IF Sent$ = "" THEN GOTO RCN90


RCN50: 'Remove the # sign
IF ASC(LEFT$(Sent$, 1)) = 35 THEN
  Sent$ = RIGHT$(Sent$, LEN(Sent$) - 1): 'Skip #
  ELSE
  GOTO RCN60
END IF

RCN55: 'Preserve Sent$ through next space
  CleanSent$ = CleanSent$ + LEFT$(Sent$, 1): Sent$ = RIGHT$(Sent$, LEN(Sent$) - 1)
  IF ASC(LEFT$(Sent$, 1)) > 32 THEN GOTO RCN55
 
  CleanSent$ = CleanSent$ + LEFT$(Sent$, 1): Sent$ = RIGHT$(Sent$, LEN(Sent$) - 1)
  GOTO RCN30


RCN60: 'Remove leading number (only one digit) and space (Added Color Numbers)
  temp = ASC(LEFT$(Sent$, 1))
  IF temp > 48 AND temp < 59 THEN
     Sent$ = RIGHT$(Sent$, LEN(Sent$) - 2)
     GOTO RCN30
  END IF


RCN70:
CleanSent$ = CleanSent$ + First(Sent$) + " "
Sent$ = ButFirst(Sent$)
GOTO RCN30


RCN90:
IF RIGHT$(CleanSent$, 1) = " " THEN
  CleanSent$ = LEFT$(CleanSent$, LEN(CleanSent$) - 1)
END IF


END SUB

DEFINT A-Z
SUB sparkle


REM     Creates flashing border for intro and game over screens
REM    Sets Red with Black Background
  COLOR 4, 0

  a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "
  WHILE INKEY$ <> "": WEND: REM  Clear keyboard buffer

  WHILE INKEY$ = "": REM   Pause until Key Pressed

    FOR a = 1 TO 5: COLOR a + 10
      LOCATE 1, 1: REM  print horizontal sparkles
      PRINT MID$(a$, a, 80);
      LOCATE 22, 1
      PRINT MID$(a$, 6 - a, 80);

      FOR b = 2 TO 21: REM  Print Vertical sparkles
        c = (a + b) MOD 5
        IF c = 1 THEN
          LOCATE b, 80
          PRINT "*";
          LOCATE 23 - b, 1
          PRINT "*";
        ELSE
          LOCATE b, 80
          PRINT " ";
          LOCATE 23 - b, 1
          PRINT " ";
        END IF
      NEXT b:     REM   Move the Side Edges
    NEXT a:       REM   Move the top and bottom rows
 
  WEND: REM   Waiting for Key Press
END SUB

DEFSNG A-Z
SUB StartUpStuff

CLS
LOCATE 1, 1
COLOR 14
CALL CenterIt("NewsLetter Display Program")

LOCATE 24, 1: COLOR 15
CALL Printit2(Cent("  Press  1 <ENTER>  to Continue"), 0)

COLOR 14
LOCATE 10, 1
CALL CenterIt("AI_Educator")

LOCATE 12, 1
CALL CenterIt("Newsletter Display Program")

CALL sparkle


END SUB

SUB TodaysHeadlines
DIM Head$(10)

Head$(0) = "       2 Todays Headlines          3  News One             Version H              "
Head$(1) = "     1 a =                         1 j = Jennifer             1 s =               "
Head$(2) = "     1 b =                         1 k =                      1 t =               "
Head$(3) = "     1 c =                         1 l =                      1 u =               "
Head$(4) = "     1 d =                         1 m =                      1 v =               "
Head$(5) = "     1 e =                         1 n = Neal                 1 w = Write It      "
Head$(6) = "     1 f =                         1 o =                      1 x =               "
Head$(7) = "     1 g =                         1 p =                      1 y = Jun Notes     "
Head$(8) = "     1 h = Headlines               1 q =                      1 z = Anne Notes    "
Head$(9) = "     1 i =                         1 r =                 "
Head$(10) = "             "



LOCATE 5, 1
CALL Printit2(Head$(0), 0)
PRINT : PRINT


COLOR 7
CALL Printit2(Head$(1), 0)
CALL Printit2(Head$(2), 0)
CALL Printit2(Head$(3), 0)
CALL Printit2(Head$(4), 0)
CALL Printit2(Head$(5), 0)
CALL Printit2(Head$(6), 0)
CALL Printit2(Head$(7), 0)
CALL Printit2(Head$(8), 0)
CALL Printit2(Head$(9), 0)



END SUB

SUB Wed

REM This is the Wed eve notes
REM
REM I will be typing lots of stuff even while the TV is on
REM I will try to type up the slow notes from the AM
REM We have the word7 macro that bold faces the numbered words
REM Now I will try to type up the notes from this afternoon
REM
REM
REM wed Aft notes
REM
REM Write an interactive file
REM to save the persons answers to text files
REM
REM structure a set of quotations so you write up the news well
REM set up questions to write up program descriptions well
REM set up writing formats for different individuals
REM set up journalism writing questions for word on the Mac
REM
REM sent these journalism questions to the journalism professor from southern
REM
REM set up questions for children
REM for parents
REM
REM on values
REM on diet
REM on exercise
REM
REM
REM make your newspaper completely interactive
REM to send on email or on paper
REM or in basic
REM
REM have the possibility of writing different text files
REM write your responses to this program to us
REM what do you think
REM how could we make this better
REM
REM what do you think about AI
REM about modeling your intelligence
REM about having an adventure in your mind
REM about you getting to know you better
REM about connecting with others who think about the same kinds of things as you do
REM
REM out emphasis is your writing up your stories
REM your life
REM we emphasize you managing your life with this newsletter management system
REM
REM practice with this top level front end of our newsletter management system
REM
REM develop some comprehensive categories of life to use in the news letter management system
REM
REM read the news each week so you wont be surprised
REM so you will not get behind
REM
REM press the numbers and the letters on the keyboard for some surprises
REM
REM press enter for more data on any subject
REM press h for the headlines and the page number or letter where the article is found
REM
REM weekly news starting Jun first
REM
REM buy the CD ROM that has all the news from AI Ed for the past three years
REM with comprehensive indexes perhaps
REM
REM basic can run exe program
REM
REM have programmers write extensive design documentation
REM and program implementation systems and
REM isolated subroutines for their program contributions
REM
REM run the Microsoft discipline of daily builds that can be integrated with everyone elses work
REM
REM develop systems where multiple programmers and writers can work together in the AI newspaper system worldwide
REM
REM ask each person to develop personal newspapers
REM
REM lead the way with your own personal newspaper
REM
REM replace the headlines (letters) with real headlines
REM
REM tools, commands
REM maybe 4 hierarchies of letter combinations
REM that can restore you anywhere in a huge database
REM
REM what a 3 powerful tutorial system
REM
REM read the news on the computer so you can write your own responses and stories
REM
REM and then integrate them into your own newsletter
REM have basic look for a file name
REM integrate a file name into itself
REM
REM with data statements and possible labels
REM
REM or at least to the top of the newsletter
REM
REM
REM write the newsletter in basic
REM run the program that adds it immediately back into itself
REM into a new version of itself
REM
REM you are adding intelligence to the program
REM to the tutorial
REM to your own personal data base
REM
REM talk slowly to neal on the phone
REM
REM share ideas and predict the future through these slow ideas
REM
REM use the H letter for headline summary in the newspaper we are producing
REM
REM encourage people to type responses immediately so they know the contents of the numbered sections
REM
REM have them explore one two a c whatever by looking at the first paragraph in our newspaper
REM and then pressing enter to proceed through the newspaper
REM
REM consider an exhaustive, detailed large newspaper each week in exe form
REM
REM implement Neals suggestion of a response ability of the program
REM
REM allow comments on any item in the newspaper out to a separate text file
REM
REM develop an automatic program which adds the readers text back into an exe program
REM
REM run multiple exe programs controlled from basic ultimately from a CD ROM
REM
REM with appropriate indexes and headline section
REM
REM basic can go back and run the exe news0 program
REM and even search for the subject
REM
REM so we can develop a large data base from these basic program
REM
REM multi level menus can allow huge numbers of restores
REM and a could lead us to all of the second character divisions under a
REM
REM encourage people to write their own news letters
REM
REM encourage people to develop expert comments on their own subjects
REM
REM facilitate database collections for phone numbers available in this newsletter management system
REM
REM a possible task for customers is programming Bart pictures
REM
REM another task is programming questions of newspaper generation
REM
REM write a program but then write either a tutorial or a description of the program
REM
REM write a description from the categories offered in the newsletter management program
REM
REM answer a set of questions
REM view this
REM predict this
REM develop structured prompts for people to use in writing tutorial documentation
REM program design features
REM
REM insist any programmer who works for you offer
REM elaborate design documents if possible before committing to extensive coding
REM
REM create newspapers where people can comment themselves and distribute their feelings about religion or politics or psychic powers
REM
REM create a newsletter of your horoscope for the month
REM
REM create a version of neal thoughts file
REM
REM look at whether schanks program hierarchy
REM whether the current AI Ed plan is appropriate
REM
REM would it be better to have political quotes
REM and your familys response
REM
REM find out what people buy
REM
REM offer the existing programming hierarchy
REM
REM continue to explore high level issues
REM
REM explore issues in your office
REM
REM what are the responses of people in our office to the subjects we are discussing
REM
REM here is an interactive newsletter program where you read and you type without having to know any word processing program to build consensus
REM
REM build consensus in the office via this program
REM
REM anyone who plays in our system to do both
REM learn some programming every day every week
REM and to write some philosophy
REM some parenting guideline
REM some knowledge story
REM
REM write newsletter content for EL, me neal and gene
REM maybe ayal
REM
REM add an automatic indexing device that says your text is label a13
REM
REM intelligently merge text comments into the proper section of the program
REM
REM develop and use this system at the office now
REM
REM your audience of all the people there
REM
REM use the system with your family
REM your audience consists of all members of your family
REM
REM use the system with your organizations
REM your audience consists of all members
REM
REM fix the bathroom drawer
REM
REM record the to do lists each day
REM move to do list issues up and down on a
REM screen face priority hierarchy
REM
REM get the computer to numbers ideas automatically in priority screens
REM
REM focus constantly on mental health
REM on management
REM on psychology issues
REM as you do AI programming in qbasic
REM
REM
REM I dont think we can to that sentence
REM so we will
REM end this tape now
REM
REM

REM Travel
REM boards
REM Lawyers
REM
REM
REM Home town buffet
REM
REM gazpacho
REM borscht
REM saffron rice
REM
REM








END SUB

SUB WriteIt
REM Input File Name to write notes too (or use My_Notes.Bas)
REM Input Lines of Notes, End with 3 Carriage Returns (2 Blank lines)

PRINT : COLOR 13

  PRINT "File Name  (less that 8 Characters, no spaces, no periods) "
  LINE INPUT "or press  <ENTER> to use MY_NOTES.BAS ", FileName$
  IF FileName$ = "" THEN FileName$ = "MY_NOTES"
  FileName$ = FileName$ + ".BAS": PRINT
                     
         COLOR 14: PRINT "type  <ENTER>  twice to end": PRINT
         OPEN FileName$ FOR APPEND AS #2: z$ = CHR$(39): REM Apostrophie

WriteLoop:
                  COLOR 15
                  IF LEN(z$) < 1 THEN WriteCounter = WriteCounter + 1
                  IF WriteCounter = 2 THEN CLOSE #2: GOTO WriteItBye
                  IF LEN(z$) >= 1 THEN WriteCounter = 0

                  LINE INPUT z$
                  PRINT #2, CHR$(39); z$
                  GOTO WriteLoop

WriteItBye:
END SUB

