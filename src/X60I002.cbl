      CBL OPT(2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. X60I002 RECURSIVE.
       AUTHOR.     ALAIMO.
      *----------------------------------------------------------------
      * X60I002
      * **++ routine ricorsiva per inblock di un campo TLV.
      *
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-370.
       OBJECT-COMPUTER.    IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
      * class to check valid HEX string
           CLASS HEX-VALID IS '0' THRU '9', 'A' THRU 'F'.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      **
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
       01 WK-LITERALS.
         03 PGM-NAME                         PIC X(8) VALUE 'X60I002'.
         03 CC-HEX                           PIC X(1) VALUE 'H'.
      *
       LOCAL-STORAGE SECTION.
       01 LS-UTILS.
         03 BITS.
           05 BIT-8                          PIC X.
           05 BIT-7                          PIC X.
           05 BIT-6                          PIC X.
           05 BIT-5                          PIC X.
           05 BIT-4                          PIC X.
           05 BIT-3                          PIC X.
           05 BIT-2                          PIC X.
           05 BIT-1                          PIC X.
         03 IDX                              PIC 9(9) COMP.
         03 W-IDX                            PIC 9(9) COMP.
         03 PREV-ERR-POS                     PIC X(50).
         03 EFF-T-LEN                        PIC 9(9) COMP.
         03 EFF-V-LEN                        PIC 9(9) COMP.
         03 TAG-ID                           PIC X(20) VALUE SPACE.
         03 TAG-LEN-AREA.
           05 TAG-LEN-N                      PIC 9(9).
           05 TAG-LEN-X REDEFINES TAG-LEN-N  PIC X(9).
         03 TAG-VAL                          PIC X(999).
         03 FIRST-SUBEL-POS         PIC 9(9) COMP.
         03 FIRST-DASH-POS          PIC 9(9) COMP.
         03 FIRST-SPACE-POS         PIC 9(9) COMP.
         03 PATTERN-LEVEL                    PIC 9(9) COMP.
         03 DETAIL-LEVEL                     PIC 9(9) COMP.
         03 SUB-LABEL                        PIC X(20).
         03 BUFFER-KEY                       PIC X(20).
         03 LABEL-KEY                        PIC X(20).
         03 PREV-LABEL                       PIC X(20).

         03 HEX-LEN-FMT-CONVERSION.
           05 HEX-LEN-X                      PIC X(4).
           05 HEX-LEN-B REDEFINES HEX-LEN-X
                                             PIC 9(9) COMP.
           05 HEX-LEN-N                      PIC 9(8).

       01 VALUE-AREA.
         03 VALUE-LEN                        PIC 9(9) COMP.
         03 VALUE-LEN-ZONED                  PIC 9(9).
         03 VALUE-DATA                       PIC X(999).

       01 BUILD-TLV-AREA.
         03 BUILD-TLV-STRING                 PIC X(999).
         03 BUILD-TLV-POS                    PIC 9(9) COMP.
         03 BUILD-LEN-START                  PIC 9(9) COMP.

       COPY X60I002I REPLACING ==:X:== BY ==N==.
       COPY X60I002O REPLACING ==:X:== BY ==N==.

      **
       LINKAGE SECTION.
       COPY X60I002I REPLACING ==:X:== BY ==C==.
       COPY X60I002O REPLACING ==:X:== BY ==C==.
       COPY X60MCR.

       PROCEDURE DIVISION USING TLV-C-IN
                                TLV-C-OUT
                                MR.

      *
       BEGIN.

DEB   *    DISPLAY '--- X60I002 - START ---'
DEB   *    PERFORM SHOW-INPUT.

           INITIALIZE BUILD-TLV-STRING.
           MOVE 1                        TO BUILD-TLV-POS.

      * formal check input
           PERFORM FORMAL-CHECK-DETAILS

           MOVE 1                        TO IDX
           PERFORM UNTIL IDX > TLV-C-DET-TOT
              PERFORM SET-TAG-ID
              PERFORM SET-TAG-VALUE  *> idx increment and recursive call
              PERFORM CALC-TAG-LEN   *> length depending on value
              PERFORM BUILD-TLV
           END-PERFORM.

           COMPUTE TLV-C-TEXT-LEN = BUILD-TLV-POS - 1
           MOVE BUILD-TLV-STRING (1 : TLV-C-TEXT-LEN)
                                         TO TLV-C-TEXT-DATA

DEB   *    DISPLAY '--- X60I002 - END ---'
           GOBACK.

      *
       FORMAL-CHECK-DETAILS.
      * coerenza tra dettaglio e formato:
      * se C2C2 allora ci deve essere un solo '-'
      * inspect detail-label tallyng '-' VS pattern length / 4 - 1
      *
      * non ci devono essere label duplicate

           COMPUTE PATTERN-LEVEL = (FUNCTION LENGTH(
                                    FUNCTION TRIM(TLV-C-PATTERN)
                                   ) / 4) - 1

           INITIALIZE PREV-LABEL
           PERFORM VARYING IDX FROM 1 BY 1
           UNTIL IDX > TLV-C-DET-TOT
              INITIALIZE DETAIL-LEVEL
              INSPECT TLV-C-DET-LABEL(IDX)
              TALLYING DETAIL-LEVEL
              FOR ALL '-'

              IF DETAIL-LEVEL NOT EQUAL PATTERN-LEVEL
      * detail not coherent with pattern
                 PERFORM RAISE-DETAIL-NOT-MATCH-FMT
              END-IF

              IF TLV-C-DET-LABEL(IDX) EQUAL PREV-LABEL
      * duplicate detail
                 PERFORM RAISE-DUPLICATE-DETAIL-ERR
              END-IF
              MOVE TLV-C-DET-LABEL(IDX)  TO PREV-LABEL
           END-PERFORM.

      *
       GET-SUB-LABEL.
           UNSTRING TLV-C-DET-LABEL(IDX) DELIMITED BY '-' OR SPACE
               INTO LABEL-KEY
           END-UNSTRING.

           PERFORM FIND-1ST-SUBELEMENT-POS

           MOVE TLV-C-DET-LABEL(IDX) (FIRST-SUBEL-POS:)
                                             TO SUB-LABEL.

      *
       FIND-1ST-SUBELEMENT-POS.
           INITIALIZE FIRST-DASH-POS
                      FIRST-SPACE-POS
                      FIRST-SUBEL-POS

           INSPECT TLV-C-DET-LABEL(IDX)
           TALLYING FIRST-DASH-POS  FOR CHARACTERS BEFORE INITIAL '-'
           INSPECT TLV-C-DET-LABEL(IDX)
           TALLYING FIRST-SPACE-POS FOR CHARACTERS BEFORE INITIAL SPACE

           IF FIRST-DASH-POS < FIRST-SPACE-POS
              COMPUTE FIRST-SUBEL-POS = FIRST-DASH-POS + 2
           ELSE
              MOVE 1                         TO FIRST-SUBEL-POS
           END-IF.

      *
       SET-TAG-VALUE.
           INITIALIZE VALUE-AREA.
           IF PATTERN-LEVEL = 0 *> caso base
              MOVE TLV-C-DET-VALUE-LEN (IDX)  TO VALUE-LEN
              MOVE TLV-C-DET-VALUE-DATA(IDX)  TO VALUE-DATA
              ADD 1                      TO IDX
           ELSE
              PERFORM RECURSIVE-CALL
           END-IF.

      *
       RECURSIVE-CALL.
           INITIALIZE TLV-N-PATTERN
                      TLV-N-DET-TOT

           MOVE TLV-C-INNER-PATTERN           TO TLV-N-PATTERN
           MOVE TLV-C-CONVERSION              TO TLV-N-CONVERSION

           ADD 1                              TO TLV-N-DET-TOT
           PERFORM GET-SUB-LABEL
           MOVE SUB-LABEL                     TO TLV-N-DET-LABEL
                                                 (TLV-N-DET-TOT)
           MOVE TLV-C-DET-VALUE (IDX)         TO TLV-N-DET-VALUE
                                                 (TLV-N-DET-TOT)
           PERFORM FILL-BUFFER.

           CALL PGM-NAME USING TLV-N-IN
                               TLV-N-OUT
                               MR.

           MOVE TLV-N-TEXT-LEN                TO VALUE-LEN.
           MOVE TLV-N-TEXT-DATA               TO VALUE-DATA.

      *
       FILL-BUFFER.
           INITIALIZE BUFFER-KEY
           MOVE LABEL-KEY                     TO BUFFER-KEY
           PERFORM UNTIL IDX > TLV-C-DET-TOT
           OR LABEL-KEY NOT EQUAL BUFFER-KEY
              ADD 1                      TO IDX

              IF IDX > TLV-C-DET-TOT
                 EXIT PERFORM
              END-IF

              PERFORM GET-SUB-LABEL
              IF LABEL-KEY EQUAL BUFFER-KEY
                 ADD 1                        TO TLV-N-DET-TOT
                 MOVE SUB-LABEL               TO TLV-N-DET-LABEL
                                                 (TLV-N-DET-TOT)
                 MOVE TLV-C-DET-VALUE (IDX)   TO TLV-N-DET-VALUE
                                                 (TLV-N-DET-TOT)
              END-IF
           END-PERFORM.

      *
       SET-TAG-ID.
           INITIALIZE TAG-ID

      *   adjust tag length id length
           MOVE T-C-LEN                 TO EFF-T-LEN

           IF T-C-FMT = CC-HEX
      * double the tag length for hex format tags
              MULTIPLY 2                BY EFF-T-LEN

              IF T-C-LEN = 1
                 IF TLV-C-INNER-PATTERN EQUAL SPACE  *> base case
                    PERFORM ADJUST-HEX-TAG-ID-LENGTH
                 END-IF
              END-IF
           END-IF.

           MOVE TLV-C-DET-LABEL(IDX) (1:EFF-T-LEN)   TO TAG-ID

      *   ensure tag id value
           IF  TLV-C-DET-LABEL(IDX) (EFF-T-LEN + 1:1) NOT EQUAL '-'
           AND TLV-C-DET-LABEL(IDX) (EFF-T-LEN + 1:1) NOT EQUAL SPACE
              PERFORM RAISE-WRONG-TAG-LENGTH-ERROR
           END-IF.

      *
       ADJUST-HEX-TAG-ID-LENGTH.
      * hexadecimal tags on a single bytes are extended
      * on 2 bytes for 'xF' tags where the rightmost bit
      * of the uppher nibble is ON
           IF TLV-C-DET-LABEL(IDX) (1:2) EQUAL '1F' OR '3F' OR '5F' OR
                                               '7F' OR '9F' OR 'BF' OR
                                               'DF' OR 'FF'
              ADD 2                     TO EFF-T-LEN
           END-IF.

      *
       CALC-TAG-LEN.
      *   check tag length aganst pattern
           IF T-C-FMT = CC-HEX
      *   256 ^ L-C-LEN -> maximum number expressed in
      *   L-C-LEN digits
              IF VALUE-LEN > ((256 ** L-C-LEN) - 1)
                 PERFORM RAISE-VALUE-TOO-LONG-ERROR
              END-IF
           ELSE
      *   10 ^ L-C-LEN - 1 -> maximum number expressed in
      *   L-C-LEN digits
              IF VALUE-LEN > ((10 ** L-C-LEN) - 1)
                 PERFORM RAISE-VALUE-TOO-LONG-ERROR
              END-IF
           END-IF.

           INITIALIZE TAG-LEN-N
           COMPUTE TAG-LEN-N = VALUE-LEN.

      *
       BUILD-TLV.
           PERFORM BUILD-TLV-TAG.
           PERFORM BUILD-TLV-LENGTH.
           PERFORM BUILD-TLV-VALUE.

      *
       BUILD-TLV-TAG.
           IF T-C-FMT = CC-HEX
              IF FUNCTION TRIM(TAG-ID) IS NOT HEX-VALID
                 PERFORM RAISE-INVALID-HEX-TAG-ERR
              END-IF

              STRING FUNCTION HEX-TO-CHAR(FUNCTION TRIM(TAG-ID))
                                              DELIMITED BY SIZE
                INTO BUILD-TLV-STRING
                POINTER BUILD-TLV-POS
              END-STRING
           ELSE
              STRING FUNCTION TRIM(TAG-ID)    DELIMITED BY SIZE
                INTO BUILD-TLV-STRING
                POINTER BUILD-TLV-POS
              END-STRING
           END-IF.

      *
       BUILD-TLV-LENGTH.
           IF TLV-C-CONV-FLAG EQUAL CC-HEX
              DIVIDE 2                  INTO TAG-LEN-N
           END-IF

           IF L-C-FMT = CC-HEX
              MOVE TAG-LEN-N                  TO HEX-LEN-B
              COMPUTE BUILD-LEN-START = LENGTH OF HEX-LEN-B -
                                        L-C-LEN + 1

              STRING HEX-LEN-X (BUILD-LEN-START : L-C-LEN)
                                                 DELIMITED BY SIZE
                INTO BUILD-TLV-STRING
                POINTER BUILD-TLV-POS
              END-STRING
           ELSE
              COMPUTE BUILD-LEN-START = LENGTH OF TAG-LEN-N -
                                        L-C-LEN + 1

              STRING TAG-LEN-X (BUILD-LEN-START : L-C-LEN)
                                                 DELIMITED BY SIZE
                INTO BUILD-TLV-STRING
                POINTER BUILD-TLV-POS
              END-STRING
           END-IF.

      *
       BUILD-TLV-VALUE.
           IF TLV-C-CONV-FLAG EQUAL CC-HEX
              IF FUNCTION TRIM(VALUE-DATA (1:VALUE-LEN))
              IS NOT HEX-VALID
                 PERFORM RAISE-INVALID-HEX-VALUE-ERR
              END-IF
              STRING FUNCTION HEX-TO-CHAR(VALUE-DATA (1:VALUE-LEN))
                                               DELIMITED BY SIZE
                INTO BUILD-TLV-STRING
                POINTER BUILD-TLV-POS
              END-STRING
           ELSE
              STRING VALUE-DATA (1:VALUE-LEN)  DELIMITED BY SIZE
                INTO BUILD-TLV-STRING
                POINTER BUILD-TLV-POS
              END-STRING
           END-IF.

      *
       RAISE-DETAIL-NOT-MATCH-FMT.
           MOVE 20                           TO MR-RESULT.
           STRING 'Detail label does not match pattern '
                  TLV-C-PATTERN              DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE TLV-C-DET-LABEL(IDX)         TO MR-POSITION.

           GOBACK.

      *
       RAISE-WRONG-TAG-LENGTH-ERROR.
           MOVE 21                           TO MR-RESULT.
           STRING 'tag id '                  DELIMITED BY SIZE
                  TAG-ID                     DELIMITED BY SIZE
                  ' wrong tag length. expected length: '
                                             DELIMITED BY SIZE
                  T-C-LEN                    DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE TAG-ID                       TO MR-POSITION.

           GOBACK.

      *
       RAISE-VALUE-TOO-LONG-ERROR.
           MOVE 22                           TO MR-RESULT.

           MOVE VALUE-LEN                    TO VALUE-LEN-ZONED
           STRING 'tag value length '        DELIMITED BY SIZE
                  VALUE-LEN-ZONED            DELIMITED BY SIZE
                  ' exceedes the maximum length allowed by '
                                             DELIMITED BY SIZE
                  L-C-LEN                    DELIMITED BY SIZE
                  ' digits in the pattern '  DELIMITED BY SIZE
                  TLV-C-PATTERN              DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE TAG-ID                       TO MR-POSITION.

           GOBACK.

      *
       RAISE-INVALID-HEX-TAG-ERR.
           MOVE 23                           TO MR-RESULT.

           MOVE VALUE-LEN                    TO VALUE-LEN-ZONED
           STRING 'hex conversion requested on invalid hex tag '
                                             DELIMITED BY SIZE
                  TAG-ID                     DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE TAG-ID                       TO MR-POSITION.

           GOBACK.

      *
       RAISE-INVALID-HEX-VALUE-ERR.
           MOVE 24                           TO MR-RESULT.

           MOVE VALUE-LEN                    TO VALUE-LEN-ZONED
           STRING 'hex conversion requested on invalid hex value '
                                             DELIMITED BY SIZE
                  ' exceedes the maximum length allowed by '
                                             DELIMITED BY SIZE
                  L-C-LEN                    DELIMITED BY SIZE
                  ' digits in the pattern '  DELIMITED BY SIZE
                  TLV-C-PATTERN              DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE TAG-ID                       TO MR-POSITION.

           GOBACK.

      *
       RAISE-DUPLICATE-DETAIL-ERR.
           MOVE 25                           TO MR-RESULT.
           MOVE 'Duplicate detail found'
                                             TO MR-DESCRIPTION.
           MOVE TLV-C-DET-LABEL(IDX)         TO MR-POSITION.

           GOBACK.

      *
       RAISE-CALL-ERROR.
           MOVE 26                           TO MR-RESULT.
           STRING 'CALL for program '        DELIMITED BY SIZE
                  PGM-NAME                   DELIMITED BY SIZE
                  ' raised an exception'     DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE TAG-ID                       TO MR-POSITION.

           GOBACK.

      *
       CHECK-CALL-RESULT.
           IF MR-RESULT NOT EQUAL ZERO
              MOVE MR-POSITION       TO PREV-ERR-POS
              STRING TAG-ID                  DELIMITED BY SPACE
                     '.'                     DELIMITED BY SIZE
                     PREV-ERR-POS            DELIMITED BY SPACE
                INTO MR-POSITION

             GOBACK
           END-IF.

      **************
DEB    SHOW-INPUT.
           DISPLAY '------------------------------------'
           DISPLAY 'INPUT PATTERN: ' TLV-C-PATTERN (1:4)
           DISPLAY 'INNER PATTERN: ' TLV-C-INNER-PATTERN
           MOVE 1                        TO IDX
           PERFORM UNTIL IDX > TLV-C-DET-TOT
              DISPLAY IDX ' ' TLV-C-DET-LABEL (IDX)
                      ' : ' TLV-C-DET-VALUE-DATA (IDX) (1: 30)
              ADD 1 TO IDX
           END-PERFORM.
           DISPLAY '------------------------------------'.
