      CBL OPT(2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. X60D002 RECURSIVE.
       AUTHOR.     ALAIMO.
      *----------------------------------------------------------------
      * X60D002
      * **++ routine ricorsiva per deblock di un campo TLV.
      *
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-370.
       OBJECT-COMPUTER.    IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
      * class to check NUMVAL input
           CLASS NUMVAL-VALID IS '0' THRU '9', SPACE.
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
         03 PGM-NAME                         PIC X(8) VALUE 'X60D002'.
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
         03 T-BUCKET                         PIC X(20) VALUE SPACE.
         03 L-BUCKET                         PIC X(20).
         03 V-BUCKET                         PIC X(999).

         03 HEX-LEN-FMT-CONVERSION.
           05 HEX-LEN-X                      PIC X(4).
           05 HEX-LEN-B REDEFINES HEX-LEN-X
                                             PIC 9(9) COMP.
           05 HEX-LEN-N                      PIC 9(8).

       COPY X60D002I REPLACING ==:X:== BY ==N==.
       COPY X60D002O REPLACING ==:X:== BY ==N==.

      **
       LINKAGE SECTION.
       COPY X60D002I REPLACING ==:X:== BY ==C==.
       COPY X60D002O REPLACING ==:X:== BY ==C==.
       COPY X60MCR.

       PROCEDURE DIVISION USING TLV-C-IN
                                TLV-C-OUT
                                MR.

      *
       BEGIN.
           MOVE 1                        TO IDX.

           PERFORM UNTIL IDX > TLV-C-TEXT-LEN
              PERFORM PARSE-TAG-ID
              PERFORM PARSE-VALUE-LENGTH
      * skip tags with length 0
              IF EFF-V-LEN NOT EQUAL ZERO
                 PERFORM PARSE-VALUE-DATA

                 IF TLV-C-INNER-PATTERN NOT EQUAL SPACE
                    PERFORM RECURSIVE-CALL
                 ELSE
                    PERFORM APPEND-TAG
                 END-IF
              END-IF


           END-PERFORM.

           GOBACK.

      *
       PARSE-TAG-ID.
           INITIALIZE T-BUCKET.
           MOVE T-C-LEN                           TO EFF-T-LEN
           IF EFF-T-LEN EQUAL ZERO
              PERFORM RAISE-ZERO-LENGTH-ERROR
           END-IF

           IF T-C-FMT = CC-HEX
              IF T-C-LEN = 1
                 IF TLV-C-INNER-PATTERN EQUAL SPACE  *> base case
                    PERFORM ADJUST-HEX-TAG-ID-LENGTH
                 END-IF

              END-IF
      * hexadecimal tags are showed as plan hex
              MOVE FUNCTION HEX-OF(TLV-C-TEXT-DATA(IDX : EFF-T-LEN))
                                                  TO T-BUCKET
           ELSE
              MOVE TLV-C-TEXT-DATA(IDX:EFF-T-LEN) TO T-BUCKET
           END-IF.

           ADD EFF-T-LEN                          TO IDX.

      *
       ADJUST-HEX-TAG-ID-LENGTH.
      * hexadecimal tags on a single bytes are extended
      * on 2 bytes for 'xF' tags where the rightmost bit
      * of the uppher nibble is ON
           MOVE FUNCTION BIT-OF(TLV-C-TEXT-DATA(IDX : 1))
                                                  TO BITS.

           IF        (BIT-1 EQUAL '1' AND   *> it's an 'xF' tag
                BIT-2 EQUAL '1' AND
                BIT-3 EQUAL '1' AND
                BIT-4 EQUAL '1' )
           AND BIT-5 EQUAL '1' *> rightmost bit of upper nibble ON
              ADD 1                               TO EFF-T-LEN
           END-IF.

      *
       PARSE-VALUE-LENGTH.
           IF L-C-LEN EQUAL ZERO
              PERFORM RAISE-ZERO-LENGTH-ERROR
           END-IF
           IF L-C-FMT = CC-HEX
              MOVE TLV-C-TEXT-DATA(IDX : L-C-LEN) TO HEX-LEN-X
      * Shift to right:
      * divide hex number by 16 to the power of
      * number of digits to be shifted
              COMPUTE HEX-LEN-N = HEX-LEN-B / (16 **
                          ((LENGTH OF HEX-LEN-X - L-C-LEN) * 2))

              MOVE HEX-LEN-N                      TO L-BUCKET


           ELSE
              MOVE TLV-C-TEXT-DATA(IDX : L-C-LEN) TO L-BUCKET
           END-IF.
           ADD L-C-LEN                            TO IDX.

           IF L-BUCKET IS NUMVAL-VALID
              COMPUTE EFF-V-LEN = FUNCTION NUMVAL(L-BUCKET)
           ELSE
              PERFORM RAISE-NUMVAL-ERROR
           END-IF.

      *
       PARSE-VALUE-DATA.
           IF EFF-V-LEN EQUAL ZERO
              PERFORM RAISE-ZERO-LENGTH-ERROR
           END-IF
           MOVE TLV-C-TEXT-DATA(IDX : EFF-V-LEN)  TO V-BUCKET.
           ADD EFF-V-LEN                          TO IDX.

      *
       APPEND-TAG.
           ADD 1                TO LIST-C-TOT
           INITIALIZE              EL-C-TAG-ID         (LIST-C-TOT).
           MOVE T-BUCKET        TO EL-C-TAG-ID         (LIST-C-TOT).

           IF TLV-C-CONV-FLAG EQUAL CC-HEX
              MULTIPLY 2 BY EFF-V-LEN
              MOVE FUNCTION HEX-OF (V-BUCKET)     TO V-BUCKET
           END-IF.

           MOVE EFF-V-LEN       TO EL-C-TAG-VALUE-LEN  (LIST-C-TOT).
           MOVE V-BUCKET        TO EL-C-TAG-VALUE-DATA (LIST-C-TOT).

      *
       RECURSIVE-CALL.
           MOVE EFF-V-LEN              TO TLV-N-TEXT-LEN.
           MOVE V-BUCKET               TO TLV-N-TEXT-DATA.
           MOVE TLV-C-CONVERSION       TO TLV-N-CONVERSION.
           MOVE TLV-C-INNER-PATTERN    TO TLV-N-PATTERN.
           MOVE ZERO                   TO LIST-N-TOT.
           CALL PGM-NAME USING TLV-N-IN TLV-N-OUT MR
                    ON EXCEPTION PERFORM RAISE-CALL-ERROR
                NOT ON EXCEPTION PERFORM CHECK-CALL-RESULT
           END-CALL

           PERFORM VARYING W-IDX FROM 1 BY 1
           UNTIL W-IDX > LIST-N-TOT
              PERFORM APPEND-SUBTAG
           END-PERFORM.

      *
       APPEND-SUBTAG.
           ADD 1                TO LIST-C-TOT
           INITIALIZE EL-C-TAG-ID (LIST-C-TOT)
           STRING FUNCTION TRIM(T-BUCKET) DELIMITED BY SIZE
                  '-'                     DELIMITED BY SIZE
                  FUNCTION TRIM(EL-N-TAG-ID (W-IDX))
                                          DELIMITED BY SIZE
             INTO EL-C-TAG-ID (LIST-C-TOT).
           MOVE EL-N-TAG-VALUE-LEN (W-IDX)
                                TO EL-C-TAG-VALUE-LEN  (LIST-C-TOT).
           MOVE EL-N-TAG-VALUE-DATA (W-IDX)
                                TO EL-C-TAG-VALUE-DATA (LIST-C-TOT).

      *
      * --- RUNTIME ERRORS ---
       RAISE-CALL-ERROR.
           MOVE 20                           TO MR-RESULT.
           STRING 'CALL for program '        DELIMITED BY SIZE
                  PGM-NAME                   DELIMITED BY SIZE
                  ' raised an exception'     DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE T-BUCKET                     TO MR-POSITION.

           GOBACK.

      *
       RAISE-NUMVAL-ERROR.
           MOVE 21                           TO MR-RESULT.
           MOVE 'tag length is NOT numeric'  TO MR-DESCRIPTION.
           MOVE T-BUCKET                     TO MR-POSITION.

           GOBACK.

      *
       RAISE-ZERO-LENGTH-ERROR.
           MOVE 22                           TO MR-RESULT.
           MOVE 'ZERO length error for subscrupted MOVE'
                                             TO MR-DESCRIPTION.
           MOVE T-BUCKET                     TO MR-POSITION.

           GOBACK.

      *
       CHECK-CALL-RESULT.
           IF MR-RESULT NOT EQUAL ZERO
              MOVE MR-POSITION       TO PREV-ERR-POS
              STRING T-BUCKET        DELIMITED BY SPACE
                     '.'             DELIMITED BY SIZE
                     PREV-ERR-POS    DELIMITED BY SPACE
                INTO MR-POSITION

             GOBACK
           END-IF.

