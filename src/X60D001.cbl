      CBL OPT(2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. X60D001.
       AUTHOR.     ALAIMO.
      *----------------------------------------------------------------
      * X60D001
      * **++ programma per effettuare deblock da messaggio ISO8583
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
      **
       FILE SECTION.
      **
       WORKING-STORAGE SECTION.

       01 WK-LITERALS.
         03 CC-DE-PRESENT           PIC X(1)   VALUE '1'.
         03 CC-FIXED                PIC X(1)   VALUE 'F'.
         03 CC-HEX                  PIC X(1)   VALUE 'H'.
         03 CC-TLV                  PIC X(3)   VALUE 'TLV'.
         03 CC-SPC                  PIC X(3)   VALUE 'SPC'.
         03 CC-TLV-ROUTINE          PIC X(8)   VALUE 'X60_002'.

      *
       COPY X60MCSTR.

      **
       LOCAL-STORAGE SECTION.
       01 LS-UTILS.

         03 DBLK-ROUTINE            PIC X(8)      VALUE SPACE.
         03 FILLER REDEFINES DBLK-ROUTINE.
           05 FILLER                PIC X(3).
           05 FILLER                PIC X(3).
         03 IDX                     PIC 9(9) COMP VALUE ZERO.
         03 IDX-N                   PIC 9(3)      VALUE ZERO.
         03 IDX-X REDEFINES IDX-N   PIC X(3).
         03 W-IDX                   PIC 9(9) COMP VALUE ZERO.
         03 MSG-IDX                 PIC 9(9) COMP VALUE ZERO.
         03 VARIABLE-LENGTH         PIC 9(9) COMP VALUE ZERO.
         03 PREV-ERR-POS            PIC X(50)     VALUE SPACE.
         03 FOUND-DE-AREA.
           05 FOUND-DE-VALUE        PIC X(999)    VALUE SPACE.
           05 FOUND-DE-VALUE-LEN    PIC 9(9) COMP VALUE ZERO.

      *
       01 BITMAPS-AREA.
         03 ISO-MSG-BITMAP          PIC X(128)    VALUE SPACE.

      *
       COPY X60MCSP.

      *
       COPY X60D002I REPLACING ==:X:== BY ==C==.

      *
       COPY X60D002O REPLACING ==:X:== BY ==C==.

      **
       LINKAGE SECTION.
       COPY X60MIO.
       COPY X60MCFMT.
       COPY X60MCP.
       COPY X60MCR.

      **
       PROCEDURE DIVISION USING MIO
                                MIO-FMT
                                MP
                                MR.
      *
       BEGIN.

           MOVE ZERO                            TO MR-RESULT
           MOVE ZERO                            TO MIO-DETAILS-TOT

      * Set iso message version
           PERFORM SET-ISO-MSG-VERSION.

      * Set bitmap to know which data element is present in the message
           PERFORM SET-ISO-MSG-BITMAP.

      * Apply format override depending on input parameters
           PERFORM APPLY-FORMAT-OVERRIDE.

      * Deblock ISO message
           PERFORM DEBLOCK-ISO-MSG.

           GOBACK.

      *
       SET-ISO-MSG-VERSION.
           EVALUATE TRUE
              WHEN MP-VERSION-87
                 MOVE ISO-MSG-V87               TO ISO-MSG-STRUCT
      *       WHEN MP-VERSION-93
      *          MOVE ISO-MSG-V93               TO ISO-MSG-STRUCT
              WHEN OTHER
                 PERFORM RAISE-INVALID-VERSION
           END-EVALUATE.

      *
       SET-ISO-MSG-BITMAP.
           MOVE ALL ZERO                        TO ISO-MSG-BITMAP.
      * 1st bitmap
           MOVE FUNCTION BIT-OF(MIO-ISO-MESSAGE(5:8))
                                                TO ISO-MSG-BITMAP(1:64).

      * 2nd bitmap if present
           IF ISO-MSG-BITMAP(1:1) = CC-DE-PRESENT
              MOVE FUNCTION BIT-OF(MIO-ISO-MESSAGE(13:8))
                                                TO ISO-MSG-BITMAP(65:)
           END-IF.

      *
       APPLY-FORMAT-OVERRIDE.
      * For each rule, override the relative record in FMT structure.
      * If the record is not found, the add it
           PERFORM VARYING W-IDX FROM 1 BY 1
           UNTIL W-IDX > MP-OR-TOT
              PERFORM VARYING FMT-IDX FROM 1 BY 1
              UNTIL FMT-IDX > FMT-TOT
              OR FMT-DE (FMT-IDX) EQUAL MP-OR-DE (W-IDX)
              END-PERFORM

      * rule not found, add a new rule slot
      * in this case FMT IDX is already placed at the new slot
              IF FMT-IDX > FMT-TOT
                 ADD 1                  TO FMT-TOT
              END-IF

              MOVE MP-OR-EL (W-IDX)
                                        TO FMT-EL (FMT-IDX)
           END-PERFORM.

      *
       DEBLOCK-ISO-MSG.

           PERFORM APPEND-MTI-AND-BITMAP


           MOVE 13                              TO MSG-IDX.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 128
              IF ISO-MSG-BITMAP(IDX:1)  EQUAL CC-DE-PRESENT

                 IF DE-FMT (IDX) EQUAL CC-FIXED
                    PERFORM DEBLOCK-FIXED-DE
                 ELSE
                    PERFORM DEBLOCK-VARIABLE-DE
                 END-IF

                 PERFORM WRITE-DATA-ELEMENT

              END-IF
           END-PERFORM.

      *
       WRITE-DATA-ELEMENT.
      * if data element is subdefined then deblock all the subfields
           PERFORM GET-DE-FORMAT.

           IF FMT-IDX > FMT-TOT   *> not found
              PERFORM APPEND-SIMPLE-DE
           ELSE
              IF FMT-TYPE (FMT-IDX) = SPACE *> not subdefined
                 PERFORM APPEND-SIMPLE-DE
              ELSE
                 PERFORM DEBLOCK-SUBFIELDS
              END-IF
           END-IF.

      *
       DEBLOCK-FIXED-DE.

           MOVE DE-LENGTH (IDX)             TO FOUND-DE-VALUE-LEN.
           IF DE-LENGTH (IDX) EQUAL ZERO
              PERFORM RAISE-ZERO-LENGTH-ERROR
           END-IF
           MOVE MIO-ISO-MESSAGE(MSG-IDX:DE-LENGTH (IDX))
                                            TO FOUND-DE-VALUE.

           ADD DE-LENGTH (IDX)              TO MSG-IDX.

      *
       DEBLOCK-VARIABLE-DE.

           IF DE-LENGTH (IDX) EQUAL ZERO
              PERFORM RAISE-ZERO-LENGTH-ERROR
           END-IF
           IF MIO-ISO-MESSAGE (MSG-IDX:DE-LENGTH (IDX)) IS NUMVAL-VALID
              COMPUTE VARIABLE-LENGTH = FUNCTION NUMVAL(MIO-ISO-MESSAGE
                                     (MSG-IDX:DE-LENGTH (IDX)))
           ELSE
              PERFORM RAISE-NUMVAL-ERROR
           END-IF.

           ADD DE-LENGTH (IDX)              TO MSG-IDX.

           MOVE VARIABLE-LENGTH             TO FOUND-DE-VALUE-LEN.
           IF VARIABLE-LENGTH EQUAL ZERO
              PERFORM RAISE-ZERO-LENGTH-ERROR
           END-IF
           MOVE MIO-ISO-MESSAGE(MSG-IDX:VARIABLE-LENGTH)
                                            TO FOUND-DE-VALUE.

           ADD VARIABLE-LENGTH              TO MSG-IDX.

      *
       GET-DE-FORMAT.
           PERFORM VARYING FMT-IDX FROM 1 BY 1
           UNTIL FMT-IDX > FMT-TOT
           OR FMT-DE (FMT-IDX) EQUAL IDX
           END-PERFORM.

      *
       APPEND-MTI-AND-BITMAP.
      * mti
           ADD 1                       TO MIO-DETAILS-TOT.
           MOVE 'MTI'                  TO MIO-DETAIL-LABEL
                                          (MIO-DETAILS-TOT).

           MOVE 4                      TO MIO-DETAIL-VALUE-LEN
                                        (MIO-DETAILS-TOT).
           MOVE MIO-ISO-MESSAGE(1 : 4) TO MIO-DETAIL-VALUE-DATA
                                        (MIO-DETAILS-TOT).
      * bitmap
           ADD 1                       TO MIO-DETAILS-TOT.
           MOVE 'BITMAP'               TO MIO-DETAIL-LABEL
                                        (MIO-DETAILS-TOT).

           MOVE 8                      TO MIO-DETAIL-VALUE-LEN
                                        (MIO-DETAILS-TOT).
           MOVE MIO-ISO-MESSAGE(5 : 8) TO MIO-DETAIL-VALUE-DATA
                                        (MIO-DETAILS-TOT).

       APPEND-SIMPLE-DE.
           ADD 1                     TO MIO-DETAILS-TOT.
           MOVE IDX                  TO IDX-N
           MOVE IDX-X                TO MIO-DETAIL-LABEL
                                        (MIO-DETAILS-TOT).

           IF FMT-OUT-CONV (FMT-IDX) EQUAL CC-HEX
              MULTIPLY 2 BY FOUND-DE-VALUE-LEN
              MOVE FUNCTION HEX-OF(FOUND-DE-VALUE)
                                     TO FOUND-DE-VALUE
           END-IF.

           MOVE FOUND-DE-VALUE-LEN   TO MIO-DETAIL-VALUE-LEN
                                        (MIO-DETAILS-TOT).
           MOVE FOUND-DE-VALUE       TO MIO-DETAIL-VALUE-DATA
                                        (MIO-DETAILS-TOT).

      *
       DEBLOCK-SUBFIELDS.

           EVALUATE FMT-TYPE (FMT-IDX)
              WHEN CC-TLV
                 PERFORM SET-DEBLOCK-ROUTINE-INPUT
                 MOVE CC-TLV-ROUTINE      TO DEBLOCK-ROUTINE
              WHEN CC-SPC
                 PERFORM SET-DEBLOCK-ROUTINE-INPUT
                 MOVE FMT-DEDICATED-PGM (FMT-IDX)
                                          TO DEBLOCK-ROUTINE
              WHEN OTHER
                 PERFORM RAISE-INVALID-FORMAT-TYPE
           END-EVALUATE.

           SET DEBLOCK-BEHAVIOR           TO TRUE
           CALL DEBLOCK-ROUTINE USING TLV-C-IN TLV-C-OUT MR
                    ON EXCEPTION PERFORM RAISE-CALL-ERROR
                NOT ON EXCEPTION PERFORM CHECK-CALL-RESULT
           END-CALL
           PERFORM GET-DEBLOCK-ROUTINE-OUTPUT.

      *
       SET-DEBLOCK-ROUTINE-INPUT.
           MOVE FOUND-DE-VALUE-LEN        TO TLV-C-TEXT-LEN.
           MOVE FOUND-DE-VALUE            TO TLV-C-TEXT-DATA.
           MOVE FMT-OUT-CONV (FMT-IDX)    TO TLV-C-CONV-FLAG.
           MOVE FMT-PATTERN (FMT-IDX)     TO TLV-C-PATTERN.
           MOVE ZERO                      TO LIST-C-TOT.

      *
       GET-DEBLOCK-ROUTINE-OUTPUT.
           PERFORM VARYING W-IDX FROM 1 BY 1
           UNTIL W-IDX > LIST-C-TOT
              PERFORM APPEND-SUBELEMENT
           END-PERFORM.

      *
       APPEND-SUBELEMENT.
           ADD 1                     TO MIO-DETAILS-TOT.
           INITIALIZE MIO-DETAIL-LABEL (MIO-DETAILS-TOT)
           MOVE IDX                  TO IDX-N
           STRING IDX-X    DELIMITED BY SIZE
                  '-'      DELIMITED BY SIZE
                  FUNCTION TRIM(EL-C-TAG-ID (W-IDX))
                           DELIMITED BY SIZE
             INTO MIO-DETAIL-LABEL (MIO-DETAILS-TOT).
           MOVE EL-C-TAG-VALUE-LEN (W-IDX)
                                     TO MIO-DETAIL-VALUE-LEN
                                        (MIO-DETAILS-TOT).
           MOVE EL-C-TAG-VALUE-DATA (W-IDX)
                                     TO MIO-DETAIL-VALUE-DATA
                                        (MIO-DETAILS-TOT).

      *
       CHECK-CALL-RESULT.
           IF MR-RESULT NOT EQUAL ZERO
              MOVE MR-POSITION          TO PREV-ERR-POS
              MOVE IDX                  TO IDX-N
              STRING IDX-X              DELIMITED BY SIZE
                     '.'                DELIMITED BY SIZE
                     PREV-ERR-POS       DELIMITED BY SPACE
                INTO MR-POSITION

              GOBACK
           END-IF.

      *
      * --- INPUT ERRORS ---
       RAISE-INVALID-VERSION.
           MOVE 10                           TO MR-RESULT.
           STRING 'Invalid version requested: ' DELIMITED BY SIZE
                  MP-VERSION                    DELIMITED BY SIZE
             INTO MR-DESCRIPTION.

           GOBACK.

      *
       RAISE-INVALID-FORMAT-TYPE.
           MOVE 12                           TO MR-RESULT.
           STRING 'Invalid format type: '      DELIMITED BY SIZE
                  FMT-TYPE (FMT-IDX)           DELIMITED BY SIZE
             INTO MR-DESCRIPTION.

           GOBACK.

      *
      * --- RUNTIME ERRORS ---
       RAISE-CALL-ERROR.
           MOVE 13                           TO MR-RESULT.
           STRING 'CALL for program '        DELIMITED BY SIZE
                  DBLK-ROUTINE               DELIMITED BY SIZE
                  ' raised an exception'     DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE IDX                          TO IDX-N.
           MOVE IDX-X                        TO MR-POSITION.

           GOBACK.

      *
       RAISE-NUMVAL-ERROR.
           MOVE 14                           TO MR-RESULT.
           MOVE 'DE length is NOT numeric'   TO MR-DESCRIPTION.
           MOVE IDX                          TO IDX-N.
           MOVE IDX-X                        TO MR-POSITION.

           GOBACK.

      *
       RAISE-ZERO-LENGTH-ERROR.
           MOVE 15                           TO MR-RESULT.
           MOVE 'ZERO length error for subscrupted MOVE'
                                             TO MR-DESCRIPTION.
           MOVE IDX                          TO IDX-N.
           MOVE IDX-X                        TO MR-POSITION.

           GOBACK.

