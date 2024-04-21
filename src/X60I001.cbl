      CBL OPT(2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. X60I001.
       AUTHOR.     ALAIMO.
      *----------------------------------------------------------------
      * X60I001
      * **++ programma per effettuare inblock da messaggio ISO8583
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
         03 CC-DE-PRESENT           PIC X(1)   VALUE '1'.
         03 CC-FIXED                PIC X(1)   VALUE 'F'.
         03 CC-TLV                  PIC X(3)   VALUE 'TLV'.
         03 CC-SPC                  PIC X(3)   VALUE 'SPC'.
         03 CC-TLV-ROUTINE          PIC X(8)   VALUE 'X60_002'.

      **
       LOCAL-STORAGE SECTION.
      *
       01 DEB-VARS.
         05 DEB-MAX-LEN PIC 9(9) COMP.
      *
       01 LS-UTILS.
         03 IBLK-ROUTINE            PIC X(8).
         03 IDX                     PIC 9(9) COMP.
         03 IDX-N                   PIC 9(3).
         03 IDX-X REDEFINES IDX-N   PIC X(3).
         03 W-IDX                   PIC 9(9) COMP.
         03 S-IDX                   PIC 9(9) COMP.
         03 MSG-IDX                 PIC 9(9) COMP.
         03 ISOMSG-POS              PIC 9(9) COMP.
         03 PADDING-BYTES           PIC 9(9) COMP.
         03 VARIABLE-LENGTH         PIC 9(9) COMP.
         03 FOUND-DE-VALUE          PIC X(999).
         03 FOUND-DE-VALUE-LEN      PIC 9(9) COMP.
         03 PREV-ERR-POS            PIC X(50).
         03 DE-VAR-LEN-N            PIC 9(3).
         03 DE-VAR-LEN-X REDEFINES
            DE-VAR-LEN-N            PIC X(3).
         03 FIRST-SUBEL-POS         PIC 9(9) COMP.
         03 FIRST-DASH-POS          PIC 9(9) COMP.
         03 FIRST-SPACE-POS         PIC 9(9) COMP.
         03 VAR-LEN-START-POS       PIC 9(9) COMP.

      *
       01 MTI-AREA.
         03 MTI                     PIC X(4).

      *
       01 BITMAPS-AREA.
         03 ISO-MSG-BITMAP          PIC X(128).
         03 FILLER REDEFINES ISO-MSG-BITMAP.
           05 ISO-MSG-BITMAP1       PIC X(064).
           05 ISO-MSG-BITMAP2       PIC X(064).

      *
       01 BUILD-MESSAGE-AREA.
         03 BUILD-MSG-TEXT          PIC X(4096).
         03 BUILD-MSG-POS           PIC 9(9) COMP.

      *
       01 DETAIL-BASE-AREA.
         03 DETAIL-BASE.
           05 DETAIL-DE-X          PIC X(3).
           05 DETAIL-DE-N REDEFINES
              DETAIL-DE-X          PIC 9(3).

      *
      *01 DETAIL-BASE-KEY          PIC X(5).
       01 DETAIL-BASE-KEY          PIC X(3).

      *
       01 BUFFER-DE-AREA.
      *  03 BUFFER-BASE            PIC X(5).
         03 BUFFER-BASE            PIC X(3).
         03 FILLER REDEFINES BUFFER-BASE.
           05 BUFFER-DE-X          PIC X(3).
           05 BUFFER-DE-N REDEFINES
              BUFFER-DE-X          PIC 9(3).
         03 BUF-TABLE-AREA.
           05 BUF-TOT              PIC 9(9) COMP.
           05 BUF-LIST.
             10 BUF-ELEMENT OCCURS 0 TO 1000
                            DEPENDING ON BUF-TOT.
               15 BUF-LABEL        PIC X(20).
               15 BUF-VALUE.
                 20 BUF-VALUE-LEN PIC 9(9) COMP.
                 20 BUF-VALUE-DATA PIC X(999).

      *
       COPY X60MCSTR.

      *
       COPY X60I002I REPLACING ==:X:== BY ==C==.

      *
       COPY X60I002O REPLACING ==:X:== BY ==C==.

      *
       COPY X60MCSP.

      **
       LINKAGE SECTION.
       COPY X60MIO.
       COPY X60MCFMT.
       COPY X60MCP.
       COPY X60MCR.

       PROCEDURE DIVISION USING MIO
                                MIO-FMT
                                MP
                                MR.
      *
       BEGIN.

           MOVE ZERO                            TO MR-RESULT
           MOVE 1                               TO BUILD-MSG-POS

      * Set iso message version
           PERFORM SET-ISO-MSG-VERSION.

      * Apply format override depending on input parameters
           PERFORM APPLY-FORMAT-OVERRIDE

      * sort input elements
           SORT MIO-DETAIL ASCENDING KEY MIO-DETAIL-LABEL.

      * default MTI
           MOVE '0000'   TO MTI

      * default bitmap fields to be updated depending on data elements
      * added to the message
           MOVE ALL '0'                         TO ISO-MSG-BITMAP

           MOVE 1                        TO IDX
      * for TLV elements the process must retrieve all the details
      * so a buffered scrolling is needed
           PERFORM FILL-DETAIL-BUFFER
           PERFORM UNTIL IDX > MIO-DETAILS-TOT
              PERFORM PROCESS-DETAIL-BUFFER

              PERFORM FILL-DETAIL-BUFFER
           END-PERFORM.
           PERFORM PROCESS-DETAIL-BUFFER

      * set message_area with MTI, BITMAP, ÝDE001¨, build_area
           PERFORM SET-ISO-MESSAGE.

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
       SET-DETAIL-BASE.
           INITIALIZE DETAIL-BASE.

           UNSTRING MIO-DETAIL-LABEL (IDX) DELIMITED BY '-'
                                           OR SPACE
               INTO DETAIL-BASE
           END-UNSTRING.

      *
       FILL-DETAIL-BUFFER.
           PERFORM SET-DETAIL-BASE.
           MOVE DETAIL-BASE           TO DETAIL-BASE-KEY.
           INITIALIZE BUF-TOT.

           PERFORM UNTIL IDX > MIO-DETAILS-TOT
           OR DETAIL-BASE NOT EQUAL DETAIL-BASE-KEY
      * skip 0 length values
              IF MIO-DETAIL-VALUE-LEN (IDX) > ZERO
                 ADD 1                TO BUF-TOT

                 IF BUF-TOT EQUAL 1
                    MOVE DETAIL-BASE  TO BUFFER-BASE
                 END-IF

                 PERFORM FIND-1ST-SUBELEMENT-POS

                 MOVE MIO-DETAIL-LABEL(IDX) (FIRST-SUBEL-POS:)
                                                TO BUF-LABEL (BUF-TOT)

                 MOVE MIO-DETAIL-VALUE(IDX) TO BUF-VALUE (BUF-TOT)

              END-IF
              ADD 1                   TO IDX
              IF IDX <= MIO-DETAILS-TOT
                 PERFORM SET-DETAIL-BASE
              END-IF
           END-PERFORM.

      *
       FIND-1ST-SUBELEMENT-POS.
           INITIALIZE FIRST-DASH-POS
                      FIRST-SPACE-POS
                      FIRST-SUBEL-POS

           INSPECT MIO-DETAIL-LABEL(IDX)
           TALLYING FIRST-DASH-POS  FOR CHARACTERS BEFORE INITIAL '-'
           INSPECT MIO-DETAIL-LABEL(IDX)
           TALLYING FIRST-SPACE-POS FOR CHARACTERS BEFORE INITIAL SPACE

           IF FIRST-DASH-POS < FIRST-SPACE-POS
              COMPUTE FIRST-SUBEL-POS = FIRST-DASH-POS + 2
           ELSE
              MOVE 1                         TO FIRST-SUBEL-POS
           END-IF.

      *
       PROCESS-DETAIL-BUFFER.
      *   recognize data element in X60DCSTR structure
      *   if not recognized, skip
      *   details MTI, BITMAP, DE001 -MUST- needs dedicated processing
      *     MTI -> set MTI
      *     BITMAP -> SKIP
      *     DE001  -> SKIP
      *     DEnnn  -> PROCESS
           IF BUFFER-BASE EQUAL 'MTI'
              MOVE BUF-VALUE-DATA (1)      TO MTI
           ELSE
              SET DE-IDX           TO 1
              SEARCH ISO-MSG-DE VARYING DE-IDX
                 AT END CONTINUE
                 WHEN DE-DESCR(DE-IDX) EQUAL BUFFER-DE-X
                    IF DE-DESCR(DE-IDX) NOT EQUAL '001' *> skip DE001
                       PERFORM FORMAT-DATA-ELEMENT
                    END-IF
              END-SEARCH
           END-IF.

      *
       FORMAT-DATA-ELEMENT.
           IF DE-FMT(DE-IDX) EQUAL CC-FIXED
              PERFORM FORMAT-FIXED-DATA-ELEMENT
           ELSE
      * search for composed data elements
              SET FMT-IDX          TO 1
              SEARCH FMT-EL VARYING FMT-IDX
                 AT END
                    PERFORM FORMAT-VARIABLE-DATA-ELEMENT
                 WHEN FMT-DE(FMT-IDX) EQUAL BUFFER-DE-N
                    PERFORM FORMAT-COMPLEX-DATA-ELEMENT
              END-SEARCH
           END-IF.

      * set bitmap element
           MOVE '1'         TO ISO-MSG-BITMAP(BUFFER-DE-N:1).

      *
       FORMAT-FIXED-DATA-ELEMENT.
      * only 1 element is expected in buffer
           IF BUF-TOT > 1
              PERFORM RAISE-MORE-THAN-1-DETAIL-ERR
           END-IF.

      * value too long, raise error
           IF BUF-VALUE-LEN (1) > DE-LENGTH(DE-IDX)
              PERFORM RAISE-VALUE-TOO-LONG-ERROR
           END-IF.

      * set data element value
           STRING BUF-VALUE-DATA (1)   (1:BUF-VALUE-LEN (1))
                                       DELIMITED BY SIZE
             INTO BUILD-MSG-TEXT
             POINTER BUILD-MSG-POS
           END-STRING.

      * padding SPACE
           COMPUTE PADDING-BYTES = DE-LENGTH(DE-IDX) -
                                   BUF-VALUE-LEN (1)
           PERFORM PADDING-BYTES TIMES
              STRING ' '               DELIMITED BY SIZE
                INTO BUILD-MSG-TEXT
                POINTER BUILD-MSG-POS
              END-STRING
           END-PERFORM.

      *
       FORMAT-VARIABLE-DATA-ELEMENT.
      *    only 1 element is expected in buffer
           IF BUF-TOT > 1
              PERFORM RAISE-MORE-THAN-1-DETAIL-ERR
           END-IF.

      * value exceedes maximum possible length
      *   10 ^ DE-LENGTH(IDX) -1: maximum number expressed in
      *   DE-LENGTH(IDX) digits
           IF BUF-VALUE-LEN (1) > ((10 ** DE-LENGTH(DE-IDX)) - 1)
              PERFORM RAISE-VALUE-TOO-LONG-ERROR
           END-IF.

      * set data element length
           MOVE BUF-VALUE-LEN (1)           TO DE-VAR-LEN-N
           PERFORM SET-DATA-ELEMENT-LENGTH

      * set data element value
           STRING BUF-VALUE-DATA (1)   (1:BUF-VALUE-LEN (1))
                                       DELIMITED BY SIZE
             INTO BUILD-MSG-TEXT
             POINTER BUILD-MSG-POS
           END-STRING.

      *
       FORMAT-COMPLEX-DATA-ELEMENT.
      * pass entire buffer to X60I002 recursive program
      * to build a TLV format depending on specification in X60MCFMT
      * OR
      * pass entire buffer to 'dedicated inblock program'
      * to build dataelement depending on specification in X60MCFMT

           MOVE FMT-PATTERN(FMT-IDX)        TO TLV-C-PATTERN
           MOVE FMT-OUT-CONV(FMT-IDX)       TO TLV-C-CONVERSION
           MOVE BUF-TABLE-AREA              TO TLV-C-DETAILS

           IF FMT-TYPE(FMT-IDX) EQUAL CC-TLV
              MOVE CC-TLV-ROUTINE           TO INBLOCK-ROUTINE
           ELSE
              MOVE FMT-DEDICATED-PGM(FMT-IDX) TO INBLOCK-ROUTINE
           END-IF

           SET INBLOCK-BEHAVIOR           TO TRUE
           CALL INBLOCK-ROUTINE USING TLV-C-IN TLV-C-OUT MR
                    ON EXCEPTION PERFORM RAISE-CALL-ERROR
                NOT ON EXCEPTION PERFORM CHECK-CALL-RESULT
           END-CALL

      * value exceedes maximum possible length
      *   10 ¬ DE-LENGTH(IDX) -1: maximum number expressed in
      *   DE-LENGTH(IDX) digits
           IF TLV-C-TEXT-LEN  > ((10 ** DE-LENGTH(IDX)) - 1)
              PERFORM RAISE-VALUE-TOO-LONG-ERROR
           END-IF.

      * set data element length
           MOVE TLV-C-TEXT-LEN              TO DE-VAR-LEN-N
           PERFORM SET-DATA-ELEMENT-LENGTH

      * set data element value
           STRING TLV-C-TEXT-DATA    (1:TLV-C-TEXT-LEN)
                                       DELIMITED BY SIZE
             INTO BUILD-MSG-TEXT
             POINTER BUILD-MSG-POS
           END-STRING.

      *
       SET-DATA-ELEMENT-LENGTH.
           COMPUTE VAR-LEN-START-POS = LENGTH OF DE-VAR-LEN-N -
                                       DE-LENGTH(DE-IDX) + 1

           STRING DE-VAR-LEN-N (VAR-LEN-START-POS : DE-LENGTH(DE-IDX))
                                       DELIMITED BY SIZE
             INTO BUILD-MSG-TEXT
             POINTER BUILD-MSG-POS
           END-STRING.

      *
       SET-ISO-MESSAGE.
      * set DE001 bit if needed
           IF ISO-MSG-BITMAP2 NOT EQUAL ALL ZERO
              MOVE '1'         TO ISO-MSG-BITMAP(1:1)
           END-IF.

           MOVE 1 TO ISOMSG-POS.

           PERFORM SET-ISO-MESSAGE-BITMAP.

           IF ISO-MSG-BITMAP2 NOT EQUAL ALL ZERO
              PERFORM SET-ISO-MESSAGE-DE001
           END-IF.

           PERFORM SET-ISO-MESSAGE-STRING.

      *
       SET-ISO-MESSAGE-BITMAP.
           STRING MTI                             DELIMITED BY SIZE
                  FUNCTION BIT-TO-CHAR(ISO-MSG-BITMAP1)
                                                  DELIMITED BY SIZE
             INTO MIO-ISO-MESSAGE
             POINTER ISOMSG-POS
           END-STRING.

      *
       SET-ISO-MESSAGE-DE001.
           STRING FUNCTION BIT-TO-CHAR(ISO-MSG-BITMAP2)
                                               DELIMITED BY SIZE
             INTO MIO-ISO-MESSAGE
             POINTER ISOMSG-POS
           END-STRING.

      *
       SET-ISO-MESSAGE-STRING.
           SUBTRACT 1                       FROM BUILD-MSG-POS
           STRING BUILD-MSG-TEXT(1:BUILD-MSG-POS) DELIMITED BY SIZE
             INTO MIO-ISO-MESSAGE
             POINTER ISOMSG-POS
           END-STRING.

      *
       CHECK-CALL-RESULT.
           IF MR-RESULT NOT EQUAL ZERO
              MOVE MR-POSITION          TO PREV-ERR-POS
              STRING BUFFER-BASE        DELIMITED BY SIZE
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
       RAISE-MORE-THAN-1-DETAIL-ERR.
           MOVE 12                           TO MR-RESULT.
           MOVE 'More than 1 detail for simple data element'
                                             TO MR-DESCRIPTION.
           MOVE BUFFER-BASE                  TO MR-POSITION.

           GOBACK.

      *
       RAISE-VALUE-TOO-LONG-ERROR.
           MOVE 13                           TO MR-RESULT.
           MOVE 'Input value too long for data element'
                                             TO MR-DESCRIPTION.
           MOVE BUFFER-BASE                  TO MR-POSITION.

           GOBACK.

      * --- RUNTIME ERRORS ---
       RAISE-CALL-ERROR.
           MOVE 15                           TO MR-RESULT.
           STRING 'CALL for program '        DELIMITED BY SIZE
                  INBLOCK-ROUTINE            DELIMITED BY SIZE
                  ' raised an exception'     DELIMITED BY SIZE
             INTO MR-DESCRIPTION.
           MOVE BUFFER-BASE                  TO MR-POSITION.

           GOBACK.

