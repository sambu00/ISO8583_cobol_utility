      CBL OPT(2) DYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. X60DTS01.
       AUTHOR.     ALAIMO.
      *----------------------------------------------------------------
      * X60DTS01 - Test suite X60D001
      * MI2457.PSPS.ISO8583.XUNIT
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-370.
       OBJECT-COMPUTER.    IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TCIN                      ASSIGN TO TCIN
                                            FILE STATUS TCIN-FS.
      **
       DATA DIVISION.
      *
       FILE SECTION.
       FD TCIN                      RECORDING F.
       01 TCIN-REC.
         03 TCIN-PARMS.
           05 TCIN-DESCRIPTION      PIC X(50).
           05 FILLER                PIC X.
           05 TCIN-ISO8583-VERSION  PIC X(02).
             88 TCIN-ISO8583-VERSION-87  VALUE '87'.
             88 TCIN-ISO8583-VERSION-93  VALUE '93'.
           05 FILLER                PIC X.
           05 TCIN-ISO8583-TCT      PIC X(06).
             88 TCIN-ISO8583-TCT-INCAS   VALUE 'X54088' 'X54090'.
             88 TCIN-ISO8583-TCT-STRATUS VALUE 'X54089'.
           05 FILLER                PIC X.
           05 TCIN-EXPECTED-CHECK   PIC X.
             88 TCIN-EXPECTED-EQUAL      VALUE '='.
             88 TCIN-EXPECTED-NOT-EQUAL  VALUE '!'.
           05 TCIN-EXPECTED         PIC 9(04).
           05 FILLER                PIC X.
         03 TCIN-ISO-MSG            PIC X(4096).
         03 FILLER                  PIC X.
         03 TCIN-FMT-REC-OVERRIDE   PIC X(30).
         03 FILLER                  PIC X(806).

      *
       WORKING-STORAGE SECTION.
       01 CC-ISO-DEBLOCKER          PIC X(08) VALUE 'X60D001'.

       COPY X60MIO.
       COPY X60MCP.
       COPY X60MCR.
       COPY X60MCFMT.
       COPY X60MCF01.
       COPY X60MCF02.

      *
       LOCAL-STORAGE SECTION.
       01 LS-TEST-CASE-SWITCH       PIC X.
         88 TEST-CASE-PASSED          VALUE 'P'.
         88 TEST-CASE-FAILED          VALUE 'F'.
      *
       01 LS-COUNTERS.
         03 TEST-CASE-CTR           PIC S9(9) COMP VALUE ZERO.
         03 TEST-CASE-PASSED-CTR    PIC S9(9) COMP VALUE ZERO.
         03 TEST-CASE-FAILED-CTR    PIC S9(9) COMP VALUE ZERO.
      *
       01 LS-FILE-STATUSES.
         03 TCIN-FS                 PIC XX.
           88 TCIN-OK                 VALUE '00'.
           88 TCIN-EOF                VALUE '10'.

      **
       PROCEDURE DIVISION.
      *
       MAIN.
           DISPLAY ' ************** X60DTS01 START **************'.

           PERFORM OPEN-TEST-CASES-FILE.
           PERFORM READ-TEST-CASES-FILE.

           PERFORM UNTIL TCIN-EOF
              PERFORM EXECUTE-TEST-CASE
              PERFORM READ-TEST-CASES-FILE
           END-PERFORM

           PERFORM CLOSE-TEST-CASES-FILE.

           PERFORM SHOW-STATISTICS.

           DISPLAY ' *************** X60DTS01 END ***************'.

           IF TEST-CASE-FAILED-CTR NOT EQUAL ZERO
              MOVE 12                     TO RETURN-CODE
           END-IF.

           GOBACK.

      *
       OPEN-TEST-CASES-FILE.
           OPEN INPUT TCIN.

           IF NOT TCIN-OK
              DISPLAY 'TEST CASESFILE OPEN ERROR - FS: ' TCIN-FS
              PERFORM RAISE-ERROR
           END-IF.

      *
       READ-TEST-CASES-FILE.
           READ TCIN.

           IF NOT TCIN-OK AND NOT TCIN-EOF
              DISPLAY 'TEST CASES FILE READ ERROR - FS: ' TCIN-FS
              PERFORM RAISE-ERROR
           END-IF.

      *
       CLOSE-TEST-CASES-FILE.
           CLOSE TCIN.

           IF NOT TCIN-OK
              DISPLAY 'TEST CASES FILE CLOSE ERROR - FS: ' TCIN-FS
              PERFORM RAISE-ERROR
           END-IF.

      *
       EXECUTE-TEST-CASE.
           ADD 1                          TO TEST-CASE-CTR.

           PERFORM SET-TEST-CASE-INPUT.

           CALL CC-ISO-DEBLOCKER USING MIO MIO-FMT
                                       MP MR
                    ON EXCEPTION PERFORM RAISE-CALL-ERROR
                NOT ON EXCEPTION PERFORM TEST-CASE-CHECK
           END-CALL.


      *
       SET-TEST-CASE-INPUT.
           INITIALIZE FMT-TOT
                      MIO-ISO-MESSAGE.

           MOVE TCIN-ISO8583-VERSION           TO MP-VERSION

           IF TCIN-ISO8583-TCT-INCAS
              MOVE FMT-MSG-INCAS               TO MIO-FMT
           END-IF.

           IF TCIN-ISO8583-TCT-STRATUS
              MOVE FMT-MSG-STRATUS             TO MIO-FMT
           END-IF.

           MOVE TCIN-ISO-MSG                   TO MIO-ISO-MESSAGE.

           IF TCIN-FMT-REC-OVERRIDE NOT EQUAL SPACE
              ADD 1                            TO MP-OR-TOT
              MOVE TCIN-FMT-REC-OVERRIDE       TO MP-OR-EL (MP-OR-TOT)
           ELSE
              INITIALIZE MP-OR-TOT
           END-IF.

      *
       TEST-CASE-CHECK.
           SET TEST-CASE-FAILED           TO TRUE

           EVALUATE TRUE
              WHEN TCIN-EXPECTED-EQUAL
                 IF MR-RESULT EQUAL TCIN-EXPECTED
                    SET TEST-CASE-PASSED  TO TRUE
                 END-IF
              WHEN TCIN-EXPECTED-NOT-EQUAL
                 IF MR-RESULT NOT EQUAL TCIN-EXPECTED
                    SET TEST-CASE-PASSED  TO TRUE
                 END-IF
           END-EVALUATE.

           PERFORM SHOW-TEST-CASE-RESULT.

      *
       SHOW-TEST-CASE-RESULT.
           IF TEST-CASE-PASSED
              ADD 1                       TO TEST-CASE-PASSED-CTR
              DISPLAY '---> TEST CASE ' TEST-CASE-CTR ' -PASSED-'
           ELSE
              ADD 1                       TO TEST-CASE-FAILED-CTR
              DISPLAY ' '
              DISPLAY '!!-> TEST CASE ' TEST-CASE-CTR ' -FAILED- <-!!'
              DISPLAY FUNCTION TRIM(TCIN-DESCRIPTION)
                      ' - EXPECTED ' TCIN-EXPECTED-CHECK
                      ' ' TCIN-EXPECTED
                      ' - ACTUAL: ' MR-RESULT
              IF MR-RESULT NOT EQUAL ZERO
                 DISPLAY 'AT POSITION: ' MR-POSITION
                 DISPLAY 'DESCRIPTION: '
                         FUNCTION TRIM(MR-DESCRIPTION)
              END-IF
              DISPLAY ' '
           END-IF.

      *
       SHOW-STATISTICS.
           DISPLAY ' '.
           DISPLAY '************* TEST SUITE RECAP *************'.
           DISPLAY '* TEST CASES: ' TEST-CASE-CTR.
           DISPLAY '* PASSED:     ' TEST-CASE-PASSED-CTR.
           DISPLAY '* FAILED:     ' TEST-CASE-FAILED-CTR.
           DISPLAY '********************************************'.
           DISPLAY ' '.

      *
       RAISE-CALL-ERROR.
           DISPLAY 'CALL EXCEPTION WHEN CALLING ' CC-ISO-DEBLOCKER
           PERFORM RAISE-ERROR.

      *
       RAISE-ERROR.
           MOVE 8                         TO RETURN-CODE.
           GOBACK.
