      * **++ XDBLKCP LINKAGE PARAMETERS COPYBOOKS
      *
       01 MP.
         03 MP-VERSION                 PIC XX.
           88 MP-VERSION-87                VALUE '87'.
           88 MP-VERSION-93                VALUE '93'.
         03 MP-OVERRIDE.
           05 MP-OR-TOT                PIC 9(9) COMP VALUE ZERO.
           05 MP-OR-TB.
             10 MP-OR-EL         OCCURS 0 TO 128
                                 DEPENDING ON MP-OR-TOT.
               15 MP-OR-DE                    PIC 9(3).
               15 FILLER                      PIC X(1).
               15 MP-OR-OUT-CONV              PIC X(1).
               15 FILLER                      PIC X(1).
               15 MP-OR-TYPE                  PIC X(3).
               15 FILLER                      PIC X(1).
               15 MP-OR-PATTERN               PIC X(20).
               15 FILLER REDEFINES MP-OR-PATTERN.
                 20 MP-OR-DEBLOCK-PGM         PIC X(08).
                 20 FILLER                    PIC X(12).
