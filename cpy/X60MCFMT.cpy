      * **++ Data elements format table
       01 MIO-FMT.
         03 FMT-TOT                 PIC 9(9) COMP VALUE ZERO.
      *
         03 FMT-MAP.
           05 FMT-EL OCCURS 500 TIMES
                     DEPENDING ON FMT-TOT
                     INDEXED BY FMT-IDX.
             10 FMT-DE              PIC 9(3).
             10 FILLER              PIC X(1).
             10 FMT-OUT-CONV        PIC X(1).
             10 FILLER              PIC X(1).
             10 FMT-TYPE            PIC X(3).
             10 FILLER              PIC X(1).
             10 FMT-PATTERN         PIC X(20).
             10 FILLER REDEFINES FMT-PATTERN.
               15 FMT-DEDICATED-PGM PIC X(08).
               15 FILLER            PIC X(12).
      *
