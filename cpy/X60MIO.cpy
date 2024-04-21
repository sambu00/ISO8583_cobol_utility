      * **++ ISO8583 MESSAGE IO COPYBOOK
      *
       01 MIO.
         03 MIO-ISO-MESSAGE.
           05 FILLER                       PIC X(4096)   VALUE SPACE.
      *
         03 MIO-DETAILS.
           05 MIO-DETAILS-TOT              PIC 9(9) COMP VALUE ZERO.
           05 MIO-DETAILS-TB.
             10 MIO-DETAIL OCCURS 0 TO 1000
                           DEPENDING ON MIO-DETAILS-TOT.
               15 MIO-DETAIL-LABEL         PIC X(20).
               15 MIO-DETAIL-VALUE.
                 20 MIO-DETAIL-VALUE-LEN   PIC 9(9) COMP.
                 20 MIO-DETAIL-VALUE-DATA  PIC X(999).
