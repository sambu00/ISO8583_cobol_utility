      * **++ Area input per deblock tlv
       01 TLV-:X:-IN.
         03 TLV-:X:-PATTERN.
           05 FILLER.
             07 T-:X:-FMT                    PIC X(1).
             07 T-:X:-LEN                    PIC 9(1).
             07 L-:X:-FMT                    PIC X(1).
             07 L-:X:-LEN                    PIC 9(1).
           05 TLV-:X:-INNER-PATTERN          PIC X(16).
      *
         03 TLV-:X:-CONVERSION.
           05 TLV-:X:-CONV-FLAG              PIC X.
      *
         03 TLV-:X:-DETAILS.
           05 TLV-:X:-DET-TOT                PIC 9(9) COMP.
           05 TLV-:X:-DET-LIST.
             10 TLV-:X:-DET-ELEMENT OCCURS 0 TO 1000
                                    DEPENDING ON TLV-:X:-DET-TOT
                                    INDEXED BY TLV-:X:-DET-I.
               15 TLV-:X:-DET-LABEL          PIC X(20).
               15 TLV-:X:-DET-VALUE.
                 20 TLV-:X:-DET-VALUE-LEN    PIC 9(9) COMP.
                 20 TLV-:X:-DET-VALUE-DATA   PIC X(999).
