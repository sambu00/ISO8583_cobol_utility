      * **++ Area input per deblock tlv
       01 TLV-:X:-IN.
         03 TLV-:X:-TEXT.
           05 TLV-:X:-TEXT-LEN               PIC 9(9) COMP.
           05 TLV-:X:-TEXT-DATA              PIC X(999).
         03 TLV-:X:-CONVERSION.
           05 TLV-:X:-CONV-FLAG              PIC X.
         03 TLV-:X:-PATTERN.
           05 FILLER.
             07 T-:X:-FMT                    PIC X(1).
             07 T-:X:-LEN                    PIC 9(1).
             07 L-:X:-FMT                    PIC X(1).
             07 L-:X:-LEN                    PIC 9(1).
           05 TLV-:X:-INNER-PATTERN          PIC X(16).
