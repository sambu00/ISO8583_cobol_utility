      * **++ Area output per deblock TLV
       01 TLV-:X:-OUT.
         03 LIST-:X:-TOT                        PIC 9(9) COMP.
         03 LIST-:X:-TB.
           05 LIST-EL OCCURS 0 TO 500 DEPENDING ON LIST-:X:-TOT.
             10 EL-:X:-TAG-ID                   PIC X(20).
             10 EL-:X:-TAG-VALUE.
               15 EL-:X:-TAG-VALUE-LEN          PIC 9(9) COMP.
               15 EL-:X:-TAG-VALUE-DATA         PIC X(999).
