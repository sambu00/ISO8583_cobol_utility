      * **++ AREA PER SELEZIONE ROUTINE DEBLOCK/INBLOCK
       01 SPECIAL-ROUTINE-AREA.
         03 SPECIAL-ROUTINE         PIC X(8)      VALUE SPACE.
         03 FILLER REDEFINES SPECIAL-ROUTINE.
           05 FILLER                PIC X(3).
           05 ROUTINE-BEHAVIOR      PIC X(1).
             88  DEBLOCK-BEHAVIOR             VALUE 'D'.
             88  INBLOCK-BEHAVIOR             VALUE 'I'.
           05 FILLER                PIC X(4).
         03 DEBLOCK-ROUTINE REDEFINES SPECIAL-ROUTINE
                                    PIC X(8).
         03 INBLOCK-ROUTINE REDEFINES SPECIAL-ROUTINE
                                    PIC X(8).
