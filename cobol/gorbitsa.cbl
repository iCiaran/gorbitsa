       IDENTIFICATION DIVISION.
       PROGRAM-ID. GORBITSA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROGRAM-FILE ASSIGN TO DYNAMIC PROGRAM-PATH
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PROGRAM-FILE.
       01 INSTRUCTION-RECORD.
         03 OPCODE-RECORD      PIC X(1).
         03 OPERAND-RECORD     PIC X(3).
         
       WORKING-STORAGE SECTION.
       01 GORBITSA.
         03 PC                 PIC 9(3) VALUE 1.
         03 X                  PIC 9(3) VALUE 0.
         03 RAM                PIC 9(3) OCCURS 256 TIMES.
         03 INSTRUCTION                 OCCURS 256 TIMES.
           05 OPCODE           PIC X(1).
           05 OPERAND          PIC 9(3).
       01 PROGRAM-PATH         PIC X(200).
       01 ARG-COUNT            PIC 9(3).
       01 ERROR-STRING         PIC X(100). 
       01 EOF                  PIC X(1) VALUE "N".
       01 LNUM                 PIC 9(3) VALUE 1.
       01 IDX.
         03 DIRECTION          PIC X(1).
         03 IDX-P              PIC 9(3).
         03 IDX-C              PIC 9(3).
       01 RECEIVE-IN           PIC X(30).
       01 RECEIVE-IN-X         PIC 9(3).
       01 RECEIVE-LEN          PIC 9(2).
       01 I                    PIC 9(2).

       PROCEDURE DIVISION.
           PERFORM LOAD-PROGRAM THRU LOAD-PROGRAM-FN.
           PERFORM RUN-PROGRAM  THRU RUN-PROGRAM-FN.

       EXIT-PROGRAM.
           IF ERROR-STRING NOT = SPACES
             DISPLAY 
               "Error in " ERROR-STRING
             END-DISPLAY
           END-IF.
           STOP RUN.

       LOAD-PROGRAM.
      *-------------*
+DEBUG*    DISPLAY "====      START LOADING PROGRAM       ====" 
+DEBUG*    END-DISPLAY.
           ACCEPT ARG-COUNT 
             FROM ARGUMENT-NUMBER
           END-ACCEPT.
           IF ARG-COUNT NOT = 1
             STRING "LOAD-PROGRAM | Wrong number of arguments: " 
               ARG-COUNT
               INTO ERROR-STRING
             END-STRING
             GO EXIT-PROGRAM
           END-IF.

           ACCEPT PROGRAM-PATH 
             FROM ARGUMENT-VALUE
           END-ACCEPT.

           OPEN INPUT PROGRAM-FILE.
           PERFORM UNTIL EOF = "Y"
             READ PROGRAM-FILE
               AT END 
                 MOVE "Y" TO EOF
               NOT AT END
                 MOVE OPCODE-RECORD  OF INSTRUCTION-RECORD 
                          TO OPCODE  OF INSTRUCTION(LNUM)
                 MOVE OPERAND-RECORD OF INSTRUCTION-RECORD
                          TO OPERAND OF INSTRUCTION(LNUM)
+DEBUG*          DISPLAY "  -- " LNUM " : "
+DEBUG*                INSTRUCTION (LNUM) END-DISPLAY
                 ADD 1 TO LNUM 
                   GIVING LNUM
                 END-ADD
             END-READ
           END-PERFORM.
           CLOSE PROGRAM-FILE.
+DEBUG*    DISPLAY "====       END LOADING PROGRAM        ====" 
+DEBUG*    END-DISPLAY.
       LOAD-PROGRAM-FN.
      *----------------*
           EXIT.

       RUN-PROGRAM.
      *------------*
+DEBUG*    DISPLAY "====      START RUNNING PROGRAM       ====" 
+DEBUG*    END-DISPLAY.
           PERFORM UNTIL PC >= 256
             EVALUATE OPCODE OF INSTRUCTION (PC)
      * Base instructions
               WHEN "G"
                 PERFORM I-GRAB     THRU I-GRAB-FN
               WHEN "O"
                 PERFORM I-OFFER    THRU I-OFFER-FN
               WHEN "R"
                 PERFORM I-RECEIVE  THRU I-RECEIVE-FN
               WHEN "B"
                 PERFORM I-BRANCH   THRU I-BRANCH-FN
               WHEN "I"
                 PERFORM I-INCREASE THRU I-INCREASE-FN
               WHEN "T"
                 PERFORM I-TRANSMIT THRU I-TRANSMIT-FN
               WHEN "S"
                 PERFORM I-SET      THRU I-SET-FN
               WHEN "A"
                 PERFORM I-ADD      THRU I-ADD-FN
      * Extended instructions
               WHEN "g"
                 PERFORM E-GRAB     THRU E-GRAB-FN
               WHEN "o"
                 PERFORM E-OFFER    THRU E-OFFER-FN
               WHEN "r"
                 PERFORM E-RECEIVE  THRU E-RECEIVE-FN
               WHEN OTHER
                 PERFORM I-NOOP     THRU I-NOOP-FN
             END-EVALUATE
           END-PERFORM.
+DEBUG*    DISPLAY "====       END RUNNING PROGRAM        ====" 
+DEBUG*    END-DISPLAY.
       RUN-PROGRAM-FN.
      *---------------*
           EXIT.

       I-NOOP.
      *-----*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
           ADD 1 TO PC END-ADD.
       I-NOOP-FN.
      *--------*
           EXIT.

      *================================================================*
      *                     ___     _     ___   ___                    *
      *                    | _ )   /_\   / __| | __|                   *
      *                    | _ \  / _ \  \__ \ | _|                    *
      *                    |___/ /_/ \_\ |___/ |___|                   *
      *                                                                *
      *================================================================*

       I-GRAB.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING GRAB" END-DISPLAY.
           MOVE OPERAND OF INSTRUCTION (PC) TO IDX-P       OF IDX.
           MOVE "I"                         TO DIRECTION   OF IDX.
           PERFORM CORRECT-INDEX THRU CORRECT-INDEX-FN.
           MOVE RAM (IDX-C OF IDX) TO X.
+DEBUG*    DISPLAY "   - Grabbed " X " from [" 
+DEBUG*    OPERAND OF INSTRUCTION(PC) "](" IDX-C OF IDX ")" END-DISPLAY.
           ADD 1 TO PC END-ADD.
       I-GRAB-FN.
      *--------------*
           EXIT.

       I-OFFER.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING OFFER" END-DISPLAY.
           MOVE OPERAND OF INSTRUCTION (PC) TO IDX-P       OF IDX.
           MOVE "I"                         TO DIRECTION   OF IDX.
           PERFORM CORRECT-INDEX THRU CORRECT-INDEX-FN.
           MOVE X TO RAM (IDX-C OF IDX).
+DEBUG*    DISPLAY "   - Offered " X " to [" OPERAND OF INSTRUCTION(PC)
+DEBUG*                  "](" IDX-C OF IDX ")" END-DISPLAY.
           ADD 1 TO PC END-ADD.
       I-OFFER-FN.
      *--------------*
           EXIT.

       I-RECEIVE.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING RECEIVE" END-DISPLAY.
           DISPLAY "> " WITH NO ADVANCING END-DISPLAY.
           ACCEPT RECEIVE-IN END-ACCEPT.
+DEBUG*    DISPLAY "   - Accepted: "RECEIVE-IN END-DISPLAY.
           MOVE 0 TO RECEIVE-LEN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL RECEIVE-IN(I:1) = SPACE
                                                             OR I > 30
             IF RECEIVE-IN(I:1) NOT = SPACE
               ADD 1 TO RECEIVE-LEN END-ADD
             END-IF
           END-PERFORM.

           IF RECEIVE-IN(1:RECEIVE-LEN) IS NUMERIC
+DEBUG*    DISPLAY "   - Input is numeric." END-DISPLAY 
             MOVE RECEIVE-IN(1:3) TO X
             IF X > 255
               STRING 
                 "I-RECEIVE | Number is too big: " DELIMITED BY SIZE
                 RECEIVE-IN DELIMITED BY SPACE
                 INTO ERROR-STRING
               END-STRING
               GO EXIT-PROGRAM
             END-IF
           ELSE
+DEBUG*    DISPLAY "   - Input is not numeric." END-DISPLAY 
             IF RECEIVE-LEN = 1
               MOVE FUNCTION ORD(RECEIVE-IN(1:1)) TO X
             ELSE
               STRING 
                 "I-RECEIVE | Can only receive single letters: " 
                 DELIMITED BY SIZE
                 RECEIVE-IN DELIMITED BY SPACE
                 INTO ERROR-STRING
               END-STRING
               GO EXIT-PROGRAM
             END-IF
           END-IF.
+DEBUG*    DISPLAY "   - Received " X " from input." END-DISPLAY. 
           ADD 1 TO PC END-ADD.
       I-RECEIVE-FN.
      *--------------*
           EXIT.

       I-BRANCH.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING BRANCH" END-DISPLAY.
           IF X = 0
              MOVE OPERAND OF INSTRUCTION (PC) TO IDX-P     OF IDX 
              MOVE "I"                         TO DIRECTION OF IDX
              PERFORM CORRECT-INDEX THRU CORRECT-INDEX-FN
+DEBUG*    DISPLAY "   - Branching to ["IDX-P OF IDX"]("
+DEBUG*                                 IDX-C OF IDX")" END-DISPLAY
              MOVE IDX-C OF IDX TO PC
           ELSE
             ADD 1 TO PC END-ADD
           END-IF.
       I-BRANCH-FN.
      *--------------*
           EXIT.

       I-INCREASE.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING INCREASE" END-DISPLAY.
+DEBUG*    DISPLAY "   - Adding " OPERAND OF INSTRUCTION (PC)
+DEBUG*                              " to X" END-DISPLAY.
           ADD OPERAND OF INSTRUCTION (PC) TO X GIVING X END-ADD.
           IF X > 255
+DEBUG*    DISPLAY "   - Overflowed X="X END-DISPLAY
             SUBTRACT 256 FROM X END-SUBTRACT
           END-IF.
           ADD 1 TO PC END-ADD.
       I-INCREASE-FN.
      *--------------*
           EXIT.

       I-TRANSMIT.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING TRANSMIT" END-DISPLAY.
           DISPLAY X END-DISPLAY.
           ADD 1 TO PC END-ADD.
       I-TRANSMIT-FN.
      *--------------*
           EXIT.

       I-SET.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING SET" END-DISPLAY.
+DEBUG*    DISPLAY "   - Setting X to " 
+DEBUG*         OPERAND OF INSTRUCTION (PC)  END-DISPLAY.
           MOVE OPERAND OF INSTRUCTION (PC) TO X.
           ADD 1 TO PC END-ADD.
       I-SET-FN.
      *--------------*
           EXIT.

       I-ADD.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING ADD" END-DISPLAY.
           MOVE OPERAND OF INSTRUCTION (PC) TO IDX-P     OF IDX.
           MOVE "I"                         TO DIRECTION OF IDX.
           PERFORM CORRECT-INDEX THRU CORRECT-INDEX-FN.
+DEBUG*    DISPLAY "   - Adding " RAM(IDX-C OF IDX) " to X from "
+DEBUG*            "["IDX-P OF IDX"]("IDX-C OF IDX")" END-DISPLAY.
           ADD RAM(IDX-C OF IDX) TO X END-ADD.
           IF X > 255
+DEBUG*    DISPLAY "   - Overflowed X="X END-DISPLAY
             SUBTRACT 256 FROM X END-SUBTRACT
           END-IF.
           ADD 1 TO PC END-ADD.
       I-ADD-FN.
      *--------------*
           EXIT.

      *================================================================*
      *      ___  __  __  _____   ___   _  _   ___    ___   ___        *
      *     | __| \ \/ / |_   _| | __| | \| | |   \  | __| |   \       *
      *     | _|   >  <    | |   | _|  | .` | | |) | | _|  | |) |      *
      *     |___| /_/\_\   |_|   |___| |_|\_| |___/  |___| |___/       *
      *                                                                *
      *================================================================*

       E-GRAB.
      *------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING E-GRAB" END-DISPLAY.
           MOVE OPERAND OF INSTRUCTION (PC) TO IDX-P     OF IDX.
           MOVE "I"                         TO DIRECTION OF IDX.
           PERFORM CORRECT-INDEX          THRU CORRECT-INDEX-FN.
+DEBUG*    DISPLAY "   - Indirect address is ["IDX-P OF IDX"]("
+DEBUG*            IDX-C OF IDX")" END-DISPLAY.
           MOVE RAM (IDX-C)                 TO IDX-P     OF IDX.
           PERFORM CORRECT-INDEX          THRU CORRECT-INDEX-FN.
+DEBUG*    DISPLAY "   - Direct address is   ["IDX-P OF IDX"]("
+DEBUG*            IDX-C OF IDX")" END-DISPLAY.
           MOVE RAM (IDX-C)                 TO X.
+DEBUG*    DISPLAY "Grabbing " RAM (IDX-C) " into X" END-DISPLAY.
           ADD 1 TO PC END-ADD.
       E-GRAB-FN.
      *----------*
           EXIT.
           
       E-OFFER.
      *------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING E-OFFER" END-DISPLAY.
           MOVE OPERAND OF INSTRUCTION (PC) TO IDX-P     OF IDX.
           MOVE "I"                         TO DIRECTION OF IDX.
           PERFORM CORRECT-INDEX          THRU CORRECT-INDEX-FN.
+DEBUG*    DISPLAY "   - Indirect address is ["IDX-P OF IDX"]("
+DEBUG*            IDX-C OF IDX")" END-DISPLAY.
           MOVE RAM (IDX-C)                 TO IDX-P     OF IDX.
           PERFORM CORRECT-INDEX          THRU CORRECT-INDEX-FN.
+DEBUG*    DISPLAY "   - Direct address is   ["IDX-P OF IDX"]("
+DEBUG*            IDX-C OF IDX")" END-DISPLAY.
           MOVE X                           TO RAM (IDX-C).
+DEBUG*    DISPLAY "Offering " X " to " RAM (IDX-C) END-DISPLAY.
           ADD 1 TO PC END-ADD.
       E-OFFER-FN.
      *----------*
           EXIT.

       E-RECEIVE.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY "  == EXECUTING E-RECEIVE" END-DISPLAY.
           MOVE OPERAND OF INSTRUCTION (PC) TO IDX-P       OF IDX.
           MOVE "I"                         TO DIRECTION   OF IDX.
           PERFORM CORRECT-INDEX THRU CORRECT-INDEX-FN.
           DISPLAY "> " WITH NO ADVANCING END-DISPLAY.
           ACCEPT RECEIVE-IN END-ACCEPT.
+DEBUG*    DISPLAY "   - Accepted: "RECEIVE-IN END-DISPLAY.
           MOVE 0 TO RECEIVE-LEN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL RECEIVE-IN(I:1) = SPACE
                                                             OR I > 30
             IF RECEIVE-IN(I:1) NOT = SPACE
               ADD 1 TO RECEIVE-LEN END-ADD
             END-IF
           END-PERFORM.

           IF RECEIVE-IN(1:RECEIVE-LEN) IS NUMERIC
+DEBUG*    DISPLAY "   - Input is numeric." END-DISPLAY 
             MOVE RECEIVE-IN(1:3) TO RAM(IDX-C OF IDX)
             IF X > 255
               STRING 
                 "I-RECEIVE | Number is too big: " DELIMITED BY SIZE
                 RECEIVE-IN DELIMITED BY SPACE
                 INTO ERROR-STRING
               END-STRING
               GO EXIT-PROGRAM
             END-IF
           ELSE
+DEBUG*    DISPLAY "   - Input is not numeric." END-DISPLAY 
             IF RECEIVE-LEN = 1
               MOVE FUNCTION ORD(RECEIVE-IN(1:1)) TO RAM(IDX-C OF IDX)
             ELSE
               STRING 
                 "I-RECEIVE | Can only receive single letters: " 
                 DELIMITED BY SIZE
                 RECEIVE-IN DELIMITED BY SPACE
                 INTO ERROR-STRING
               END-STRING
               GO EXIT-PROGRAM
             END-IF
           END-IF.
+DEBUG*    DISPLAY "   - Received " RAM(IDX-C OF IDX) " from input " 
+DEBUG*         "into ["IDX-P OF IDX"]("IDX-C OF IDX")" END-DISPLAY.
           ADD 1 TO PC END-ADD.
       E-RECEIVE-FN.
      *--------------*
           EXIT.

       PRINT-DEBUG.
      *------------*
           DISPLAY "==== DEGUG"
                   " -- PC:" PC
                   " - X:"   X
                   " - I:"   INSTRUCTION (PC)
                   " ===="  
           END-DISPLAY.
       PRINT-DEBUG-FN.
      *---------------*
           EXIT.

       CORRECT-INDEX.
      *--------------*
      * GORBITSA programs are 0-indexed, our memory is 1-indexed
           IF DIRECTION OF IDX = "O"
             SUBTRACT 1 FROM IDX-C GIVING IDX-P END-SUBTRACT
           ELSE 
              ADD 1 TO IDX-P GIVING IDX-C END-ADD
           END-IF.
       CORRECT-INDEX-FN.
      *-----------------*
           EXIT.
