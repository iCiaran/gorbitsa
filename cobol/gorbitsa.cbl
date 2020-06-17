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
         03 OPERAND-RECORD     PIC 9(3).
         
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
       01 FILE-LINE            PIC 9(3) VALUE 1.

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
             READ PROGRAM-FILE INTO INSTRUCTION (FILE-LINE)
               AT END 
                 MOVE "Y" TO EOF
               NOT AT END
+DEBUG*          DISPLAY "Loaded: " INSTRUCTION (FILE-LINE) END-DISPLAY
                 ADD 1 TO FILE-LINE 
                   GIVING FILE-LINE
                 END-ADD
             END-READ
           END-PERFORM.
           CLOSE PROGRAM-FILE.
       LOAD-PROGRAM-FN.
      *----------------*
           EXIT.

       RUN-PROGRAM.
      *------------*
           PERFORM UNTIL PC > 256
             EVALUATE OPCODE OF INSTRUCTION (PC)
               WHEN "T"
                 PERFORM I-TRANSMIT THRU I-TRANSMIT-FN
               WHEN "S"
                 PERFORM I-SET THRU I-SET-FN
               WHEN OTHER
                 PERFORM I-NOOP THRU I-NOOP-FN
             END-EVALUATE
           END-PERFORM.
       RUN-PROGRAM-FN.
      *---------------*
           EXIT.

       I-NOOP.
      *-----*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY"==== EXECUTING NOOP ====".
           ADD 1 TO PC END-ADD.
       I-NOOP-FN.
      *--------*
           EXIT.

       I-TRANSMIT.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY"==== EXECUTING TRANSMIT ====".
           DISPLAY X END-DISPLAY.
           ADD 1 TO PC END-ADD.

       I-TRANSMIT-FN.
      *--------------*
           EXIT.

       I-SET.
      *-----------*
+DEBUG*    PERFORM PRINT-DEBUG THRU PRINT-DEBUG-FN.
+DEBUG*    DISPLAY"==== EXECUTING SET ====".
+DEBUG*    DISPLAY"  -- Setting X to " OPERAND OF INSTRUCTION (PC).
           MOVE OPERAND OF INSTRUCTION (PC) TO X.
           ADD 1 TO PC END-ADD.
       I-SET-FN.
      *--------------*
           EXIT.

