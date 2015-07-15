       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RSSABB00.
      
       AUTHOR. Metaware.
      * ------------------------------------------------------------- *
      *                  Simple Sample Application                    *
      * ------------------------------------------------------------- *
      * Description:                                                  *
      *    -This program reads a QSAM file containing customer's data.*
      *    -Records are used to populate a VSAM-KSDS file which will  *
      *     be manipulated by the Simple Sample Application CICS      *
      *     programs.                                                 *
      * ------------------------------------------------------------- *
      * INPUT file : QSAM - PJ01AAA.RT.QSAM.CUSTOMER                  *
      * Output file: VSAM - PJ01AAA.RT.VSAM.CUSTOMER                  *
      * ------------------------------------------------------------- *
      * Note:                                                         *
      * The VSAM must be defined and empty                            *
      * ------------------------------------------------------------- *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      
      * Customer's data sequential input file
           SELECT QSAMCUST-FILE
               ASSIGN       TO QSAMCUST
               ORGANIZATION is SEQUENTIAL
               ACCESS MODE  is SEQUENTIAL
               FILE STATUS  is QSAMCUST-STATUS.
      
      * Customer's data VSAM-KSDS output file
           SELECT VKSDCUST-FILE
               ASSIGN       TO VKSDCUST
               ORGANIZATION is indexed
               ACCESS MODE  is RANDOM
               RECORD KEY   is VS-CUSTIDENT
               FILE STATUS  is VKSDCUST-STATUS.
      
      * ------------------------------------------------------------- *
       DATA DIVISION.
      
       FILE SECTION.
       FD   QSAMCUST-FILE
            RECORD CONTAINS 266 CHARACTERS.
       COPY ODCSF0.
      
       FD  VKSDCUST-FILE.
       COPY ODCSF0B.
      
      * ------------------------------------------------------------- *
       WORKING-STORAGE SECTION.
      
      * File status for input and output files
       01  VKSDCUST-STATUS.
           05  VKSDCUST-STAT1      pic X.
           05  VKSDCUST-STAT2      pic X.
       01  QSAMCUST-STATUS.
           05  QSAMCUST-STAT1      pic X.
           05  QSAMCUST-STAT2      pic X.
      
       01  IO-STATUS.
           05  IO-STAT1            pic X.
           05  IO-STAT2            pic X.
       01  TWO-BYTES.
           05  TWO-BYTES-LEFT      pic X.
           05  TWO-BYTES-RIGHT     pic X.
       01  TWO-BYTES-BINARY        redefines TWO-BYTES pic 9(4) comp.
      
       01  END-OF-FILE             pic X       value 'N'.
       01  DISP-MSG                pic X(80)   value SPACES.
       01  APPL-RESULT             pic S9(9)   COMP.
           88  APPL-AOK            value 0.
           88  APPL-EOF            value 16.
      
      * ------------------------------------------------------------- *
       PROCEDURE DIVISION.
      
      * ------------------------------------------------------------- *
      * Files opening...
           DISPLAY "Opening input and output files..."
           PERFORM QSAMCUST-OPEN.
           PERFORM VKSDCUST-OPEN.
      
      * ------------------------------------------------------------- *
      * QSAM file's sweeping
           DISPLAY "Sweeping INPUT QSAM file..."
           PERFORM UNTIL END-OF-FILE = 'Y'
              PERFORM QSAMCUST-GET
              IF END-OF-FILE = 'N'
                 MOVE QS-ODCSF0-RECORD TO VS-ODCSF0-RECORD
                 PERFORM VKSDCUST-WRITE
              END-IF
           END-PERFORM.
      
      * ------------------------------------------------------------- *
      * Files closing...
           DISPLAY "Closing INPUT and output files..."
           PERFORM VKSDCUST-CLOSE.
           PERFORM QSAMCUST-CLOSE.
      
           DISPLAY "Exiting program..."
           GOBACK.
      
      * ------------------------------------------------------------- *
      * Routines TO do a sequential READ of the QSAM file.            *
      * ------------------------------------------------------------- *
       QSAMCUST-GET.
           READ QSAMCUST-FILE
           IF  QSAMCUST-STATUS = '00'
               SUBTRACT APPL-RESULT from APPL-RESULT
           ELSE
               IF  QSAMCUST-STATUS = '10'
                   ADD 16 TO ZERO giving APPL-RESULT
               ELSE
                   ADD 12 TO ZERO giving APPL-RESULT
               END-IF
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-FILE
               ELSE
                   MOVE 'RSSABB00: QSAMCUST-FAILURE-GET...'
      
                     TO   DISP-MSG
                   MOVE QSAMCUST-STATUS TO IO-STATUS
                   PERFORM Z-DISPLAY-DISP-MSG
                   PERFORM Z-DISPLAY-IO-STATUS
                   PERFORM Z-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       QSAMCUST-OPEN.
           ADD 8 TO ZERO giving APPL-RESULT.
           OPEN INPUT QSAMCUST-FILE
           IF  QSAMCUST-STATUS = '00'
               SUBTRACT APPL-RESULT from APPL-RESULT
           ELSE
               ADD 12 TO ZERO giving APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               MOVE 'RSSABB00: QSAMCUST-FAILURE-OPEN...'
      
                 TO DISP-MSG
               MOVE QSAMCUST-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-DISP-MSG
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       QSAMCUST-CLOSE.
           ADD 8 TO ZERO giving APPL-RESULT.
           CLOSE QSAMCUST-FILE
           IF  QSAMCUST-STATUS = '00'
               SUBTRACT APPL-RESULT from APPL-RESULT
           ELSE
               ADD 12 TO ZERO giving APPL-RESULT
           END-IF
      
           IF  APPL-AOK
               CONTINUE
           ELSE
               MOVE 'RSSABB00: QSAMCUST, FAILURE, CLOSE...'
      
                 TO DISP-MSG
               MOVE QSAMCUST-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-DISP-MSG
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      
      * ------------------------------------------------------------- *
      * Routines to open and populate the VSAM file.                  *
      * ------------------------------------------------------------- *
       VKSDCUST-WRITE.
           WRITE VS-ODCSF0-RECORD
           IF  VKSDCUST-STATUS = '00'
               SUBTRACT APPL-RESULT from APPL-RESULT
           ELSE
               IF  VKSDCUST-STATUS = '10'
                   ADD 16 TO ZERO giving APPL-RESULT
               ELSE
                   ADD 12 TO ZERO giving APPL-RESULT
               END-IF
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'YES' TO END-OF-FILE
               ELSE
                   MOVE 'RSSABB00: VKSDCUST-FAILURE-WRITE...'
      
                     TO   DISP-MSG
                   MOVE VKSDCUST-STATUS TO IO-STATUS
                   PERFORM Z-DISPLAY-DISP-MSG
                   PERFORM Z-DISPLAY-IO-STATUS
                   PERFORM Z-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       VKSDCUST-OPEN.
           ADD 8 TO ZERO giving APPL-RESULT
           OPEN OUTPUT VKSDCUST-FILE
           IF  VKSDCUST-STATUS = '00'
               SUBTRACT APPL-RESULT from APPL-RESULT
           ELSE
               ADD 12 TO ZERO giving APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               MOVE 'RSSABB00: VKSDCUST-FAILURE-OPEN...'
      
                 TO DISP-MSG
               MOVE VKSDCUST-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-DISP-MSG
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       VKSDCUST-CLOSE.
           ADD 8 TO ZERO giving APPL-RESULT.
           CLOSE VKSDCUST-FILE
           IF  VKSDCUST-STATUS = '00'
               SUBTRACT APPL-RESULT from APPL-RESULT
           ELSE
               ADD 12 TO ZERO giving APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               MOVE 'RSSABB00: VKSDCUST-FAILURE-CLOSE...'
      
                 TO   DISP-MSG
               MOVE VKSDCUST-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-DISP-MSG
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
      * The following Z-Routines PERFORM administrative tasks         *
      * for this program.                                             *
      *---------------------------------------------------------------*
      
      *---------------------------------------------------------------*
      * ABEND the program, displayu a message and stop the program.   *
      *---------------------------------------------------------------*
       Z-ABEND-PROGRAM.
           IF  DISP-MSG not = SPACES
               PERFORM Z-DISPLAY-DISP-MSG
           END-IF
           MOVE 'RSSABB00: PROGRAM-IS-ABENDING...'   TO DISP-MSG
      
           PERFORM Z-DISPLAY-DISP-MSG
           ADD 12 TO ZERO giving RETURN-CODE
           STOP RUN.
      
      *---------------------------------------------------------------*
      * DISPLAY the file status bytes. This routine will DISPLAY as   *
      * two digits IF the full two byte file status is numeric. IF    *
      * second byte is non-numeric then it will be treated as a       *
      * binary number.                                                *
      *---------------------------------------------------------------*
       Z-DISPLAY-IO-STATUS.
           IF  IO-STATUS not NUMERIC
           or  IO-STAT1 = '9'
               SUBTRACT TWO-BYTES-BINARY from TWO-BYTES-BINARY
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT
               DISPLAY '* RSSABB00 FILE-STATUS-' IO-STAT1 '/'
      
                       TWO-BYTES-BINARY
           ELSE
               DISPLAY '* RSSABB00 FILE-STATUS-' IO-STATUS
      
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       Z-DISPLAY-DISP-MSG.
           DISPLAY DISP-MSG
           MOVE ALL SPACES TO DISP-MSG
           EXIT.
