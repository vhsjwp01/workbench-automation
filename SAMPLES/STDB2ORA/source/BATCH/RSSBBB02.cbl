       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RSSBBB02.
       AUTHOR. METAWARE ERIC LEBRET.
      * ------------------------------------------------------------- *
      *                  SIMPLE SAMPLE APPLICATION                    *
      * ------------------------------------------------------------- *
      * DESCRIPTION:                                                  *
      *    -THIS PROGRAM READS A QSAM FILE CONTAINING CUSTOMER'S DATA *
      *     AND ACTIONS TO BE PERFORMED AGAINST THE DB2 TABLE         *
      * ------------------------------------------------------------- *
      * INPUT FILE  : QSAM - PJ01AAA.RT.QSAM.CUSTOMER.UPDATE          *
      * OUTPUT TABLE: DB2  - PJ01DB2.ODCSF0                           *
      * ------------------------------------------------------------- *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      
      * CUSTOMER'S DATA SEQUENTIAL INPUT FILE
           SELECT QSAMCUST-FILE
               ASSIGN       TO QSAMCUST
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS QSAMCUST-STATUS.
      
      * ------------------------------------------------------------- *
       DATA DIVISION.
      
       FILE SECTION.
       FD   QSAMCUST-FILE
            RECORD CONTAINS 269 CHARACTERS.
            COPY ODCSFU.
      
      * ------------------------------------------------------------- *
       WORKING-STORAGE SECTION.
      
      * FILE STATUS FOR INPUT AND OUTPUT FILES
       01  QSAMCUST-STATUS.
           05  QSAMCUST-STAT1      PIC X.
           05  QSAMCUST-STAT2      PIC X.
      
       01  IO-STATUS.
           05  IO-STAT1            PIC X.
           05  IO-STAT2            PIC X.
       01  TWO-BYTES.
           05  TWO-BYTES-LEFT      PIC X.
           05  TWO-BYTES-RIGHT     PIC X.
       01  TWO-BYTES-BINARY        REDEFINES TWO-BYTES PIC 9(4) COMP.
      
       01  END-OF-FILE             PIC X       VALUE 'N'.
       01  DISP-MSG                PIC X(80)   VALUE SPACES.
       01  APPL-RESULT             PIC S9(9)   COMP.
           88  APPL-AOK            VALUE 0.
           88  APPL-EOF            VALUE 16.
      
       01  WS-DATE-10.
           05 WS-DATE-10-CC        PIC X(02).
           05 WS-DATE-10-YY        PIC X(02).
           05 FILLER               PIC X(01) VALUE '-'.
           05 WS-DATE-10-MM        PIC X(02).
           05 FILLER               PIC X(01) VALUE '-'.
           05 WS-DATE-10-DD        PIC X(02).
      
       01  WS-SQLCODE              PIC -999.
           EXEC SQL INCLUDE SQLCA    END-EXEC.
           EXEC SQL INCLUDE ODCSF0DB END-EXEC.
      
      * ------------------------------------------------------------- *
       PROCEDURE DIVISION.
      
      * ------------------------------------------------------------- *
      * FILE OPENING...
           DISPLAY "OPENING INPUT FILE..."
           PERFORM QSAMCUST-OPEN.
      
      * ------------------------------------------------------------- *
      * QSAM FILE'S SWEEPING
           DISPLAY "SWEEPING INPUT QSAM FILE..."
           PERFORM UNTIL END-OF-FILE = 'Y'
              PERFORM QSAMCUST-GET
              IF END-OF-FILE = 'N'
                 PERFORM PROCESS-TABLE-UPDATE
              END-IF
           END-PERFORM.
      
      * ------------------------------------------------------------- *
      * FILE CLOSING...
           DISPLAY "CLOSING INPUT FILE..."
           PERFORM QSAMCUST-CLOSE.
      
           DISPLAY "EXITING PROGRAM..."
           GOBACK.
      
      * ------------------------------------------------------------- *
      * PROCESS THE INPUT RECORD FROM QSAM FILE DEPENDING ON          *
      * ACTION CODE...                                                *
      * ------------------------------------------------------------- *
       PROCESS-TABLE-UPDATE.
           EVALUATE QS-ACTION
              WHEN 'ADD'
                   PERFORM PROCESS-ADD-RECORD
              WHEN 'MOD'
                   PERFORM PROCESS-MOD-RECORD
              WHEN 'DEL'
                   PERFORM PROCESS-DEL-RECORD
              WHEN OTHER
                   DISPLAY 'INVALID ACTION ' QS-ACTION
                        ' RECORD REJECTED'
           END-EVALUATE.
      
      * ------------------------------------------------------------- *
      * ROUTINES TO CREATE A NEW CUSTOMER IN THE VSAM FILE            *
      * ------------------------------------------------------------- *
       PROCESS-ADD-RECORD.
           DISPLAY 'CREATING CUSTOMER ' QS-CUSTIDENT
           MOVE QS-CUSTIDENT     TO VS-CUSTIDENT
           MOVE QS-CUSTLNAME     TO VS-CUSTLNAME
           MOVE QS-CUSTFNAME     TO VS-CUSTFNAME
           MOVE QS-CUSTADDRS     TO VS-CUSTADDRS
           MOVE QS-CUSTCITY      TO VS-CUSTCITY
           MOVE QS-CUSTSTATE     TO VS-CUSTSTATE
           MOVE QS-CUSTBDATE-CC  TO WS-DATE-10-CC
           MOVE QS-CUSTBDATE-YY  TO WS-DATE-10-YY
           MOVE QS-CUSTBDATE-MM  TO WS-DATE-10-MM
           MOVE QS-CUSTBDATE-DD  TO WS-DATE-10-DD
           MOVE WS-DATE-10       TO VS-CUSTBDATE
           MOVE QS-CUSTEMAIL     TO VS-CUSTEMAIL
           MOVE QS-CUSTPHONE     TO VS-CUSTPHONE
      
           EXEC SQL INSERT INTO PJ01DB2.ODCSF0
                       ( CUSTIDENT, CUSTLNAME, CUSTFNAME,
                         CUSTADDRS, CUSTCITY,  CUSTSTATE,
                         CUSTBDATE, CUSTEMAIL, CUSTPHONE
                       )
                VALUES (
                         :VS-CUSTIDENT, :VS-CUSTLNAME, :VS-CUSTFNAME,
                         :VS-CUSTADDRS, :VS-CUSTCITY,  :VS-CUSTSTATE,
                         :VS-CUSTBDATE, :VS-CUSTEMAIL, :VS-CUSTPHONE
                       )
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
      
           IF SQLCODE NOT = +0
              DISPLAY 'ERROR ON CREATION OF CUSTOMER ' QS-CUSTIDENT
                      ' SQLCODE:' WS-SQLCODE
           EXIT.
      
      * ------------------------------------------------------------- *
      * ROUTINES TO UPDATE EXISTING CUSTOMER'S DATA                   *
      * ------------------------------------------------------------- *
       PROCESS-MOD-RECORD.
           DISPLAY 'UPDATING CUSTOMER ' QS-CUSTIDENT
           MOVE QS-CUSTIDENT     TO VS-CUSTIDENT
           MOVE QS-CUSTLNAME     TO VS-CUSTLNAME
           MOVE QS-CUSTFNAME     TO VS-CUSTFNAME
           MOVE QS-CUSTADDRS     TO VS-CUSTADDRS
           MOVE QS-CUSTCITY      TO VS-CUSTCITY
           MOVE QS-CUSTSTATE     TO VS-CUSTSTATE
           MOVE QS-CUSTBDATE-CC  TO WS-DATE-10-CC
           MOVE QS-CUSTBDATE-YY  TO WS-DATE-10-YY
           MOVE QS-CUSTBDATE-MM  TO WS-DATE-10-MM
           MOVE QS-CUSTBDATE-DD  TO WS-DATE-10-DD
           MOVE WS-DATE-10       TO VS-CUSTBDATE
           MOVE QS-CUSTEMAIL     TO VS-CUSTEMAIL
           MOVE QS-CUSTPHONE     TO VS-CUSTPHONE
      
           EXEC SQL UPDATE PJ01DB2.ODCSF0
                SET CUSTLNAME = :VS-CUSTLNAME,
                    CUSTFNAME = :VS-CUSTFNAME,
                    CUSTADDRS = :VS-CUSTADDRS,
                    CUSTCITY  = :VS-CUSTCITY,
                    CUSTSTATE = :VS-CUSTSTATE,
                    CUSTBDATE = :VS-CUSTBDATE,
                    CUSTEMAIL = :VS-CUSTEMAIL,
                    CUSTPHONE = :VS-CUSTPHONE
              WHERE CUSTIDENT = :VS-CUSTIDENT
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
      
           IF SQLCODE NOT = +0
              DISPLAY 'ERROR ON UPDATE OF CUSTOMER ' QS-CUSTIDENT
                      ' SQLCODE:' WS-SQLCODE
           EXIT.
      
      * ------------------------------------------------------------- *
      * ROUTINES TO SUPPRESS A CUSTOMER FROM VSAM FILE.               *
      * ------------------------------------------------------------- *
       PROCESS-DEL-RECORD.
           DISPLAY 'DELETING CUSTOMER ' QS-CUSTIDENT
           MOVE QS-CUSTIDENT TO VS-CUSTIDENT.
           EXEC SQL
                DELETE FROM PJ01DB2.ODCSF0
                WHERE  CUSTIDENT = :VS-CUSTIDENT
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
      
           IF SQLCODE NOT = +0
              DISPLAY 'ERROR ON DELETION OF CUSTOMER ' QS-CUSTIDENT
                      ' SQLCODE=' WS-SQLCODE
           EXIT.
      
      * ------------------------------------------------------------- *
      * ROUTINES TO DO A SEQUENTIAL READ OF THE QSAM FILE.            *
      * ------------------------------------------------------------- *
       QSAMCUST-GET.
           READ QSAMCUST-FILE
           IF  QSAMCUST-STATUS = '00'
               SUBTRACT APPL-RESULT FROM APPL-RESULT
           ELSE
               IF  QSAMCUST-STATUS = '10'
                   ADD 16 TO ZERO GIVING APPL-RESULT
               ELSE
                   ADD 12 TO ZERO GIVING APPL-RESULT
               END-IF
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-FILE
               ELSE
                   MOVE 'RSSBBB02: QSAMCUST-FAILURE-GET...'
      
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
           ADD 8 TO ZERO GIVING APPL-RESULT.
           OPEN INPUT QSAMCUST-FILE
           IF  QSAMCUST-STATUS = '00'
               SUBTRACT APPL-RESULT FROM APPL-RESULT
           ELSE
               ADD 12 TO ZERO GIVING APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               MOVE 'RSSBBB02: QSAMCUST-FAILURE-OPEN...'
      
                 TO DISP-MSG
               MOVE QSAMCUST-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-DISP-MSG
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       QSAMCUST-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE QSAMCUST-FILE
           IF  QSAMCUST-STATUS = '00'
               SUBTRACT APPL-RESULT FROM APPL-RESULT
           ELSE
               ADD 12 TO ZERO GIVING APPL-RESULT
           END-IF
      
           IF  APPL-AOK
               CONTINUE
           ELSE
               MOVE 'RSSBBB02:, QSAMCUST, FAILURE, CLOSE...'
      
                 TO DISP-MSG
               MOVE QSAMCUST-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-DISP-MSG
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
      * THE FOLLOWING Z-ROUTINES PERFORM ADMINISTRATIVE TASKS         *
      * FOR THIS PROGRAM.                                             *
      *---------------------------------------------------------------*
      
      *---------------------------------------------------------------*
      * ABEND THE PROGRAM, DISPLAYU A MESSAGE AND STOP THE PROGRAM.   *
      *---------------------------------------------------------------*
       Z-ABEND-PROGRAM.
           IF  DISP-MSG NOT = SPACES
               PERFORM Z-DISPLAY-DISP-MSG
           END-IF
           MOVE 'RSSBBB02: PROGRAM-IS-ABENDING...'  TO DISP-MSG
           PERFORM Z-DISPLAY-DISP-MSG
           ADD 12 TO ZERO GIVING RETURN-CODE
           STOP RUN.
      
      *---------------------------------------------------------------*
      * DISPLAY THE FILE STATUS BYTES. THIS ROUTINE WILL DISPLAY AS   *
      * TWO DIGITS IF THE FULL TWO BYTE FILE STATUS IS NUMERIC. IF    *
      * SECOND BYTE IS NON-NUMERIC THEN IT WILL BE TREATED AS A       *
      * BINARY NUMBER.                                                *
      *---------------------------------------------------------------*
       Z-DISPLAY-IO-STATUS.
           IF  IO-STATUS NOT NUMERIC
           OR  IO-STAT1 = '9'
               SUBTRACT TWO-BYTES-BINARY FROM TWO-BYTES-BINARY
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT
               DISPLAY '* RSSBBB02: FILE-STATUS-' IO-STAT1 '/'
                       TWO-BYTES-BINARY
           ELSE
               DISPLAY '* RSSBBB02: FILE-STATUS-' IO-STATUS
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       Z-DISPLAY-DISP-MSG.
           DISPLAY DISP-MSG
           MOVE ALL SPACES TO DISP-MSG
           EXIT.
