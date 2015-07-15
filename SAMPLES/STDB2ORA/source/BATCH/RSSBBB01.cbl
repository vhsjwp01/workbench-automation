       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RSSBBB01.
       AUTHOR. METAWARE.
      * ------------------------------------------------------------- *
      *                  SIMPLE SAMPLE APPLICATION                    *
      * ------------------------------------------------------------- *
      * DESCRIPTION:                                                  *
      *    -THIS PROGRAM READS THE DB2 TABLE AND PRODUCES             *
      *     A CUSTOMERS LIST REPORT.                                  *
      * ------------------------------------------------------------- *
      * INPUT TABLE: DB2  - PJ01DB2.ODCSF0                            *
      * OUTPUT FILE: QSAM - PJ01AAA.RT.QSAM.CUSTOMER.REPORT           *
      * ------------------------------------------------------------- *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      
      * CUSTOMER'S REPORT OUTPUT FILE
           SELECT SYSPRINT
                  ASSIGN TO UT-S-SYSPRINT.
      
      * ------------------------------------------------------------- *
       DATA DIVISION.
      
       FILE SECTION.
       FD  SYSPRINT
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS REPORT-REC
           RECORDING MODE IS F.
       01  REPORT-REC                 PIC X(132).
      
      * ------------------------------------------------------------- *
       WORKING-STORAGE SECTION.
      
       77  W-LINE                  PIC S9(04) COMP VALUE +60.
       77  W-MAX-LINE              PIC S9(04) COMP VALUE +12.
      * FILE STATUS FOR OUTPUT FILE
       01  QSAMREPT-STATUS.
           05  QSAMREPT-STAT1      PIC X.
           05  QSAMREPT-STAT2      PIC X.
      
       01  IO-STATUS.
           05  IO-STAT1            PIC X.
           05  IO-STAT2            PIC X.
       01  TWO-BYTES.
           05  TWO-BYTES-LEFT      PIC X.
           05  TWO-BYTES-RIGHT     PIC X.
       01  TWO-BYTES-BINARY        REDEFINES TWO-BYTES PIC 9(4) COMP.
      
       77  END-OF-TABLE            PIC X       VALUE 'N'.
       77  DISP-MSG                PIC X(80)   VALUE SPACES.
       77  APPL-RESULT             PIC S9(9)   COMP.
           88  APPL-AOK            VALUE 0.
           88  APPL-EOF            VALUE 16.
      
      *-- WORKING ZONE FOR DB2  MANIPULATION
           EXEC SQL INCLUDE SQLCA    END-EXEC.
      
           EXEC SQL INCLUDE ODCSF0DB END-EXEC.
      
           EXEC SQL DECLARE CUST-ASC CURSOR FOR
                SELECT CUSTIDENT, CUSTLNAME, CUSTFNAME,
                       CUSTADDRS, CUSTCITY , CUSTSTATE,
                       CUSTBDATE, CUSTEMAIL, CUSTPHONE
                FROM   PJ01DB2.ODCSF0
                ORDER BY CUSTIDENT
           END-EXEC.
      
       77  WS-SQLCODE              PIC -999.
      
      *-- WORKING ZONE FOR DATE MANIPULATION
       01  WS-DATE.
           05  WS-DATE-CC          PIC X(002).
           05  WS-DATE-YY          PIC X(002).
           05  WS-DATE-MM          PIC X(002).
           05  WS-DATE-DD          PIC X(002).
       01  WS-DATE-10.
           05  WS-DATE-10-CC       PIC X(002).
           05  WS-DATE-10-YY       PIC X(002).
           05  FILLER              PIC X(001).
           05  WS-DATE-10-MM       PIC X(002).
           05  FILLER              PIC X(001).
           05  WS-DATE-10-DD       PIC X(002).
       01  WE-DATE.
           05  WE-DATE-MM          PIC X(002).
           05  FILLER              PIC X(001) VALUE '/'.
           05  WE-DATE-DD          PIC X(002).
           05  FILLER              PIC X(001) VALUE '/'.
           05  WE-DATE-CC          PIC X(002).
           05  WE-DATE-YY          PIC X(002).
      
      *-- DESCRIPTION OF THE REPORT TITLE LINE
       01  W-TITLE.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(008) VALUE 'RSSBBB01'.
           05  FILLER              PIC X(038) VALUE SPACES.
           05  FILLER              PIC X(026) VALUE
               'SIMPLE SAMPLE APPLICATION'.
           05  FILLER              PIC X(039) VALUE SPACES.
           05  WT-DATE             PIC X(010).
      
      *-- DESCRIPTION OF A CUSTOMER'S HEADER PRINTED DATA
       01  W-CUST-HEADER1.
           05  FILLER              PIC X(010) VALUE SPACES.
           05  FILLER              PIC X(006) VALUE '_ ID _'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(030)
                               VALUE '_    LAST NAME               _'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(020)
                               VALUE '_    FIRST NAME    _'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(020)
                               VALUE '_    CITY          _'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(010)
                               VALUE '_ PHONE  _'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(010)
                               VALUE '_B. DATE _'.
           05  FILLER              PIC X(011) VALUE SPACE.
      
      *-- DESCRIPTION OF A CUSTOMER'S HEADER PRINTED DATA
       01  W-CUST-HEADER2.
           05  FILLER              PIC X(010) VALUE SPACES.
           05  FILLER              PIC X(006) VALUE '------'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(030)
                               VALUE '------------------------------'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(020)
                               VALUE '--------------------'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(020)
                               VALUE '--------------------'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(010)
                               VALUE '----------'.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(010)
                               VALUE '----------'.
           05  FILLER              PIC X(011) VALUE SPACE.
      
      *-- DESCRIPTION OF A CUSTOMER'S PRINTED DATA
       01  W-CUST-DETAIL.
           05  FILLER              PIC X(010) VALUE SPACES.
           05  WC-IDENT            PIC ZZZZZ9.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  WC-LNAME            PIC X(030).
           05  FILLER              PIC X(001) VALUE SPACE.
           05  WC-FNAME            PIC X(020).
           05  FILLER              PIC X(001) VALUE SPACE.
           05  WC-CITY             PIC X(020).
           05  FILLER              PIC X(001) VALUE SPACE.
           05  WC-PHONE            PIC X(010).
           05  FILLER              PIC X(001) VALUE SPACE.
           05  WC-BDATE            PIC X(010).
           05  FILLER              PIC X(011) VALUE SPACE.
      
      
      * ------------------------------------------------------------- *
       PROCEDURE DIVISION.
      
      * ------------------------------------------------------------- *
      * FILE OPENING...
           DISPLAY "OPENING OUTPUT FILE AND DB2 CURSOR..."
           OPEN OUTPUT SYSPRINT.
           EXEC SQL
                OPEN CUST-ASC
           END-EXEC.
      
      * ------------------------------------------------------------- *
      * QSAM FILE'S SWEEPING
           DISPLAY "SWEEPING DB2 TABLE..."
           PERFORM UNTIL END-OF-TABLE = 'Y'
              PERFORM FETCH-ODCSF0
              IF END-OF-TABLE = 'N'
                 PERFORM WRITE-REPORT
              END-IF
           END-PERFORM.
      
      * ------------------------------------------------------------- *
      * FILE CLOSING...
           DISPLAY "CLOSING OUTPUT FILE AND DB2 CURSOR..."
           CLOSE SYSPRINT.
           EXEC SQL
                CLOSE CUST-ASC
           END-EXEC.
      
           DISPLAY "EXITING PROGRAM..."
           GOBACK.
      
      *---------------------------------------------------------------*
       FETCH-ODCSF0.
      
           EXEC SQL
                FETCH CUST-ASC
                INTO :VS-CUSTIDENT, :VS-CUSTLNAME, :VS-CUSTFNAME,
                     :VS-CUSTADDRS, :VS-CUSTCITY , :VS-CUSTSTATE,
                     :VS-CUSTBDATE, :VS-CUSTEMAIL, :VS-CUSTPHONE
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
      
           IF SQLCODE = +0
              CONTINUE
           ELSE
              IF SQLCODE = +100
                 MOVE 'Y' TO END-OF-TABLE
              ELSE
                 DISPLAY 'FETCH ON TABLE PJ01DB2.ODCSF0.... '
                         ' SQLCODE:' WS-SQLCODE
                 PERFORM Z-ABEND-PROGRAM
              END-IF
           END-IF.
           EXIT.
      
      *---------------------------------------------------------------*
       WRITE-REPORT.
           IF W-LINE > W-MAX-LINE THEN
              PERFORM WRITE-NEW-PAGE
           END-IF
      * MOVE DATA FROM VSAM TO OUTPUT FILE...
           MOVE VS-CUSTIDENT  TO WC-IDENT
           MOVE VS-CUSTLNAME  TO WC-LNAME
           MOVE VS-CUSTFNAME  TO WC-FNAME
           MOVE VS-CUSTCITY   TO WC-CITY
           MOVE VS-CUSTPHONE  TO WC-PHONE
           MOVE VS-CUSTBDATE  TO WS-DATE-10
           MOVE WS-DATE-10-CC TO WE-DATE-CC
           MOVE WS-DATE-10-YY TO WE-DATE-YY
           MOVE WS-DATE-10-MM TO WE-DATE-MM
           MOVE WS-DATE-10-DD TO WE-DATE-DD
           MOVE WE-DATE       TO WC-BDATE
           WRITE REPORT-REC FROM W-CUST-DETAIL
                 AFTER ADVANCING 1 LINE
           ADD 1 TO W-LINE
           EXIT.
      
      *---------------------------------------------------------------*
       WRITE-NEW-PAGE.
      * TITLE...
           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATE
           MOVE WS-DATE-CC TO  WE-DATE-CC
           MOVE WS-DATE-YY TO  WE-DATE-YY
           MOVE WS-DATE-MM TO  WE-DATE-MM
           MOVE WS-DATE-DD TO  WE-DATE-DD
           MOVE WE-DATE    TO  WT-DATE
           WRITE REPORT-REC FROM W-TITLE
                 AFTER ADVANCING PAGE
      * HEADER 1...
           WRITE REPORT-REC FROM W-CUST-HEADER1
                 AFTER ADVANCING 2 LINES
      * HEADER 2...
           WRITE REPORT-REC FROM W-CUST-HEADER2
                 AFTER ADVANCING 1 LINE
           MOVE 8 TO W-LINE
           EXIT.
      
      *---------------------------------------------------------------*
      * THE FOLLOWING Z-ROUTINES PERFORM ADMINISTRATIVE TASKS         *
      * FOR THIS PROGRAM.                                             *
      *---------------------------------------------------------------*
      
      *---------------------------------------------------------------*
      * ABEND THE PROGRAM, DISPLAY A MESSAGE AND STOP THE PROGRAM.    *
      *---------------------------------------------------------------*
       Z-ABEND-PROGRAM.
           IF  DISP-MSG NOT = SPACES
               PERFORM Z-DISPLAY-DISP-MSG
           END-IF
           MOVE 'RSSBBB01: PROGRAM-IS-ABENDING...'   TO DISP-MSG
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
               DISPLAY '* RSSBBB01 FILE-STATUS-' IO-STAT1 '/'
                       TWO-BYTES-BINARY
           ELSE
               DISPLAY '* RSSBBB01: FILE-STATUS-' IO-STATUS
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       Z-DISPLAY-DISP-MSG.
           DISPLAY DISP-MSG
           MOVE ALL SPACES TO DISP-MSG
           EXIT.
