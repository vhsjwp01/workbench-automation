       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RSSABB01.
      
       AUTHOR. Metaware.
      * ------------------------------------------------------------- *
      *                  Simple Sample Application                    *
      * ------------------------------------------------------------- *
      * Description:                                                  *
      *    -This program reads the VSAM Customers file and produces  .*
      *     a customers list report.                                  *
      * ------------------------------------------------------------- *
      * INPUT file : VSAM - PJ01AAA.RT.VSAM.CUSTOMER                  *
      * Output file: QSAM - PJ01AAA.RT.QSAM.CUSTOMER.REPORT           *
      * ------------------------------------------------------------- *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      
      * Customer's report output file
           SELECT SYSPRINT
                  ASSIGN TO UT-S-SYSPRINT.
      
      * Customer's data VSAM-KSDS output file
           SELECT VKSDCUST-FILE
               ASSIGN       TO VKSDCUST
               ORGANIZATION is INDEXED
               ACCESS MODE  is SEQUENTIAL
               RECORD KEY   is QS-CUSTIDENT
               FILE STATUS  is VKSDCUST-STATUS.
      
      * ------------------------------------------------------------- *
       DATA DIVISION.
      
       FILE SECTION.
       FD  SYSPRINT
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS REPORT-REC
           RECORDING MODE IS F.
       01  REPORT-REC                 PIC X(132).
      
       FD  VKSDCUST-FILE.
       COPY ODCSF0.
      
      * ------------------------------------------------------------- *
       WORKING-STORAGE SECTION.
      
       77  W-LINE                  PIC S9(04) COMP VALUE +60.
       77  W-MAX-LINE              PIC S9(04) COMP VALUE +12.
      * File status for input and output files
       01  VKSDCUST-STATUS.
           05  VKSDCUST-STAT1      pic X.
           05  VKSDCUST-STAT2      pic X.
       01  QSAMREPT-STATUS.
           05  QSAMREPT-STAT1      pic X.
           05  QSAMREPT-STAT2      pic X.
      
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
      
      *-- Working zone for date manipulation
       01  WS-DATE.
           05  WS-DATE-CC          PIC X(002).
           05  WS-DATE-YY          PIC X(002).
           05  WS-DATE-MM          PIC X(002).
           05  WS-DATE-DD          PIC X(002).
       01  WE-DATE.
           05  WE-DATE-MM          PIC X(002).
           05  FILLER              PIC X(001) VALUE '/'.
           05  WE-DATE-DD          PIC X(002).
           05  FILLER              PIC X(001) VALUE '/'.
           05  WE-DATE-CC          PIC X(002).
           05  WE-DATE-YY          PIC X(002).
      
      *-- Description of the report title line
       01  W-TITLE.
           05  FILLER              PIC X(001) VALUE SPACE.
           05  FILLER              PIC X(008) VALUE 'RSSABB01'.
      
           05  FILLER              PIC X(038) VALUE SPACES.
           05  FILLER              PIC X(026) VALUE
               'Simple Sample Application'.
           05  FILLER              PIC X(039) VALUE SPACES.
           05  WT-DATE             PIC X(010).
      
      *-- Description of a customer's header printed data
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
      
      *-- Description of a customer's header printed data
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
      
      *-- Description of a customer's printed data
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
      * Files opening...
           DISPLAY "Opening input and output files..."
           OPEN OUTPUT SYSPRINT.
           PERFORM VKSDCUST-OPEN.
      
      * ------------------------------------------------------------- *
      * QSAM file's sweeping
           DISPLAY "Sweeping INPUT VSAM file..."
           PERFORM UNTIL END-OF-FILE = 'Y'
              PERFORM VKSDCUST-GET-NEXT
              IF END-OF-FILE = 'N'
                 PERFORM WRITE-REPORT
              END-IF
           END-PERFORM.
      
      * ------------------------------------------------------------- *
      * Files closing...
           DISPLAY "Closing INPUT and output files..."
           CLOSE SYSPRINT.
           PERFORM VKSDCUST-CLOSE.
      
           DISPLAY "Exiting program..."
           GOBACK.
      
      *---------------------------------------------------------------*
       VKSDCUST-GET-NEXT.
           READ VKSDCUST-FILE.
           IF  VKSDCUST-STATUS = '00'
               SUBTRACT APPL-RESULT FROM APPL-RESULT
           ELSE
               IF  VKSDCUST-STATUS = '10'
                   ADD 16 TO ZERO GIVING APPL-RESULT
               ELSE
                   ADD 12 TO ZERO GIVING APPL-RESULT
               END-IF
           END-IF.
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-FILE
               ELSE
                   MOVE 'VKSDCUST-FAILURE-GET...'
                     TO DISP-MSG
                   PERFORM Z-DISPLAY-DISP-MSG
                   MOVE VKSDCUST-STATUS TO IO-STATUS
                   PERFORM Z-DISPLAY-IO-STATUS
                   PERFORM Z-ABEND-PROGRAM
               END-IF
           END-IF.
           EXIT.
      
      *---------------------------------------------------------------*
       VKSDCUST-OPEN.
           ADD 8 TO ZERO giving APPL-RESULT
           OPEN INPUT VKSDCUST-FILE
           IF  VKSDCUST-STATUS = '00'
               SUBTRACT APPL-RESULT from APPL-RESULT
           ELSE
               ADD 12 TO ZERO giving APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               MOVE 'RSSABB01: VKSDCUST-FAILURE-OPEN...'
      
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
               MOVE 'RSSABB01: VKSDCUST-FAILURE-CLOSE...'
      
                 TO   DISP-MSG
               MOVE VKSDCUST-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-DISP-MSG
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       WRITE-REPORT.
           IF W-LINE > W-MAX-LINE THEN
              PERFORM WRITE-NEW-PAGE
           END-IF
      * Move data from VSAM to output file...
           MOVE QS-CUSTIDENT TO WC-IDENT
           MOVE QS-CUSTLNAME TO WC-LNAME
           MOVE QS-CUSTFNAME TO WC-FNAME
           MOVE QS-CUSTCITY  TO WC-CITY
           MOVE QS-CUSTPHONE TO WC-PHONE
           MOVE QS-CUSTBDATE TO WS-DATE
           MOVE WS-DATE-CC TO  WE-DATE-CC
           MOVE WS-DATE-YY TO  WE-DATE-YY
           MOVE WS-DATE-MM TO  WE-DATE-MM
           MOVE WS-DATE-DD TO  WE-DATE-DD
           MOVE WE-DATE    TO  WC-BDATE
           WRITE REPORT-REC FROM W-CUST-DETAIL
                 AFTER ADVANCING 1 LINE
           ADD 1 TO W-LINE
           EXIT.
      
      *---------------------------------------------------------------*
       WRITE-NEW-PAGE.
      * Title...
           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATE
           MOVE WS-DATE-CC TO  WE-DATE-CC
           MOVE WS-DATE-YY TO  WE-DATE-YY
           MOVE WS-DATE-MM TO  WE-DATE-MM
           MOVE WS-DATE-DD TO  WE-DATE-DD
           MOVE WE-DATE    TO  WT-DATE
           WRITE REPORT-REC FROM W-TITLE
                 AFTER ADVANCING PAGE
      * Header 1...
           WRITE REPORT-REC FROM W-CUST-HEADER1
                 AFTER ADVANCING 2 LINES
      * Header 2...
           WRITE REPORT-REC FROM W-CUST-HEADER2
                 AFTER ADVANCING 1 LINE
           MOVE 8 TO W-LINE
           EXIT.
      
      *---------------------------------------------------------------*
      * The following Z-Routines PERFORM administrative tasks         *
      * for this program.                                             *
      *---------------------------------------------------------------*
      
      *---------------------------------------------------------------*
      * ABEND the program, display a message and stop the program.    *
      *---------------------------------------------------------------*
       Z-ABEND-PROGRAM.
           IF  DISP-MSG not = SPACES
               PERFORM Z-DISPLAY-DISP-MSG
           END-IF
           MOVE 'RSSABB01: PROGRAM-IS-ABENDING...'   TO DISP-MSG
      
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
               DISPLAY '* RSSABB01 FILE-STATUS-' IO-STAT1 '/'
      
                       TWO-BYTES-BINARY
           ELSE
               DISPLAY '* RSSABB01: FILE-STATUS-' IO-STATUS
      
           END-IF
           EXIT.
      
      *---------------------------------------------------------------*
       Z-DISPLAY-DISP-MSG.
           DISPLAY DISP-MSG
           MOVE ALL SPACES TO DISP-MSG
           EXIT.
