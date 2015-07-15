       IDENTIFICATION DIVISION.
       PROGRAM-ID.  RSSBT001.
       AUTHOR. Metaware.
      * ------------------------------------------------------------- *
      * - Simple Sample Application                                   *
      *            Customers detailed information's inquiry           *
      * ------------------------------------------------------------- *
      * Description:                                                  *
      *    -This transactional program displays customer's data.      *
      *    -Customer's identifier is received thru the COMMAREA.      *
      *    -This program may be called directly from the Simple       *
      *     Sample Application main menu or from the customer's list  *
      *     screen. Hence, return will performed toward menu or list. *
      * ------------------------------------------------------------- *
      *    Map ....... : RSSBM01                                     *
      *    Transaction : SB01                                        *
      * ------------------------------------------------------------ *
      
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01  WORK-AREA.
           05  WS-DATE8                    PIC X(08).
           05  PGM-DEST                    PIC X(08).
           05  CUST-TABLE-KEY              PIC S9(06) COMP-3 VALUE +0.
           05  TIME-ABS                    PIC S9(15) COMP-3 VALUE +0.
           05  SCREEN-DATE.
               10 SCR-MM                   PIC X(02).
               10 FILLER                   PIC X(01) VALUE '/'.
               10 SCR-DD                   PIC X(02).
               10 FILLER                   PIC X(01) VALUE '/'.
               10 SCR-CCYY                 PIC X(04).
          05 WS-DATE-10.
               10 WS-DATE-10-CCYY          PIC X(004).
               10 FILLER                   PIC X(001) VALUE '-'.
               10 WS-DATE-10-MM            PIC X(002).
               10 FILLER                   PIC X(001) VALUE '-'.
               10 WS-DATE-10-DD            PIC X(002).
      
       01  SWITCHES.
           05  CUSTOMER-FOUND-SW           PIC X(01)  VALUE 'Y'.
               88  CUSTOMER-FOUND                     VALUE 'Y'.
      
       01  MSG-ERR-ENTRY                           PIC  X(79)
           VALUE 'This program may only be reached thru main menu (SB00
      -          'transaction)'.
      
       01  WS-SQLCODE          PIC -999.
           EXEC SQL INCLUDE SQLCA END-EXEC.
      
      * Logical MAP description          ---------------------
       01  FILLER              PIC X(16)  VALUE 'Logical Map  >>>'.
           COPY RSSBM01.
      
      * Communication area's description ---------------------
       01  FILLER              PIC X(16)  VALUE 'Commarea  --->>>'.
           COPY KUTSS001.
      
      * Customer DB2 table's data layout ---------------------
           EXEC SQL INCLUDE ODCSF0DB END-EXEC.
      
           COPY DFHAID.
      
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(50).
      
       PROCEDURE DIVISION.
       0000-PROCESS-CUSTOMER-INQUIRY.
      
      * This program may only be reached from menu or customers list...
           IF EIBCALEN = 0 THEN
              MOVE LOW-VALUE TO RSSBM01O
              MOVE  MSG-ERR-ENTRY TO VMESSO
              PERFORM SEND-MSG-END
           ELSE
              MOVE DFHCOMMAREA TO COMM-RECORD
           END-IF.
      
      * Evaluate process : check function
           EVALUATE COMM-FONC
              WHEN 'SEND'
                   PERFORM 1000-DISPLAY-SELECTED-CUSTOMER
              WHEN 'RECEIVE'
                   MOVE  0   TO COMM-NUM-MESS
                   MOVE 'OK' TO COMM-RETOUR
                   PERFORM RETURN-CALLING-PGM
              WHEN OTHER
      * Should never happen...
                   PERFORM 9999-TERMINATE-PROGRAM
           END-EVALUATE
      
      * Display screen...
      *    MOVE  COMM-DEST   TO  COMM-PROV
           MOVE 'RECEIVE'    TO  COMM-FONC
           EXEC CICS RETURN TRANSID('SB01')
                            COMMAREA(COMM-RECORD)
                            LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC
           .
      
      * --------------------------------------------------------
      * ! Get and display customer's data
      * --------------------------------------------------------
       1000-DISPLAY-SELECTED-CUSTOMER.
      
           PERFORM 1100-READ-CUSTOMER-RECORD
           PERFORM 1200-DISPLAY-INQUIRY-RESULTS
           .
      
      * --------------------------------------------------------
      * ! Get customer's data from customers DB2 table
      * --------------------------------------------------------
       1100-READ-CUSTOMER-RECORD.
      *
           MOVE COMM-CLT TO CUST-TABLE-KEY.
           EXEC SQL
                SELECT  CUSTIDENT,     CUSTLNAME,     CUSTFNAME,
                        CUSTADDRS,     CUSTCITY,      CUSTSTATE,
                        CUSTBDATE,     CUSTEMAIL,     CUSTPHONE
                INTO   :VS-CUSTIDENT, :VS-CUSTLNAME, :VS-CUSTFNAME ,
                       :VS-CUSTADDRS, :VS-CUSTCITY , :VS-CUSTSTATE,
                       :VS-CUSTBDATE, :VS-CUSTEMAIL, :VS-CUSTPHONE
                FROM    PJ01DB2.ODCSF0
                WHERE   CUSTIDENT = :CUST-TABLE-KEY
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
      
           MOVE SPACE TO VMESSO
           IF SQLCODE = 100
               MOVE  11  TO COMM-NUM-MESS
               MOVE 'KO' TO COMM-RETOUR
               PERFORM RETURN-CALLING-PGM
           ELSE
               IF SQLCODE NOT = +0
                  MOVE  WS-SQLCODE TO COMM-SQLCODE
                  MOVE  12         TO COMM-NUM-MESS
                  MOVE 'KO'        TO COMM-RETOUR
                  PERFORM RETURN-CALLING-PGM
               END-IF
           END-IF
           .
      
      * --------------------------------------------------------
      * ! Move data from table to the screen.
      * --------------------------------------------------------
       1200-DISPLAY-INQUIRY-RESULTS.
      * Move transaction and program names...
           MOVE EIBTRNID  TO VTRANSO
           MOVE 'RSSBT001' TO VPGMSCRO
      
      * Get current time and edit to output format
           EXEC CICS ASKTIME ABSTIME(TIME-ABS) END-EXEC
           EXEC CICS
                FORMATTIME ABSTIME(TIME-ABS)
                           DDMMYY(WS-DATE8)
                           DATESEP('-')
           END-EXEC
           MOVE WS-DATE8           TO VDATEO.
      * Move customer's data
           MOVE VS-CUSTIDENT       TO VIDCUSTO
           MOVE VS-CUSTLNAME       TO VLNAMEO
           MOVE VS-CUSTFNAME       TO VFNAMEO
           MOVE VS-CUSTADDRS       TO VADDRSO
           MOVE VS-CUSTCITY        TO VCITYO
           MOVE VS-CUSTSTATE       TO VSTATEO
           MOVE VS-CUSTBDATE       TO WS-DATE-10
           MOVE WS-DATE-10-CCYY    TO SCR-CCYY
           MOVE WS-DATE-10-MM      TO SCR-MM
           MOVE WS-DATE-10-DD      TO SCR-DD
           MOVE SCREEN-DATE        TO VBDATEO
           MOVE VS-CUSTEMAIL       TO VEMAILO
           MOVE VS-CUSTPHONE       TO VPHONEO
      * Initialize datas attributes
           MOVE '8' TO VPGMSCRA  VDATEA   VTRANSA  VMESSA.
           MOVE '8' TO VIDCUSTA  VLNAMEA  VFNAMEA  VADDRSA  VCITYA
                       VSTATEA   VBDATEA  VEMAILA  VPHONEA.
      * Display screen...
           EXEC CICS SEND MAP('RSSBM01')
                       MAPSET('RSSBM01') FROM(RSSBM01O) ERASE
           END-EXEC
           .
      
      *- Fatal error - Quit the application...
       SEND-MSG-END.
           MOVE 'A' TO VMESSA.
           EXEC CICS SEND MAP('RSSBM01') MAPSET('RSSBM01') ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC
           .
      
      *- RETURN TO THE CALLING PROGRAMM
       RETURN-CALLING-PGM.
           IF COMM-PROV = 'LIST'
              IF COMM-NUM-PG NOT = 1
                 SUBTRACT 1 FROM COMM-NUM-PG
                 MOVE COMM-DEB-PG TO COMM-DEB-PG-SVT
              ELSE
                 MOVE 0 TO COMM-NUM-PG
                 MOVE 1 TO COMM-DEB-PG-SVT
              END-IF
              MOVE  COMM-DEST  TO  COMM-PROV
              MOVE 'LIST'      TO  COMM-DEST
              MOVE 'RSSBT003'  TO  PGM-DEST
           ELSE
              MOVE  COMM-DEST  TO  COMM-PROV
              MOVE 'MENU'      TO  COMM-DEST
              MOVE 'RSSBT000'  TO  PGM-DEST
           END-IF.
           MOVE 'SEND' TO  COMM-FONC.
           PERFORM NEXT-TASK.
      
      *-----------
       NEXT-TASK.
           EXEC CICS XCTL   PROGRAM(PGM-DEST)
                            COMMAREA(COMM-RECORD)
                            LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC.
      
       9999-TERMINATE-PROGRAM.
           EXEC CICS ABEND ABCODE('META') END-EXEC
           .
