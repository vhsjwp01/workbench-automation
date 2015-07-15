       IDENTIFICATION DIVISION.
       PROGRAM-ID.  RSSAT001.
      
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
      *    Map ....... : RSSAM01                                     *
      *    Transaction : SA01                                        *
      * ------------------------------------------------------------ *
      
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01  WORK-AREA.
           05  PGM-DEST                    PIC X(08).
           05  CUST-FILE-KEY               PIC  9(06)        VALUE  0.
           05  RESPONSE-CODE               PIC S9(08) COMP   VALUE +0.
           05  TIME-ABS                    PIC S9(15) COMP-3 VALUE +0.
           05  SCREEN-DATE.
               10 SCR-MM                   PIC X(02).
               10 FILLER                   PIC X(01) VALUE '/'.
               10 SCR-DD                   PIC X(02).
               10 FILLER                   PIC X(01) VALUE '/'.
               10 SCR-CCYY                 PIC X(04).
      
       01  SWITCHES.
           05  CUSTOMER-FOUND-SW           PIC X(01)  VALUE 'Y'.
               88  CUSTOMER-FOUND                     VALUE 'Y'.
      
       01  MSG-ERR-ENTRY                           PIC  X(79)
           VALUE 'This program may only be reached thru main menu (SA00
      -          'transaction)'.
      
      * Logical MAP description          ---------------------
       01  FILLER              PIC X(16)  VALUE 'Logical Map  >>>'.
           COPY  RSSAM01.
      
      * Communication area's description ---------------------
       01  FILLER              PIC X(16)  VALUE 'Commarea  --->>>'.
           COPY  KUTSS001.
      
      
      * Customer VSAM file's data layout ---------------------
           COPY ODCSF0.
      *
           COPY DFHAID.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(50).
      
       PROCEDURE DIVISION.
       0000-PROCESS-CUSTOMER-INQUIRY.
      
      * This program may only be reached from menu or customers list...
           IF EIBCALEN = 0 THEN
              MOVE LOW-VALUE TO RSSAM01O
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
           EXEC CICS RETURN TRANSID('SA01')
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
      * ! Get customer's data from customers VSAM file
      * --------------------------------------------------------
       1100-READ-CUSTOMER-RECORD.
      *
           MOVE COMM-CLT TO CUST-FILE-KEY
           EXEC CICS
               READ FILE('ODCSF0')
                    INTO(QS-ODCSF0-RECORD)
                    RIDFLD(CUST-FILE-KEY)
                    LENGTH(LENGTH OF QS-ODCSF0-RECORD)
                    RESP(RESPONSE-CODE)
           END-EXEC
      *
           MOVE SPACE TO VMESSO
           IF RESPONSE-CODE = DFHRESP(NOTFND)
               MOVE  11  TO COMM-NUM-MESS
               MOVE 'KO' TO COMM-RETOUR
               PERFORM RETURN-CALLING-PGM
           ELSE
               IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
                   PERFORM 9999-TERMINATE-PROGRAM
               END-IF
           END-IF
           .
      
      * --------------------------------------------------------
      * ! Move data from file's record to the screen.
      * --------------------------------------------------------
       1200-DISPLAY-INQUIRY-RESULTS.
      * Move transaction and program names...
           MOVE EIBTRNID  TO VTRANSO
           MOVE 'RSSAT001' TO VPGMSCRO
      
      * Get current time and edit to output format
           EXEC CICS ASKTIME ABSTIME(TIME-ABS) END-EXEC
           EXEC CICS
                FORMATTIME ABSTIME(TIME-ABS)
                           DDMMYY(VDATEO)
                           DATESEP('-')
           END-EXEC
      * Move customer's data
           MOVE QS-CUSTIDENT       TO VIDCUSTO
           MOVE QS-CUSTLNAME       TO VLNAMEO
           MOVE QS-CUSTFNAME       TO VFNAMEO
           MOVE QS-CUSTADDRS       TO VADDRSO
           MOVE QS-CUSTCITY        TO VCITYO
           MOVE QS-CUSTSTATE       TO VSTATEO
           MOVE QS-CUSTBDATE(1:4)  TO SCR-CCYY
           MOVE QS-CUSTBDATE-MM    TO SCR-MM
           MOVE QS-CUSTBDATE-DD    TO SCR-DD
           MOVE SCREEN-DATE        TO VBDATEO
           MOVE QS-CUSTEMAIL       TO VEMAILO
           MOVE QS-CUSTPHONE       TO VPHONEO
      * Initialize datas attributes
           MOVE '8' TO VIDCUSTA  VLNAMEA  VFNAMEA  VADDRSA  VCITYA
                       VSTATEA   VBDATEA  VEMAILA  VPHONEA.
      * Display screen...
           EXEC CICS SEND MAP('RSSAM01')
                       MAPSET('RSSAM01') FROM(RSSAM01O) ERASE
           END-EXEC
           .
      
      *- Fatal error - Quit the application...
       SEND-MSG-END.
           MOVE 'A' TO VMESSA.
           EXEC CICS SEND MAP('RSSAM01') MAPSET('RSSAM01') ERASE
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
              MOVE 'RSSAT003'  TO  PGM-DEST
      
           ELSE
              MOVE  COMM-DEST  TO  COMM-PROV
              MOVE 'MENU'      TO  COMM-DEST
              MOVE 'RSSAT000'  TO  PGM-DEST
      
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
