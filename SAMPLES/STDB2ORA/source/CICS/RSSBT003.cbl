      * ------------------------------------------------------------ *
      *                Simple Sample Application                     *
      * > Customers list                                             *
      * ------------------------------------------------------------ *
      *    Map ....... : RSSBM03                                     *
      *    Transaction : SB03                                        *
      * ------------------------------------------------------------ *
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID. RSSBT003.
       AUTHOR. Metaware.
       DATE-WRITTEN. 20-07-2009.
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS 'F'.
       INPUT-OUTPUT                              SECTION.
      
       DATA                                      DIVISION.
       WORKING-STORAGE                           SECTION.
      *-------------------------------------------------*
      
       01  FILLER              PIC X(16)  VALUE 'Logical MAP  >>>'.
           COPY  RSSBM03.
      
       01  FILLER              PIC X(16)  VALUE 'DFHAID block >>>'.
           COPY  DFHAID.
      
       01  FILLER              PIC X(16)  VALUE 'COMMAREA  --->>>'.
           COPY  KUTSS001.
      
       01  FILLER              PIC X(16)  VALUE 'WORKING ST -->>>'.
       01  FILLER.
           05  WS-DATE8               PIC X(08).
           05  CUST-TABLE-KEY         PIC S9(06)V COMP-3.
           05  W-IND                  PIC S9(09) COMP.
           05  ABS-TIME               PIC S9(15) COMP-3.
           05  SEL-NBR                PIC  9(2).
           05  MSG-EXIT               PIC  X(79).
           05  MSG-LGTH               PIC S9(04) COMP   VALUE +79.
           05 WS-DATE-10.
              10 WS-DATE-10-CCYY      PIC X(04).
              10 FILLER               PIC X(01).
              10 WS-DATE-10-MM        PIC X(02).
              10 FILLER               PIC X(01).
              10 WS-DATE-10-DD        PIC X(02).
           05  SCREEN-LINE.
               10 IDCUST-LINE         PIC 9(06).
               10 FILLER1-LINE        PIC X(05).
               10 LNAME-LINE          PIC X(30).
               10 FILLER2-LINE        PIC X(01).
               10 FNAME-LINE          PIC X(20).
               10 FILLER3-LINE        PIC X(01).
               10 DBIRTH-LINE.
                  15 DBIRTH-MM-LINE   PIC X(02).
                  15 DBIRTH-SEP1      PIC X(01).
                  15 DBIRTH-DD-LINE   PIC X(02).
                  15 DBIRTH-SEP2      PIC X(01).
                  15 DBIRTH-CCYY-LINE PIC X(04).
      
      *--- data for SQL
       01  WS-SQLCODE          PIC -999.
           EXEC SQL INCLUDE SQLCA    END-EXEC.
           EXEC SQL INCLUDE ODCSF0DB END-EXEC.
      
           EXEC SQL
                DECLARE  CURS-DESC CURSOR FOR
                 SELECT  CUSTIDENT, CUSTLNAME, CUSTFNAME, CUSTBDATE
                   FROM  PJ01DB2.ODCSF0
                  WHERE  CUSTIDENT < :CUST-TABLE-KEY
               ORDER BY  CUSTIDENT DESC
           END-EXEC.
      
           EXEC SQL
                DECLARE  CURS-ASC  CURSOR FOR
                 SELECT  CUSTIDENT, CUSTLNAME, CUSTFNAME, CUSTBDATE
                   FROM  PJ01DB2.ODCSF0
                  WHERE  CUSTIDENT >= :CUST-TABLE-KEY
               ORDER BY  CUSTIDENT ASC
           END-EXEC.
      
      *--- Information messages
       01  MSG-END-TRN                             PIC  X(79)
           VALUE 'End of transaction.(SB00). Enter -CESF LOGOFF command
      -            'to quit.    '.
       01  MSG-UNAUTH                              PIC  X(79)
           VALUE 'E: this transaction is unauthorized.'.
       01  MSG-PGM-ERROR.
           05  FILLER                              PIC  X(35)
           VALUE 'E: technical error on program .:'.
           05  PGM-DEST                            PIC  X(08).
           05  FILLER                              PIC  X(36)
           VALUE '. unable to process.              '.
      
       01  MSG-TABLE.
           05  FILLER PIC X(79) VALUE 'New customer added.     '.
           05  FILLER PIC X(79) VALUE 'Creation canceled.      '.
           05  FILLER PIC X(79) VALUE 'Inquiry OK.             '.
           05  FILLER PIC X(79) VALUE 'Inquiry canceld.        '.
           05  FILLER PIC X(79) VALUE 'Customer deleted.       '.
           05  FILLER PIC X(79) VALUE 'Deletion canceled.      '.
           05  FILLER PIC X(79) VALUE 'Customer updated.       '.
           05  FILLER PIC X(79) VALUE 'Maintenance canceled.   '.
           05  FILLER PIC X(79) VALUE 'Back from customers list.'.
           05  FILLER PIC X(79) VALUE 'E: technical problem when accessi
      -                               'ng data in program -RSSBT001-.'.
           05  FILLER PIC X(79) VALUE 'I: unknomwn customer ID.   '.
           05  FILLER PIC X(79) VALUE 'Technical problem on table ODCSF0
      -                               ' SQLCODE='.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
       01  FILLER REDEFINES MSG-TABLE.
           05  MSG-ITEM                            PIC  X(79)
               OCCURS 20.
       01  MSG-ERR-ENTRY                           PIC  X(79)
           VALUE 'E: this program may only be reached from menu or custo
      -          'mers list.'.
       01  MSG-ERR-FUNCTION                        PIC  X(79)
           VALUE 'E: function not supported.                 '.
       01  MSG-ERR-PFKEY                           PIC  X(79)
           VALUE 'E: function key not supported.             '.
       01  MSG-ERR-1                               PIC  X(79)
           VALUE 'I: multi lines selection not supported.    '.
       01  MSG-ERR-2                               PIC  X(79)
           VALUE '                                           '.
       01  MSG-ERR-3                               PIC  X(79)
           VALUE '                                            '.
      
       LINKAGE                                   SECTION.
       01  DFHCOMMAREA                             PIC  X(50).
      
       PROCEDURE                                 DIVISION.
       MAIN                                      SECTION.
           MOVE  LOW-VALUE  TO  RSSBM03O.
      
      * Reject if not coming from menu or customers list...
           IF EIBCALEN  =  0
              MOVE LOW-VALUE TO RSSBM03O
              MOVE  MSG-ERR-ENTRY TO        VMESSO
              PERFORM SEND-MSG-END
           END-IF.
      
      * Get communication area's data
           MOVE DFHCOMMAREA TO COMM-RECORD.
      
      * Dispatch depending on first time call, or not...
           EVALUATE COMM-FONC
              WHEN 'SEND'       PERFORM FIRST-TIME
              WHEN 'RECEIVE'    PERFORM OTHER-TIME
              WHEN  OTHER       MOVE    MSG-ERR-FUNCTION TO VMESSO
                                PERFORM SEND-MSG-END
           END-EVALUATE.
      
      *-------------------------------------------------*
      *- First time entry...
       FIRST-TIME                                SECTION.
      * Get first page...
           PERFORM NEXT-PAGE.
      * Display map
           PERFORM PREPARE-ATTRIBUTES.
           PERFORM PREPARE-SCREEN.
           PERFORM DISPLAY-MAP.
           PERFORM TRANSID-RETURN.
      
      *-------------------------------------------------*
       OTHER-TIME                                SECTION.
           PERFORM RECEIVE-MAP.
           PERFORM PREPARE-ATTRIBUTES.
           PERFORM PREPARE-SCREEN.
           PERFORM SEND-MAP-WITH-CURS.
           PERFORM TRANSID-RETURN.
      
      *----------
       RECEIVE-MAP.
      *--- Get data from last input...
           EXEC CICS IGNORE CONDITION
                            MAPFAIL
           END-EXEC.
           EXEC CICS RECEIVE MAP   ('RSSBM03')
                             MAPSET('RSSBM03')
                             INTO  (RSSBM03I)
           END-EXEC.
      
      *--- Check data.
           PERFORM CHECK-DATA.
      
      *--- Check navigation...
           EVALUATE EIBAID
              WHEN DFHPF3
                   PERFORM BACK-TO-MENU
              WHEN DFHPF7
                   PERFORM PREVIOUS-PAGE
              WHEN DFHPF8
                   PERFORM NEXT-PAGE
              WHEN DFHENTER
                   IF SEL-NBR = 1
                      PERFORM XFER-CUSTOMER-INQ
                   END-IF
              WHEN DFHCLEAR
                   PERFORM LOAD-NEXT-PAGE
              WHEN OTHER
                   MOVE MSG-ERR-PFKEY TO VMESSO
           END-EVALUATE.
      
      *--------------
       CHECK-DATA.
           MOVE LOW-VALUE TO VMESSO.
           MOVE ZEROES    TO SEL-NBR.
      
      *--- Check selection column...
           IF  VSELID0L NOT =  0
           AND VSELID0I NOT =  SPACES
              MOVE   VLIBID0O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID1L NOT =  0
           AND VSELID1I NOT =  SPACES
              MOVE   VLIBID1O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID2L NOT =  0
           AND VSELID2I NOT =  SPACES
              MOVE   VLIBID2O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID3L NOT =  0
           AND VSELID3I NOT =  SPACES
              MOVE   VLIBID3O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID4L NOT =  0
           AND VSELID4I NOT =  SPACES
              MOVE   VLIBID4O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID5L NOT =  0
           AND VSELID5I NOT =  SPACES
              MOVE   VLIBID5O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID6L NOT =  0
           AND VSELID6I NOT =  SPACES
              MOVE   VLIBID6O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID7L NOT =  0
           AND VSELID7I NOT =  SPACES
              MOVE   VLIBID7O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID8L NOT =  0
           AND VSELID8I NOT =  SPACES
              MOVE   VLIBID8O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
           IF  VSELID9L NOT =  0
           AND VSELID9I NOT =  SPACES
              MOVE   VLIBID9O       TO SCREEN-LINE
              MOVE   IDCUST-LINE    TO COMM-CLT
              ADD    +1             TO SEL-NBR
           END-IF.
      * Only one selection is authorized...
           IF SEL-NBR > 1
              MOVE    MSG-ERR-1 TO  VMESSO
              MOVE    'I'  TO  VSELID0A  VSELID1A  VSELID2A  VSELID3A
                               VSELID4A  VSELID5A  VSELID6A  VSELID7A
                               VSELID8A  VSELID9A
              MOVE    -1   TO  VSELID0L
           END-IF.
      
      *----------
       PREPARE-SCREEN.
           MOVE   'RSSBT003'       TO      VPGMSCRO.
           MOVE    EIBTRNID        TO      VTRANSO.
      
           EXEC CICS ASKTIME ABSTIME(ABS-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)
                     DDMMYY(WS-DATE8)
                     DATESEP('-')
           END-EXEC.
           MOVE WS-DATE8 TO VDATEO.
      
      *-----------
       DISPLAY-MAP.
           EXEC CICS SEND MAP('RSSBM03')
                          MAPSET('RSSBM03')
                          ERASE
           END-EXEC.
      
      *----------------
       SEND-MAP-WITH-CURS.
           EXEC CICS SEND MAP('RSSBM03')
                          MAPSET('RSSBM03')
                          CURSOR
                          ERASE
           END-EXEC.
      
      *-----------
       PREVIOUS-PAGE.
           IF COMM-NUM-PG = 1
              MOVE 'I: already on first page.          ' TO VMESSO
           ELSE
              MOVE LOW-VALUE TO VLIBID0O VLIBID1O VLIBID2O VLIBID3O
              MOVE LOW-VALUE TO VLIBID4O VLIBID5O VLIBID6O VLIBID7O
              MOVE LOW-VALUE TO VLIBID8O VLIBID9O
              SUBTRACT 1                FROM COMM-NUM-PG
              MOVE     COMM-DEB-PG      TO   COMM-DEB-PG-SVT
              PERFORM  LOAD-PREVIOUS-PAGE
           END-IF.
      
      *-----------
       NEXT-PAGE.
           IF COMM-FONC NOT = 'SEND'
           AND ((VLIBID9O = SPACES OR LOW-VALUE) OR
                (VLIBID9O NOT =  SPACES AND COMM-DEB-PG-SVT = 0))
               MOVE 'I: already on the last page.       ' TO VMESSO
           ELSE
              MOVE LOW-VALUE TO VLIBID0O VLIBID1O VLIBID2O VLIBID3O
              MOVE LOW-VALUE TO VLIBID4O VLIBID5O VLIBID6O VLIBID7O
              MOVE LOW-VALUE TO VLIBID8O VLIBID9O
              ADD      1                    TO   COMM-NUM-PG
              MOVE     COMM-DEB-PG-SVT      TO   COMM-DEB-PG
              PERFORM  LOAD-NEXT-PAGE
           END-IF.
      
      *----------
       LOAD-PREVIOUS-PAGE.
      * Start browse from saved key...
           MOVE  COMM-DEB-PG-SVT   TO CUST-TABLE-KEY.
           EXEC SQL
                OPEN CURS-DESC
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
           IF SQLCODE NOT = +0
              PERFORM CUSTOMER-TABLE-ERROR
           END-IF.
      
      * Reading backward...
           MOVE 0  TO W-IND.
           PERFORM UNTIL W-IND > 10
              EXEC SQL
                 FETCH  CURS-DESC
                  INTO :VS-CUSTIDENT,  :VS-CUSTLNAME,
                       :VS-CUSTFNAME,  :VS-CUSTBDATE
              END-EXEC
              MOVE SQLCODE TO WS-SQLCODE
              IF SQLCODE = +0
                 ADD +1 TO W-IND
                 PERFORM LOAD-TABLE-TO-LINES
                 EVALUATE W-IND
                    WHEN   1   MOVE SCREEN-LINE TO VLIBID9O
                    WHEN   2   MOVE SCREEN-LINE TO VLIBID8O
                    WHEN   3   MOVE SCREEN-LINE TO VLIBID7O
                    WHEN   4   MOVE SCREEN-LINE TO VLIBID6O
                    WHEN   5   MOVE SCREEN-LINE TO VLIBID5O
                    WHEN   6   MOVE SCREEN-LINE TO VLIBID4O
                    WHEN   7   MOVE SCREEN-LINE TO VLIBID3O
                    WHEN   8   MOVE SCREEN-LINE TO VLIBID2O
                    WHEN   9   MOVE SCREEN-LINE TO VLIBID1O
                    WHEN  10   MOVE SCREEN-LINE TO VLIBID0O
                               MOVE VS-CUSTIDENT TO COMM-DEB-PG
                 END-EVALUATE
              ELSE
                 MOVE 11 TO W-IND
              END-IF
           END-PERFORM.
      
      * End of browse...
           EXEC SQL
                CLOSE CURS-DESC
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
           IF SQLCODE NOT = +0
              PERFORM CUSTOMER-TABLE-ERROR
           END-IF.
      
      *----------
       LOAD-NEXT-PAGE.
      * Start from saved key...
           MOVE  COMM-DEB-PG       TO CUST-TABLE-KEY.
           EXEC SQL
               OPEN CURS-ASC
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
           IF SQLCODE NOT = +0
              PERFORM CUSTOMER-TABLE-ERROR
           END-IF.
      
      * Reading forward...
           MOVE 0      TO W-IND.
           PERFORM UNTIL W-IND > 11
              EXEC SQL
                 FETCH  CURS-ASC
                  INTO :VS-CUSTIDENT,  :VS-CUSTLNAME,
                       :VS-CUSTFNAME,  :VS-CUSTBDATE
              END-EXEC
              MOVE SQLCODE TO WS-SQLCODE
              IF SQLCODE = +0
                 ADD +1 TO W-IND
                 PERFORM LOAD-TABLE-TO-LINES
                 EVALUATE W-IND
                    WHEN   1   MOVE SCREEN-LINE TO VLIBID0O
                    WHEN   2   MOVE SCREEN-LINE TO VLIBID1O
                    WHEN   3   MOVE SCREEN-LINE TO VLIBID2O
                    WHEN   4   MOVE SCREEN-LINE TO VLIBID3O
                    WHEN   5   MOVE SCREEN-LINE TO VLIBID4O
                    WHEN   6   MOVE SCREEN-LINE TO VLIBID5O
                    WHEN   7   MOVE SCREEN-LINE TO VLIBID6O
                    WHEN   8   MOVE SCREEN-LINE TO VLIBID7O
                    WHEN   9   MOVE SCREEN-LINE TO VLIBID8O
                    WHEN  10   MOVE SCREEN-LINE TO VLIBID9O
                    WHEN  11   MOVE VS-CUSTIDENT TO COMM-DEB-PG-SVT
                 END-EVALUATE
              ELSE
                 IF W-IND < 11
                    MOVE 0 TO COMM-DEB-PG-SVT
                 END-IF
                 MOVE 12 TO W-IND
              END-IF
           END-PERFORM.
      * End of browse...
           EXEC SQL
              CLOSE CURS-ASC
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
           IF SQLCODE NOT = +0
              PERFORM CUSTOMER-TABLE-ERROR
           END-IF.
      
      
       PREPARE-ATTRIBUTES.
           MOVE  '8'   TO VPGMSCRA  VDATEA    VTRANSA   VMESSA.
           MOVE  'A'   TO VSELID0A  VSELID1A  VSELID2A  VSELID3A
                          VSELID4A  VSELID5A  VSELID6A  VSELID7A
                          VSELID8A  VSELID9A.
      
      *----------
      * Move data from WSAM record to screen line
       LOAD-TABLE-TO-LINES.
           MOVE  VS-CUSTIDENT       TO IDCUST-LINE.
           MOVE  VS-CUSTLNAME       TO LNAME-LINE.
           MOVE  VS-CUSTFNAME       TO FNAME-LINE.
           MOVE  VS-CUSTBDATE       TO WS-DATE-10.
           MOVE  WS-DATE-10-DD      TO DBIRTH-DD-LINE.
           MOVE  WS-DATE-10-MM      TO DBIRTH-MM-LINE.
           MOVE  WS-DATE-10-CCYY    TO DBIRTH-CCYY-LINE.
           MOVE '/'                 TO DBIRTH-SEP1    DBIRTH-SEP2.
           MOVE SPACES              TO FILLER1-LINE   FILLER2-LINE.
           MOVE SPACES              TO FILLER3-LINE.
      
      *---------------------
      * Transfer control to customer's inquiry program...
       XFER-CUSTOMER-INQ.
           MOVE  COMM-DEST  TO  COMM-PROV.
           MOVE 'INT '      TO  COMM-DEST.
           MOVE 'SEND'      TO  COMM-FONC.
           MOVE 'RSSBT001'  TO  PGM-DEST.
           PERFORM XFER-NEXT-PGM.
      
      *---------------------
       CUSTOMER-TABLE-ERROR.
           MOVE  COMM-DEST  TO  COMM-PROV.
           MOVE 'MENU'      TO  COMM-DEST.
           MOVE 'SEND'      TO  COMM-FONC.
           MOVE 'KO'        TO  COMM-RETOUR.
           MOVE  WS-SQLCODE TO  COMM-SQLCODE.
           MOVE  12         TO  COMM-NUM-MESS.
           MOVE 'RSSBT000'  TO  PGM-DEST.
           PERFORM XFER-NEXT-PGM.
      
      *---------------------
       BACK-TO-MENU.
           MOVE  COMM-DEST  TO  COMM-PROV.
           MOVE 'MENU'      TO  COMM-DEST.
           MOVE 'SEND'      TO  COMM-FONC.
           MOVE 'OK'        TO  COMM-RETOUR.
           MOVE  0          TO  COMM-NUM-MESS.
           MOVE 'RSSBT000'  TO  PGM-DEST.
           PERFORM XFER-NEXT-PGM.
      
      *-----------
       XFER-NEXT-PGM.
      * Errors handling...
           EXEC CICS HANDLE CONDITION
                            PGMIDERR(PGM-NOT-FOUND)
           END-EXEC.
      * Transfer control to 'PGM-DEST' program...
           EXEC CICS XCTL   PROGRAM(PGM-DEST)
                            COMMAREA(COMM-RECORD)
                            LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC.
      
      *---------------
       TRANSID-RETURN.
           MOVE  COMM-DEST   TO  COMM-PROV.
           MOVE 'RECEIVE'    TO  COMM-FONC.
           EXEC CICS RETURN TRANSID('SB03')
                            COMMAREA(COMM-RECORD)
                            LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC.
      
      *---------
       PGM-NOT-FOUND.
           MOVE    MSG-PGM-ERROR   TO      VMESSO.
           PERFORM SEND-MSG-END.
      
      *---------------
      * Display a message on screen...
       SEND-MSG-END.
           MOVE 'A' TO VMESSA.
           EXEC CICS SEND MAP('RSSBM03')
                          MAPSET('RSSBM03')
                          ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
      
      *--------------
       JUST-IN-CASE.
           GOBACK.
