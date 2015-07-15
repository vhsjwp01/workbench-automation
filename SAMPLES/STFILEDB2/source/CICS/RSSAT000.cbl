      * ------------------------------------------------------------ *
      * - Simple Sample Application                                  *
      *              Customers maintenance main menu                 *
      * ------------------------------------------------------------ *
      *    Map ....... : RSSAM00                                     *
      *    Transaction : SA00                                        *
      * ------------------------------------------------------------ *
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID. RSSAT000.
       AUTHOR. Metaware.
       DATE-WRITTEN. 17-07-2009.
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
       01  FILLER              PIC X(16)  VALUE 'Logical MAP   >>'.
           COPY  RSSAM00.
      
       01  FILLER              PIC X(16)  VALUE 'DFHAID block  >>'.
           COPY  DFHAID.
      
       01  FILLER              PIC X(16)  VALUE 'Customer COPY >>'.
           COPY  ODCSF0.
      
       01  FILLER              PIC X(16)  VALUE 'COMMAREA  --->>>'.
           COPY  KUTSS001.
      
       01  FILLER              PIC X(16)  VALUE 'WORKING   --->>>'.
       01  FILLER.
           05  ABS-TIME        PIC S9(15) COMP-3.
           05  TOP-CLT         PIC  9            VALUE 0.
               88  ERR-CLT                       VALUE 1.
               88  CLT-OK                        VALUE 0.
           05  MSG-END         PIC  X(79).
           05  MSG-LGTH        PIC S9(04) COMP   VALUE +79.
      
      *--- informational messages -------------
       01  MSG-EXIT                                PIC  X(80)
           VALUE 'Transaction END(SA00). Enter -CESF LOGOFF- to quit.
      -            '             '.
       01  MSG-UNAUTH                              PIC  X(80)
           VALUE 'Error : unauthorized transaction.
      -            '             '.
       01  MSG-PGM-ERROR.
           05  FILLER                              PIC  X(35)
           VALUE 'Error on program ..............:'.
           05  PGM-DEST                            PIC  X(08).
           05  FILLER                              PIC  X(37)
           VALUE '.... Access unavailable .........!'.
      
       01  MSG-TABLE.
           05  FILLER PIC X(79) VALUE 'Creation confirmed      '.
           05  FILLER PIC X(79) VALUE 'Creation canceled       '.
           05  FILLER PIC X(79) VALUE 'Inquiry OK              '.
           05  FILLER PIC X(79) VALUE 'Inquiry canceled        '.
           05  FILLER PIC X(79) VALUE 'Deletion confirmed      '.
           05  FILLER PIC X(79) VALUE 'Deletion canceled       '.
           05  FILLER PIC X(79) VALUE 'Maintenance confirmed   '.
           05  FILLER PIC X(79) VALUE 'Maintenance canceled    '.
           05  FILLER PIC X(79) VALUE 'Back from customers list'.
           05  FILLER PIC X(79) VALUE 'Problem in access program (RSSAT0
      -                               '01)         '.
           05  FILLER PIC X(79) VALUE 'Unreferenced customer
      -                               '       '.
           05  FILLER PIC X(79) VALUE 'Customers file unavailable  '.
           05  FILLER PIC X(79) VALUE 'Technical on calles VSAM function
      -                               '    '.
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
      
       01  MSG-BAD-FUNCTION                        PIC  X(79)
           VALUE 'E: function not supported.       '.
       01  MSG-BAD-PFKEY                           PIC  X(79)
           VALUE 'E: function key not supported.   '.
       01  MSG-BAD-CHOICE-1                        PIC  X(79)
           VALUE 'Please, enter your choice.                 '.
       01  MSG-BAD-CHOICE-2                        PIC  X(79)
           VALUE 'Sorry, this choice is not supported.       '.
       01  MSG-1                                   PIC  X(79)
           VALUE 'Do not enter a customer ID for this function.'.
       01  MSG-2                                   PIC  X(79)
           VALUE 'Please, enter a valid customer ID.         '.
       01  MSG-3                                   PIC  X(79)
           VALUE 'You entered an invalid customer ID number. '.
      
       LINKAGE                                   SECTION.
       01  DFHCOMMAREA                             PIC  X(50).
      
       PROCEDURE                                 DIVISION.
      
      * ----------------------------------------------- *
       MAIN                                      SECTION.
           MOVE LOW-VALUE TO RSSAM00O
           IF   EIBCALEN = 0
                PERFORM FIRST-TIME
           ELSE
                PERFORM SECOND-TIME
           END-IF.
      
      * ----------------------------------------------- *
       FIRST-TIME                                SECTION.
           MOVE    'A'     TO      CIDCUSTA
           MOVE    '8'     TO      VMESSA
           PERFORM FILL-SCREEN
           PERFORM DISPLAY-MENU
           PERFORM TRANSID-RETURN
           .
      
      * ----------------------------------------------- *
       SECOND-TIME                               SECTION.
           MOVE  DFHCOMMAREA     TO      COMM-RECORD.
      
      *    IF EIBAID  =       DFHCLEAR
      *       MOVE    MSG-EXIT        TO      VMESSO
      *       PERFORM SEND-MSG-END
      *    END-IF.
      
           IF COMM-FONC  =  'SEND'
              MOVE    SPACES  TO    VMESSO
              IF  COMM-NUM-MESS   >   0
              AND COMM-NUM-MESS   <  21
                 MOVE MSG-ITEM(COMM-NUM-MESS) TO VMESSO
              END-IF
              MOVE COMM-CLT TO CIDCUSTO
              PERFORM FIRST-TIME
           END-IF.
      
           IF COMM-FONC  =  'RECEIVE'
              PERFORM RECEIVE-MAP
           ELSE
              MOVE    MSG-BAD-FUNCTION TO VMESSO
              PERFORM SEND-MSG-END
           END-IF.
      
           PERFORM PROCESS-MAP-FIELDS.
           PERFORM FILL-SCREEN.
           PERFORM DISPLAY-MENU-WITH-CURSOR.
           PERFORM TRANSID-RETURN.
      
      * ---------
       RECEIVE-MAP.
           EXEC CICS
                IGNORE CONDITION   MAPFAIL
           END-EXEC.
           EXEC CICS
                RECEIVE MAP('RSSAM00')
                        MAPSET('RSSAM00')
                        INTO  (RSSAM00I)
           END-EXEC.
      
      *--- Attributes initialization
           MOVE    'A'     TO      CIDCUSTA
           MOVE    '8'     TO      VMESSA.
      
      *--- Function key evaluation
      *      -Process depends on PFKEY's value...
           EVALUATE EIBAID
              WHEN DFHPF3
                   MOVE MSG-EXIT     TO  VMESSO
                   PERFORM SEND-MSG-END
              WHEN DFHPF4
                   PERFORM CTRL-PROCESS-LIST
              WHEN DFHPF5
                   PERFORM CTRL-NEW-CUSTOMER-PROCESS
              WHEN DFHPF6
                   PERFORM CTRL-UPD-CUSTOMER-PROCESS
              WHEN DFHPF7
                   PERFORM CTRL-DEL-CUSTOMER-PROCESS
              WHEN DFHPF8
                   PERFORM CTRL-INQ-CUSTOMER-PROCESS
              WHEN DFHENTER
                   CONTINUE
              WHEN OTHER
                   MOVE    MSG-BAD-PFKEY TO    VMESSO
           END-EVALUATE.
      
      * - Control data before transfering to customers list.
       CTRL-PROCESS-LIST.
           MOVE    ZEROES  TO      TOP-CLT.
           IF  CIDCUSTL NOT = 0
           AND CIDCUSTI NOT = ZEROES
           AND CIDCUSTI NOT = SPACES
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-1   TO      VMESSO
           END-IF.
      
           IF CLT-OK
              MOVE    'MENU'      TO  COMM-PROV
              MOVE    'LIST'      TO  COMM-DEST
              MOVE    'SEND'      TO  COMM-FONC
              MOVE    SPACES      TO  COMM-RETOUR
              MOVE     0          TO  COMM-NUM-MESS
              MOVE     0          TO  COMM-CLT
              MOVE     0          TO  COMM-NUM-PG
              MOVE     0          TO  COMM-DEB-PG
              MOVE     1          TO  COMM-DEB-PG-SVT
              MOVE    'RSSAT003'  TO  PGM-DEST
              PERFORM TRANSFER-CONTROL
           END-IF.
      
      * - Control data before processing new customer
       CTRL-NEW-CUSTOMER-PROCESS.
      
           MOVE    ZEROES  TO      TOP-CLT.
           IF  CIDCUSTL NOT = 0
           AND CIDCUSTI NOT = ZEROES
           AND CIDCUSTI NOT = SPACES
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-1   TO      VMESSO
           END-IF.
      
           IF CLT-OK
              MOVE    'MENU'      TO  COMM-PROV
              MOVE    'CREA'      TO  COMM-DEST
              MOVE    'SEND'      TO  COMM-FONC
              MOVE    SPACES      TO  COMM-RETOUR
              MOVE     0          TO  COMM-NUM-MESS
              MOVE     0          TO  COMM-CLT
              MOVE     0          TO  COMM-NUM-PG
              MOVE     0          TO  COMM-DEB-PG
              MOVE     0          TO  COMM-DEB-PG-SVT
              MOVE    'RSSAT002'  TO  PGM-DEST
              PERFORM TRANSFER-CONTROL
           END-IF.
      
      * - Control data before transfering to customer's maintenance
       CTRL-UPD-CUSTOMER-PROCESS.
      
           MOVE    ZEROES  TO      TOP-CLT.
           IF CIDCUSTL = 0
           OR CIDCUSTI = ZEROES
           OR CIDCUSTI = SPACES
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-2   TO      VMESSO
           END-IF.
      
           IF  CIDCUSTL NOT = 0
           AND CIDCUSTI NOT NUMERIC
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-3   TO      VMESSO
           END-IF.
      
           MOVE    CIDCUSTI    TO  CIDCUSTO.
      
           IF CLT-OK
              MOVE    'MENU'      TO  COMM-PROV
              MOVE    'MAJ '      TO  COMM-DEST
              MOVE    'SEND'      TO  COMM-FONC
              MOVE    SPACES      TO  COMM-RETOUR
              MOVE     0          TO  COMM-NUM-MESS
              MOVE    CIDCUSTI    TO  COMM-CLT
              MOVE     0          TO  COMM-NUM-PG
              MOVE     0          TO  COMM-DEB-PG
              MOVE     0          TO  COMM-DEB-PG-SVT
              MOVE    'RSSAT002'  TO  PGM-DEST
              PERFORM TRANSFER-CONTROL
           END-IF.
      
      * - Control data before processing to customer's suppression
       CTRL-DEL-CUSTOMER-PROCESS.
      
           MOVE    ZEROES  TO      TOP-CLT.
           IF CIDCUSTL = 0
           OR CIDCUSTI = ZEROES
           OR CIDCUSTI = SPACES
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-2   TO      VMESSO
           END-IF.
      
           IF  CIDCUSTL NOT = 0
           AND CIDCUSTI NOT NUMERIC
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-3   TO      VMESSO
           END-IF.
      
           MOVE    CIDCUSTI    TO  CIDCUSTO.
      
           IF CLT-OK
              MOVE    'MENU'      TO  COMM-PROV
              MOVE    'SUPP'      TO  COMM-DEST
              MOVE    'SEND'      TO  COMM-FONC
              MOVE    SPACES      TO  COMM-RETOUR
              MOVE     0          TO  COMM-NUM-MESS
              MOVE    CIDCUSTI    TO  COMM-CLT
              MOVE     0          TO  COMM-NUM-PG
              MOVE     0          TO  COMM-DEB-PG
              MOVE     0          TO  COMM-DEB-PG-SVT
              MOVE    'RSSAT002'  TO  PGM-DEST
              PERFORM TRANSFER-CONTROL
           END-IF.
      
      * - Control data before processing to customer's inquiry
       CTRL-INQ-CUSTOMER-PROCESS.
      
           MOVE    ZEROES  TO      TOP-CLT.
           IF CIDCUSTL = 0
           OR CIDCUSTI = ZEROES
           OR CIDCUSTI = SPACES
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-2   TO      VMESSO
           END-IF.
      
           IF  CIDCUSTL NOT = 0
           AND CIDCUSTI NOT NUMERIC
               MOVE    1       TO      TOP-CLT
               MOVE    MSG-3   TO      VMESSO
           END-IF.
      
           MOVE    CIDCUSTI    TO  CIDCUSTO.
      
           IF CLT-OK
              MOVE    'MENU'      TO  COMM-PROV
              MOVE    'INT '      TO  COMM-DEST
              MOVE    'SEND'      TO  COMM-FONC
              MOVE    SPACES      TO  COMM-RETOUR
              MOVE     0          TO  COMM-NUM-MESS
              MOVE    CIDCUSTI    TO  COMM-CLT
              MOVE     0          TO  COMM-NUM-PG
              MOVE     0          TO  COMM-DEB-PG
              MOVE     0          TO  COMM-DEB-PG-SVT
              MOVE    'RSSAT001'  TO  PGM-DEST
              PERFORM TRANSFER-CONTROL
           END-IF.
      
      *- Processing fields before displaying the map...
       PROCESS-MAP-FIELDS.
      
           MOVE    -1      TO      CIDCUSTL.
           IF ERR-CLT
              MOVE    'R'     TO   CIDCUSTA
              MOVE    -1      TO   CIDCUSTL
           END-IF.
      
      *- Move data to the map....
       FILL-SCREEN.
           MOVE    EIBTRNID        TO      VTRANSO.
           MOVE   'RSSAT000'       TO      VPGMSCRO.
      
           EXEC CICS ASKTIME ABSTIME(ABS-TIME)  END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)
                                DDMMYY(VDATEO) DATESEP('-')
           END-EXEC.
      
      *- Display menu screen...
       DISPLAY-MENU.
           EXEC CICS
                SEND MAP('RSSAM00')
                     MAPSET('RSSAM00')
                     ERASE
           END-EXEC.
      
      *- Display Menu screen with cursor...
       DISPLAY-MENU-WITH-CURSOR.
           EXEC CICS
                SEND MAP('RSSAM00')
                     MAPSET('RSSAM00')
                     CURSOR
                     ERASE
           END-EXEC.
      
      * Transfer to next function depending on user's choice
       TRANSFER-CONTROL.
      
      * Prepare for error trapping...
           EXEC CICS
                HANDLE CONDITION PGMIDERR(PGM-NOTFOUND)
           END-EXEC.
      
      * Transfer control to next program
           EXEC CICS
                XCTL PROGRAM(PGM-DEST)
                     COMMAREA(COMM-RECORD)
                     LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC.
      
      * Resending the menu screen
       TRANSID-RETURN.
           MOVE 'MENU'       TO  COMM-PROV   COMM-DEST.
           MOVE 'RECEIVE'    TO  COMM-FONC.
           EXEC CICS
                RETURN TRANSID('SA00')
                       COMMAREA(COMM-RECORD)
                       LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC.
      
      *- This paragraph is called whenever a 'not found program error'
      *  happens...
       PGM-NOTFOUND.
           MOVE    MSG-PGM-ERROR   TO      VMESSO.
           PERFORM SEND-MSG-END.
      
      *- Display map with a message...
       SEND-MSG-END.
           MOVE 'A' TO VMESSA.
           EXEC CICS
                SEND MAP('RSSAM00')
                     MAPSET('RSSAM00')
                     ERASE
           END-EXEC.
      
      * Surrender control...
           EXEC CICS RETURN END-EXEC.
      
      *--------------
       JUST-IN-CASE.
           GOBACK.
