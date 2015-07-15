      * ------------------------------------------------------------- *
      * - Simple Sample Application                                   *
      *            Customers data maintenance                         *
      * ------------------------------------------------------------- *
      * Description:                                                  *
      *    -This transactional program performs customers maintenance *
      *     > Adding a new customer                                   *
      *     > Updating an existing customer                           *
      *     > Deleting an existing customer                           *
      *    -Customer's identifier is received thru the COMMAREA.      *
      * ------------------------------------------------------------- *
      *    Map ....... : RSSAM02                                      *
      *    Transaction : MT02                                         *
      * ------------------------------------------------------------- *
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID. RSSAT002.
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
      
       01  FILLER              PIC X(16)  VALUE 'Logical Map  >>>'.
           COPY  RSSAM02.
      
       01  FILLER              PIC X(16)  VALUE 'DFHAID block >>>'.
           COPY  DFHAID.
      
       01  FILLER              PIC X(16)  VALUE 'Cust copybook 1>'.
           COPY  ODCSF0B.
      
       01  FILLER              PIC X(16)  VALUE 'Cust copybook 2>'.
           COPY  ODCSF0.
      
       01  FILLER              PIC X(16)  VALUE 'Commarea  --->>>'.
           COPY  KUTSS001.
      
       01  FILLER              PIC X(16)  VALUE 'Workaing ST ->>>'.
       01  FILLER.
           05  RESPONSE-CODE               PIC S9(08) COMP   VALUE +0.
           05  CUST-FILE-KEY   PIC  9(06).
           05  ABS-TIME        PIC S9(15) COMP-3.
           05  ERR-NBR         PIC S9(03) COMP-3 VALUE 0.
           05  TAG-IDCUST      PIC  9            VALUE 0.
               88  ERR-IDCUST                    VALUE 1.
               88  IDCUST-OK                     VALUE 0.
           05  TAG-LNAME       PIC  9            VALUE 0.
               88  ERR-LNAME                     VALUE 1.
               88  LNAME-OK                      VALUE 0.
           05  TAG-FNAME       PIC  9            VALUE 0.
               88  ERR-FNAME                     VALUE 1.
               88  FNAME-OK                      VALUE 0.
           05  TAG-ADDRES      PIC  9            VALUE 0.
               88  ERR-ADDRES                    VALUE 1.
               88  ADDRES-OK                     VALUE 0.
           05  TAG-CITY        PIC  9            VALUE 0.
               88  ERR-CITY                      VALUE 1.
               88  CITY-OK                       VALUE 0.
           05  TAG-STATE       PIC  9            VALUE 0.
               88  ERR-STATE                     VALUE 1.
               88  STATE-OK                      VALUE 0.
           05  TAG-DBIRTH      PIC  9            VALUE 0.
               88  ERR-DBIRTH                    VALUE 1.
               88  DBIRTH-OK                     VALUE 0.
           05  TAG-EMAIL       PIC  9            VALUE 0.
               88  ERR-EMAIL                     VALUE 1.
               88  EMAIL-OK                      VALUE 0.
           05  TAG-PHONE       PIC  9            VALUE 0.
               88  ERR-PHONE                     VALUE 1.
               88  PHONE-OK                      VALUE 0.
           05  FINAL-MSG       PIC  X(79).
           05  MSG-LGTH        PIC S9(04) COMP   VALUE +79.
           05  SCREEN-DATE.
             10 SCR-MM         PIC X(02).
               88 MONTH-OK VALUE '01' '02' '03' '04' '05' '06'
                                 '07' '08' '09' '10' '11' '12'.
             10 SCR-MM9        REDEFINES SCR-MM PIC 9(02).
             10 SCR-SEP1       PIC X(01).
             10 SCR-DD         PIC X(02).
               88 DAY-OK VALUE '01' '02' '03' '04' '05' '06' '07' '08'
                               '09' '10' '11' '12' '10' '11' '12' '13'
                               '14' '15' '16' '17' '18' '19' '20' '21'
                               '21' '22' '23' '24' '25' '26' '27' '28'
                               '29' '30' '31'.
             10 SCR-DD9        REDEFINES SCR-DD PIC 9(02).
             10 SCR-SEP2       PIC X(01).
             10 SCR-CCYY       PIC X(04).
             10 SCR-CCYY9      REDEFINES SCR-CCYY PIC 9(04).
      
      *--- Informational messages
       01  MSG-EXIT                                PIC  X(80)
           VALUE 'Transaction ending(MT00). Enter -CESF LOGOFF- command
      -            'to quit.     '.
      
       01  MSG-UNAUTH                              PIC  X(80)
           VALUE 'Error : unauthorized transaction
      -            '             '.
       01  MSG-VSAM-ERROR                          PIC  X(80)
           VALUE 'Error : access to customer file unavailable
      -            '             '.
       01  MSG-PGM-ERROR.
           05  FILLER                              PIC  X(35)
           VALUE 'Technical problem on program ..:'.
           05  PGM-DEST                            PIC  X(08).
           05  FILLER                              PIC  X(37)
           VALUE ' (cannot be reached).             '.
      
       01  MSG-TABLE.
           05  FILLER PIC X(79) VALUE 'New customer added             '.
           05  FILLER PIC X(79) VALUE 'Creation canceled              '.
           05  FILLER PIC X(79) VALUE 'Query OK                       '.
           05  FILLER PIC X(79) VALUE 'Inquiry canceled by user       '.
           05  FILLER PIC X(79) VALUE 'User deletion confirmed        '.
           05  FILLER PIC X(79) VALUE 'Deletion canceled              '.
           05  FILLER PIC X(79) VALUE 'Maintenance OK                 '.
           05  FILLER PIC X(79) VALUE 'Maintenance canceled           '.
           05  FILLER PIC X(79) VALUE 'Back from customers list       '.
           05  FILLER PIC X(79) VALUE 'Technical problem in RSSAT001 pro
      -                               'gram.       '.
           05  FILLER PIC X(79) VALUE 'Customer not found on this identi
      -                               'fication number'.
           05  FILLER PIC X(79) VALUE 'Customers file not open.       '.
           05  FILLER PIC X(79) VALUE 'This ID is already used.       '.
           05  FILLER PIC X(79) VALUE 'Datas OK. Press PF12 to confirm'.
           05  FILLER PIC X(79) VALUE '                               '.
           05  FILLER PIC X(79) VALUE '                               '.
           05  FILLER PIC X(79) VALUE '                               '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
           05  FILLER PIC X(79) VALUE '                              '.
       01  FILLER REDEFINES MSG-TABLE.
           05  MSG-ITEM                            PIC  X(79)
               OCCURS 20.
      
       01  MSG-ERR-ENTRY                           PIC  X(79)
           VALUE 'This program may only be reached thru main menu (SA00
      -          'transaction)'.
       01  MSG-ERR-FUNCTION                        PIC  X(79)
           VALUE 'E: function not supported.                '.
       01  MSG-ERR-FUNC-KEY                        PIC  X(79)
           VALUE 'E: function key not supported.             '.
       01  MSG-1                                   PIC  X(79)
           VALUE 'E: pease, enter this field (mandatory).      '.
       01  MSG-2                                   PIC  X(79)
           VALUE 'E: invalid date format.                       '.
       01  MSG-3                                   PIC  X(79)
           VALUE 'E: data must be numeric.                       '.
       01  MSG-98                                  PIC  X(69)
           VALUE 'I: please, correct hilighted fields.           '.
      
       LINKAGE                                   SECTION.
       01  DFHCOMMAREA                             PIC  X(50).
      
       PROCEDURE                                 DIVISION.
       MAIN                                      SECTION.
      
      * Initialize map.
           MOVE  LOW-VALUE  TO  RSSAM02O.
      
      * If first time...
           IF EIBCALEN  =  0
              MOVE LOW-VALUE TO RSSAM02O
              MOVE  MSG-ERR-ENTRY TO        VMESSO
              PERFORM SEND-MSG-END
           END-IF.
      
      * Get Commarea...
           MOVE DFHCOMMAREA TO COMM-RECORD.
      
      * Evaluate process...
           EVALUATE COMM-FONC
              WHEN 'SEND'
                   PERFORM FIRST-TIME
              WHEN 'RECEIVE'
                   PERFORM OTHER-TIME
              WHEN OTHER
                   MOVE MSG-ERR-FUNCTION TO VMESSO
                   PERFORM SEND-MSG-END
           END-EVALUATE.
      
      *-------------------------------------------------*
       FIRST-TIME                                SECTION.
           IF COMM-DEST  =  'MAJ ' OR 'SUPP'
              PERFORM GET-CUSTOMER
              PERFORM LOAD-MAP-DATA
           END-IF.
      
           PERFORM PREPARE-ATTRIBUTES.
           PERFORM PREPARE-SCREEN.
           PERFORM DISPLAY-MAP.
           PERFORM TRANSID-RETURN.
      
      *-------------------------------------------------*
       OTHER-TIME                                SECTION.
      * Get entered data
           PERFORM RECEIVE-MAP.
           PERFORM PREPARE-ATTRIBUTES.
           PERFORM PREPARE-FIELDS.
           PERFORM PREPARE-SCREEN.
           PERFORM DISPLAY-MAP-CURS.
           PERFORM TRANSID-RETURN.
      
      *----------
       RECEIVE-MAP.
      *--- Receive map...
           EXEC CICS
                IGNORE CONDITION MAPFAIL
           END-EXEC.
           EXEC CICS
                RECEIVE MAP   ('RSSAM02')
                        MAPSET('RSSAM02')
                        INTO  (RSSAM02I)
           END-EXEC.
      
      *--- Perform control on input data
           PERFORM CTRL-INPUT-DATA.
      
      *--- Eveluate next action...
           EVALUATE EIBAID
              WHEN DFHPF3
                   MOVE  0   TO COMM-NUM-MESS
                   PERFORM BACK-TO-MENU
              WHEN DFHPF11
                   PERFORM CLEAR-SCREEN
              WHEN DFHPF12
                   IF ERR-NBR = 0
                      IF COMM-DEST = 'CREA'
                         PERFORM ADD-NEW-CUSTOMER
                      END-IF
                      IF COMM-DEST = 'MAJ '
                         PERFORM UPDATE-CUSTOMER
                      END-IF
                      IF COMM-DEST = 'SUPP'
                         PERFORM DELETE-CUSTOMER
                         MOVE  5 TO COMM-NUM-MESS
                         PERFORM BACK-TO-MENU
                      END-IF
                   END-IF
              WHEN DFHENTER
                   IF ERR-NBR = 0
                      MOVE MSG-ITEM(14) TO VMESSO
                   END-IF
                   CONTINUE
              WHEN DFHCLEAR
                   PERFORM CLEAR-SCREEN
              WHEN OTHER
                   MOVE MSG-ERR-FUNC-KEY TO VMESSO
           END-EVALUATE.
      
      *--------------
       CTRL-INPUT-DATA.
      
           MOVE LOW-VALUE TO VMESSO.
           MOVE SPACES    TO VS-ODCSF0-RECORD.
           MOVE ZEROES    TO VS-CUSTIDENT
                             VS-CUSTBDATE
                             VS-CUSTPHONE.
           MOVE ZEROES    TO ERR-NBR.
      
      *--- Edit customer's phone number
           MOVE ZEROES  TO TAG-PHONE.
           IF VPHONEL =  0
           OR VPHONEI =  SPACES
              MOVE    MSG-1    TO   VMESSO
              MOVE    1        TO   TAG-PHONE
              ADD    +1        TO   ERR-NBR
           ELSE
              IF VPHONEI NOT NUMERIC
                 MOVE    MSG-3    TO    VMESSO
                 MOVE    1        TO    TAG-PHONE
                 ADD    +1        TO    ERR-NBR
              ELSE
                 MOVE    VPHONEI  TO    VS-CUSTPHONE
              END-IF
           END-IF.
      
      *--- Edit customer's Email address
           MOVE ZEROES  TO TAG-EMAIL.
           IF VEMAILL =  0
           OR VEMAILI =  SPACES
              MOVE    MSG-1    TO      VMESSO
              MOVE    1        TO      TAG-EMAIL
              ADD    +1        TO   ERR-NBR
           ELSE
              MOVE    VEMAILI  TO      VS-CUSTEMAIL
           END-IF.
      
      *--- Edit customer's birthdate
           MOVE ZEROES  TO TAG-DBIRTH.
           IF VDBIRTHL =  0
           OR VDBIRTHI =  SPACES
              MOVE    MSG-1    TO   VMESSO
              MOVE    1        TO   TAG-DBIRTH
              ADD    +1        TO   ERR-NBR
           ELSE
              MOVE    VDBIRTHI TO   SCREEN-DATE
              PERFORM CTRL-DATE
              IF DBIRTH-OK
                 MOVE SCR-MM9   TO VS-CUSTBDATE-MM
                 MOVE SCR-DD9   TO VS-CUSTBDATE-DD
                 MOVE SCR-CCYY9 TO VS-CUSTBDATE(1:4)
              ELSE
                 MOVE  MSG-2   TO   VMESSO
                 ADD  +1       TO   ERR-NBR
              END-IF
           END-IF.
      
      *--- Edit customer's state of residence
           MOVE ZEROES  TO TAG-STATE.
           IF VSTATEL =  0
           OR VSTATEI =  SPACES
              MOVE    MSG-1    TO      VMESSO
              MOVE    1        TO      TAG-STATE
              ADD    +1       TO      ERR-NBR
           ELSE
              MOVE    VSTATEI  TO      VS-CUSTSTATE
           END-IF.
      
      *--- Edit customer's city
           MOVE ZEROES  TO TAG-CITY.
           IF VCITYL =  0
           OR VCITYI =  SPACES
              MOVE    MSG-1    TO      VMESSO
              MOVE    1        TO      TAG-CITY
              ADD    +1       TO      ERR-NBR
           ELSE
              MOVE    VCITYI   TO      VS-CUSTCITY
           END-IF.
      
      *--- Edit customer's address
           MOVE ZEROES  TO TAG-ADDRES.
           IF VADDRESL =  0
           OR VADDRESI =  SPACES
              MOVE    MSG-1    TO      VMESSO
              MOVE    1        TO      TAG-ADDRES
              ADD    +1       TO      ERR-NBR
           ELSE
              MOVE    VADDRESI TO      VS-CUSTADDRS
           END-IF.
      
      *--- Edit customer's firstname
           MOVE ZEROES  TO TAG-FNAME.
           IF VFNAMEL =  0
           OR VFNAMEI =  SPACES
              MOVE    MSG-1   TO      VMESSO
              MOVE    1       TO      TAG-FNAME
              ADD    +1       TO      ERR-NBR
           ELSE
              MOVE    VFNAMEI TO      VS-CUSTFNAME
           END-IF.
      
      *--- Edit customer's lastname
           MOVE ZEROES  TO TAG-LNAME.
           IF VLNAMEL =  0
           OR VLNAMEI =  SPACES
              MOVE    MSG-1   TO      VMESSO
              MOVE    1       TO      TAG-LNAME
              ADD    +1       TO      ERR-NBR
           ELSE
              MOVE    VLNAMEI TO      VS-CUSTLNAME
           END-IF.
      
      *--- If action is "ADD", check if ID OK and not referenced...
           MOVE ZEROES  TO TAG-IDCUST.
           IF COMM-DEST = 'CREA'
              IF VIDCUSTL =  0
              OR VIDCUSTI =  SPACES
                 MOVE    MSG-1    TO      VMESSO
                 MOVE    1        TO      TAG-IDCUST
                 ADD    +1        TO      ERR-NBR
              ELSE
                 IF VIDCUSTI NOT NUMERIC
                    MOVE    MSG-3    TO    VMESSO
                    MOVE    1        TO    TAG-IDCUST
                    ADD    +1        TO    ERR-NBR
                 ELSE
                    PERFORM CHECK-EXISTING-CUSTOMER
                    MOVE    VIDCUSTI TO COMM-CLT
                    MOVE    VIDCUSTI TO VS-CUSTIDENT
                 END-IF
              END-IF
           END-IF.
      
      *--------------
       CTRL-DATE.
           IF  NOT DAY-OK
               MOVE    1       TO      TAG-DBIRTH
               MOVE    MSG-2           TO      VMESSO
           END-IF.
      
           IF  NOT MONTH-OK
               MOVE    1       TO      TAG-DBIRTH
               MOVE    MSG-2           TO      VMESSO
           END-IF.
      
           IF  SCR-CCYY NOT NUMERIC
               MOVE    1       TO      TAG-DBIRTH
               MOVE    MSG-2           TO      VMESSO
           END-IF.
      
           IF (SCR-MM = '04' OR '06' OR '09' OR '11')
           AND SCR-DD = '31'
               MOVE    1       TO      TAG-DBIRTH
               MOVE    MSG-2           TO      VMESSO
           END-IF.
      
           IF   SCR-MM = '02'
           AND (SCR-DD = '30' OR '31')
               MOVE    1       TO      TAG-DBIRTH
               MOVE    MSG-2           TO      VMESSO
           END-IF.
      
      *-----------
       PREPARE-ATTRIBUTES.
           MOVE  'A'  TO  VLNAMEA   VFNAMEA   VADDRESA  VCITYA
                          VSTATEA   VDBIRTHA  VEMAILA   VPHONEA.
      
           MOVE  '8'  TO  VPGMSCRA  VDATEA    VTRANSA
                          VACTIONA  VMESSA.
      
           IF COMM-DEST  =  'CREA'
              MOVE 'A' TO VIDCUSTA
           ELSE
              MOVE '8' TO VIDCUSTA
           END-IF.
      
      *-----------
       PREPARE-FIELDS.
           MOVE    ZEROES  TO      ERR-NBR.
           IF ERR-PHONE
              MOVE    'I'     TO      VPHONEA
              MOVE    -1      TO      VPHONEL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-EMAIL
              MOVE    'I'     TO      VEMAILA
              MOVE    -1      TO      VEMAILL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-DBIRTH
              MOVE    'I'     TO      VDBIRTHA
              MOVE    -1      TO      VDBIRTHL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-STATE
              MOVE    'I'     TO      VSTATEA
              MOVE    -1      TO      VSTATEL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-CITY
              MOVE    'I'     TO      VCITYA
              MOVE    -1      TO      VCITYL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-ADDRES
              MOVE    'I'     TO      VADDRESA
              MOVE    -1      TO      VADDRESL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-FNAME
              MOVE    'I'     TO      VFNAMEA
              MOVE    -1      TO      VFNAMEL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-LNAME
              MOVE    'I'     TO      VLNAMEA
              MOVE    -1      TO      VLNAMEL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-IDCUST
              MOVE    'I'     TO      VIDCUSTA
              MOVE    -1      TO      VIDCUSTL
              ADD     +1      TO      ERR-NBR
           END-IF.
      
           IF ERR-NBR > 1
              MOVE    MSG-98 TO       VMESSO
           END-IF.
      
      *----------
       PREPARE-SCREEN.
           MOVE   'RSSAT002'       TO      VPGMSCRO.
           MOVE    EIBTRNID        TO      VTRANSO.
           MOVE    COMM-CLT        TO      VIDCUSTO.
      
      * Send action litteral to screen
           EVALUATE COMM-DEST
              WHEN 'MAJ '  MOVE 'UPDATE' TO VACTIONO
              WHEN 'CREA'  MOVE 'CREATE' TO VACTIONO
              WHEN 'SUPP'  MOVE 'DELETE' TO VACTIONO
              WHEN OTHER   MOVE '      ' TO VACTIONO
           END-EVALUATE.
      
      * Get date in output format.
           EXEC CICS
                ASKTIME ABSTIME(ABS-TIME)
           END-EXEC.
           EXEC CICS
                FORMATTIME ABSTIME(ABS-TIME)
                DDMMYY(VDATEO)
                DATESEP('-')
           END-EXEC.
      
      *----------
       LOAD-MAP-DATA.
           MOVE  QS-CUSTLNAME    TO VLNAMEO.
           MOVE  QS-CUSTFNAME    TO VFNAMEO.
           MOVE  QS-CUSTADDRS    TO VADDRESO.
           MOVE  QS-CUSTCITY     TO VCITYO.
           MOVE  QS-CUSTSTATE    TO VSTATEO.
           MOVE  QS-CUSTBDATE(1:4) TO SCR-CCYY9.
           MOVE  QS-CUSTBDATE-MM     TO SCR-MM9.
           MOVE  QS-CUSTBDATE-DD     TO SCR-DD9.
           MOVE '/'                  TO SCR-SEP1  SCR-SEP2.
           MOVE  SCREEN-DATE         TO VDBIRTHO.
           MOVE  QS-CUSTEMAIL        TO VEMAILO.
           MOVE  QS-CUSTPHONE        TO VPHONEO.
      
      *----------
       CLEAR-SCREEN.
           MOVE LOW-VALUE TO RSSAM02O.
           IF COMM-DEST NOT = 'CREA'
              PERFORM GET-CUSTOMER
              PERFORM LOAD-MAP-DATA
           END-IF.
           PERFORM PREPARE-ATTRIBUTES.
      
      *-----------
       DISPLAY-MAP.
           EXEC CICS
                SEND MAP('RSSAM02')
                     MAPSET('RSSAM02')
                     ERASE
           END-EXEC.
      
      *----------------
       DISPLAY-MAP-CURS.
           EXEC CICS
                SEND MAP('RSSAM02')
                     MAPSET('RSSAM02')
                     CURSOR
                     ERASE
           END-EXEC.
      
      *-----------
       CHECK-EXISTING-CUSTOMER.
      * Access VSAM file on customer'sID
           MOVE VIDCUSTI TO CUST-FILE-KEY.
           EXEC CICS
                READ DATASET ('ODCSF0')
                INTO    (QS-ODCSF0-RECORD)
                LENGTH  (LENGTH OF QS-ODCSF0-RECORD)
                RIDFLD  (CUST-FILE-KEY)
                EQUAL
                RESP    (RESPONSE-CODE)
           END-EXEC.
      
           IF RESPONSE-CODE = DFHRESP(NOTOPEN)
              PERFORM VSAM-FILE-NOT-OPEN
           END-IF.
      
           IF RESPONSE-CODE = DFHRESP(NOTFND)
              MOVE    0          TO TAG-IDCUST
           ELSE
              IF RESPONSE-CODE = DFHRESP(NORMAL)
                 MOVE  MSG-ITEM(13) TO VMESSO
                 MOVE  1            TO TAG-IDCUST
                 ADD   +1           TO ERR-NBR
              ELSE
                 PERFORM TERMINATE-PROGRAM
              END-IF
           END-IF.
      
      *-----------
       GET-CUSTOMER.
      *-----------
           MOVE COMM-CLT TO CUST-FILE-KEY.
           EXEC CICS READ DATASET ('ODCSF0')
                          INTO    (QS-ODCSF0-RECORD)
                          LENGTH  (LENGTH OF QS-ODCSF0-RECORD)
                          RIDFLD  (CUST-FILE-KEY)
                          EQUAL
                          RESP    (RESPONSE-CODE)
           END-EXEC.
      
           IF RESPONSE-CODE = DFHRESP(NOTOPEN)
              PERFORM VSAM-FILE-NOT-OPEN
           END-IF.
      
           IF RESPONSE-CODE = DFHRESP(NOTFND)
              PERFORM LECT-MAJ-INEXISTANT
           END-IF.
      
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              PERFORM TERMINATE-PROGRAM
           END-IF.
      
      *-----------
       ADD-NEW-CUSTOMER.
      *-----------
           EXEC CICS WRITE DATASET   ('ODCSF0')
                           FROM      (VS-ODCSF0-RECORD)
                           LENGTH    (LENGTH OF VS-ODCSF0-RECORD)
                           RIDFLD    (VS-CUSTIDENT)
                           KEYLENGTH (6)
                           RESP      (RESPONSE-CODE)
           END-EXEC.
      
           IF RESPONSE-CODE = DFHRESP(NOTOPEN)
              PERFORM VSAM-FILE-NOT-OPEN
           END-IF.
      
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              PERFORM TERMINATE-PROGRAM
           END-IF.
      
           MOVE MSG-ITEM(1) TO VMESSO.
      
      *-----------
       UPDATE-CUSTOMER.
      * Get customer from VSAM file
           MOVE COMM-CLT TO CUST-FILE-KEY.
           EXEC CICS READ DATASET ('ODCSF0')
                          INTO    (QS-ODCSF0-RECORD)
                          LENGTH  (LENGTH OF QS-ODCSF0-RECORD)
                          RIDFLD  (CUST-FILE-KEY)
                          EQUAL
                          UPDATE
                          RESP    (RESPONSE-CODE)
           END-EXEC.
      
           IF RESPONSE-CODE = DFHRESP(NOTOPEN)
              PERFORM VSAM-FILE-NOT-OPEN
           END-IF.
      
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              PERFORM TERMINATE-PROGRAM
           END-IF.
      
      * Move updated data to VSAM record
           MOVE VS-CUSTLNAME      TO QS-CUSTLNAME.
           MOVE VS-CUSTFNAME      TO QS-CUSTFNAME.
           MOVE VS-CUSTADDRS      TO QS-CUSTADDRS.
           MOVE VS-CUSTCITY       TO QS-CUSTCITY.
           MOVE VS-CUSTSTATE      TO QS-CUSTSTATE.
           MOVE VS-CUSTBDATE      TO QS-CUSTBDATE.
           MOVE VS-CUSTEMAIL      TO QS-CUSTEMAIL.
           MOVE VS-CUSTPHONE      TO QS-CUSTPHONE.
      * Update current record
           EXEC CICS REWRITE DATASET ('ODCSF0')
                             FROM    (QS-ODCSF0-RECORD)
                             LENGTH  (LENGTH OF QS-ODCSF0-RECORD)
                             RESP    (RESPONSE-CODE)
           END-EXEC.
      
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              PERFORM TERMINATE-PROGRAM
           END-IF.
      
           MOVE MSG-ITEM(7) TO VMESSO.
      
      *-----------
       DELETE-CUSTOMER.
      * Delete customer's record
           MOVE COMM-CLT TO CUST-FILE-KEY.
           EXEC CICS DELETE FILE    ('ODCSF0')
                            RIDFLD  (CUST-FILE-KEY)
                            RESP    (RESPONSE-CODE)
           END-EXEC.
      
           IF RESPONSE-CODE = DFHRESP(NOTOPEN)
              PERFORM VSAM-FILE-NOT-OPEN
           END-IF.
      
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              PERFORM TERMINATE-PROGRAM
           END-IF.
      
           MOVE MSG-ITEM(5) TO VMESSO.
      
      *---------------------
       VSAM-FILE-NOT-OPEN.
           MOVE  COMM-DEST  TO  COMM-PROV.
           MOVE 'MENU'      TO  COMM-DEST.
           MOVE 'SEND'      TO  COMM-FONC.
           MOVE 'KO'        TO  COMM-RETOUR.
           MOVE  12         TO  COMM-NUM-MESS.
           MOVE 'RSSAT000'  TO  PGM-DEST.
           PERFORM NEXT-TASK.
      
      *---------------------
       LECT-MAJ-INEXISTANT.
           MOVE  COMM-DEST  TO  COMM-PROV.
           MOVE 'MENU'      TO  COMM-DEST.
           MOVE 'SEND'      TO  COMM-FONC.
           MOVE 'KO'        TO  COMM-RETOUR.
           MOVE  11         TO  COMM-NUM-MESS.
           MOVE 'RSSAT000'  TO  PGM-DEST.
           PERFORM NEXT-TASK.
      
      *---------------------
       BACK-TO-MENU.
           MOVE  COMM-DEST  TO  COMM-PROV.
           MOVE 'MENU'      TO  COMM-DEST.
           MOVE 'SEND'      TO  COMM-FONC.
           MOVE 'OK'        TO  COMM-RETOUR.
           MOVE 'RSSAT000'  TO  PGM-DEST.
           PERFORM NEXT-TASK.
      
      *-----------
       NEXT-TASK.
           EXEC CICS HANDLE CONDITION
                            PGMIDERR(PGM-NOT-FOUND)
           END-EXEC.
           EXEC CICS XCTL   PROGRAM(PGM-DEST)
                            COMMAREA(COMM-RECORD)
                            LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC.
      
      *---------------
       TRANSID-RETURN.
           MOVE  COMM-DEST   TO  COMM-PROV.
           MOVE 'RECEIVE'    TO  COMM-FONC.
           EXEC CICS RETURN TRANSID('SA02')
                            COMMAREA(COMM-RECORD)
                            LENGTH(LENGTH OF COMM-RECORD)
           END-EXEC.
      
      *---------
       PGM-NOT-FOUND.
           MOVE    MSG-PGM-ERROR   TO      VMESSO.
           PERFORM SEND-MSG-END.
      
      *---------------
       SEND-MSG-END.
      *---------------
           MOVE 'A' TO VMESSA.
           EXEC CICS
                SEND MAP('RSSAM02')
                     MAPSET('RSSAM02')
                     ERASE
           END-EXEC.
      
           EXEC CICS RETURN
           END-EXEC.
      
      *---------------
       TERMINATE-PROGRAM.
      *---------------
           EXEC CICS ABEND ABCODE('META') END-EXEC
           .
      *--------------
       JUST-IN-CASE.
           GOBACK.
