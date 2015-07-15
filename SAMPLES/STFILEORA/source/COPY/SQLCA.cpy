      *****EXEC SQL
      *****     INCLUDE SQLCA
      *****END-EXEC.
        01 SQLCA.
           05 SQLCAID     PIC X(8).
           05 SQLCABC     PIC S9(9) COMP-4.
           05 SQLCODE     PIC S9(9) COMP-4.
           05 SQLERRM.
              49 SQLERRML PIC S9(4) COMP-4.
              49 SQLERRMC PIC X(70).
           05 SQLERRP     PIC X(8).
           05 SQLERRD     OCCURS 6 TIMES
                          PIC S9(9) COMP-4.
           05 SQLWARN.
              10 SQLWARN0 PIC X.
              10 SQLWARN1 PIC X.
              10 SQLWARN2 PIC X.
              10 SQLWARN3 PIC X.
              10 SQLWARN4 PIC X.
              10 SQLWARN5 PIC X.
              10 SQLWARN6 PIC X.
              10 SQLWARN7 PIC X.
           05 SQLEXT.
              10 SQLWARN8 PIC X.
              10 SQLWARN9 PIC X.
              10 SQLWARNA PIC X.
              10 SQLSTATE PIC X(5).
      
        77 SQL-TEMP      PIC X(18).
        77 DSN-TEMP      PIC S9(9)  COMP-4.
        77 DSN-TMP2      PIC S9(18) COMP-3.
        77 SQL-NULL      PIC S9(9) COMP-4 VALUE +0.
        77 SQL-INIT-FLAG PIC S9(4) COMP-4 VALUE +0.
           88 SQL-INIT-DONE VALUE +1.
        01 SQL-PLIST1.
           05 SQL-PLIST-CON   PIC S9(9) COMP-4 VALUE +2637824.
           05 SQL-CALLTYPE    PIC S9(4) COMP-4 VALUE +30.
           05 SQL-PROG-NAME   PIC X(8)         VALUE "BDBAD001".
           05 SQL-TIMESTAMP-1 PIC S9(9) COMP-4 VALUE +413096173.
           05 SQL-TIMESTAMP-2 PIC S9(9) COMP-4 VALUE +45894900.
           05 SQL-SECTION     PIC S9(4) COMP-4 VALUE +1.
           05 SQL-CODEPTR     PIC S9(9) COMP-4.
           05 SQL-VPARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-APARMPTR    PIC S9(9) COMP-4 VALUE +0.
           05 SQL-STMT-NUM    PIC S9(4) COMP-4 VALUE +73.
           05 SQL-STMT-TYPE   PIC S9(4) COMP-4 VALUE +232.
           05 SQL-PVAR-LIST1.
              10 PRE-SQLDAID  PIC X(8)  VALUE "SQLDA  Ê".
              10 PRE-SQLDABC  PIC S9(9) COMP-4 VALUE +324.
              10 PRE-SQLN     PIC S9(4) COMP-4 VALUE +7.
              10 PRE-SQLLD    PIC S9(4) COMP-4 VALUE +7.
              10 PRE-SQLVAR.
                12 SQLVAR-BASE1.
                  15 SQL-PVAR-TYPE1      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN1       PIC S9(4) COMP-4 VALUE +10.
                  15 SQL-PVAR-ADDRS1.
                     20 SQL-PVAR-ADDR1   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND1    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME1.
                     20 SQL-PVAR-NAMEL1  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC1  PIC X(30) VALUE " ".
                12 SQLVAR-BASE2.
                  15 SQL-PVAR-TYPE2      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN2       PIC S9(4) COMP-4 VALUE +20.
                  15 SQL-PVAR-ADDRS2.
                     20 SQL-PVAR-ADDR2   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND2    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME2.
                     20 SQL-PVAR-NAMEL2  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC2  PIC X(30) VALUE " ".
                12 SQLVAR-BASE3.
                  15 SQL-PVAR-TYPE3      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN3       PIC S9(4) COMP-4 VALUE +8.
                  15 SQL-PVAR-ADDRS3.
                     20 SQL-PVAR-ADDR3   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND3    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME3.
                     20 SQL-PVAR-NAMEL3  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC3  PIC X(30) VALUE " ".
                12 SQLVAR-BASE4.
                  15 SQL-PVAR-TYPE4      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN4       PIC S9(4) COMP-4 VALUE +5.
                  15 SQL-PVAR-ADDRS4.
                     20 SQL-PVAR-ADDR4   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND4    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME4.
                     20 SQL-PVAR-NAMEL4  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC4  PIC X(30) VALUE " ".
                12 SQLVAR-BASE5.
                  15 SQL-PVAR-TYPE5      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN5       PIC S9(4) COMP-4 VALUE +8.
                  15 SQL-PVAR-ADDRS5.
                     20 SQL-PVAR-ADDR5   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND5    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME5.
                     20 SQL-PVAR-NAMEL5  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC5  PIC X(30) VALUE " ".
                12 SQLVAR-BASE6.
                  15 SQL-PVAR-TYPE6      PIC S9(4) COMP-4 VALUE +484.
                  15 SQL-PVAR-LEN6       PIC S9(4) COMP-4 VALUE +2818.
                  15 SQL-PVAR-ADDRS6.
                     20 SQL-PVAR-ADDR6   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND6    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME6.
                     20 SQL-PVAR-NAMEL6  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC6  PIC X(30) VALUE " ".
                12 SQLVAR-BASE7.
                  15 SQL-PVAR-TYPE7      PIC S9(4) COMP-4 VALUE +452.
                  15 SQL-PVAR-LEN7       PIC S9(4) COMP-4 VALUE +2.
                  15 SQL-PVAR-ADDRS7.
                     20 SQL-PVAR-ADDR7   PIC S9(9) COMP-4.
                     20 SQL-PVAR-IND7    PIC S9(9) COMP-4.
                  15 SQL-PVAR-NAME7.
                     20 SQL-PVAR-NAMEL7  PIC S9(4) COMP-4 VALUE +0.
                     20 SQL-PVAR-NAMEC7  PIC X(30) VALUE " ".
