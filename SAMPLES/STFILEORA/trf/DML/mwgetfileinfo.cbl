*     *@ (c) Metaware:MWGETFILEINFO. $Revision: 1.1 $
*     *
*     * INPUT ARGS  : assign name.
*     * OUTPUT ARGS : F (as File) or T (if Table) or E (if ERROR)
*     *             : DML-LOGICAL-NAME in case of 'T'
*     *
*     * Remarks :
*     *   The unix microfocus variable DD_<assign_name> is read.
*     *   It contains the physical file name.
*     *
*     *   This program checks if <file> + ".rdb" extension associated to the
*     *   assign-name is existing.
*     *    yes-> get the accessor name in this file -> return "T" to the caller
*     *    no -> return "F"
*     *   In case of error : "E"
*     *
*     *
*     * RDB format is:
*     *  phys_file_name <size> <IDX> <unload_name> <download_name> <unload_assign> <download_assign> <relational_module_name> <logical_module_name>
*     *
      $set assign"dynamic"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mwgetfileinfo.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT RDBFILE ASSIGN RDB-FILE-NAME
              FILE STATUS    WS-RDB-STATUS
              ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  RDBFILE.
       01  RDBFILE-ART.
           02 RDBFILE-DATA PIC X(999).

       WORKING-STORAGE SECTION.
       01  RDB-FILE-NAME-LG       PIC 9(3).
       01  RDB-FILE-NAME.
           05 FILLER PIC X OCCURS 1 TO 256
              DEPENDING RDB-FILE-NAME-LG.
       01  POINTER-Y               PIC 9(6).
       01  WS-RDB-STATUS           PIC XX.


*     *-* will contain list of sub-directories from PHYS-FILE
*     *-* 01  DIRECTORY-COUNT         PIC S9(4).
*     *-* 01  DIRECTORY-LIST-ARRAY.
*     *-*     05 SELECTED-DIRECTORY   PIC X(50) OCCURS 15
*     *-*                             INDEXED BY DIR-INDEX.
*     * will contain content of the RDB file
       01  WORD-COUNT            PIC S9(4).
       01  WORD-LIST-ARRAY.
           05 SELECTED-WORD      PIC X(256) OCCURS 15
                                 INDEXED BY DIR-INDEX.

           77 WORD-PHYS-NAME     PIC 99 VALUE 1.
           77 WORD-MAX-SIZE      PIC 99 VALUE 2.
           77 WORD-ORGANIZATION  PIC 99 VALUE 3.
           77 WORD-SPEC-RECFM    PIC 99 VALUE 4.
           77 WORD-UNL-NAME      PIC 99 VALUE 5.
           77 WORD-UNL-ASG       PIC 99 VALUE 6.
           77 WORD-DWL-NAME      PIC 99 VALUE 7.
           77 WORD-DWL-ASG       PIC 99 VALUE 8.
           77 WORD-REL-MOD-NAME  PIC 99 VALUE 9.
           77 WORD-FIRST-TABLE   PIC 99 VALUE 10.
           77 WORD-SQL-CLEAN     PIC 99 VALUE 11.
           77 WORD-SQL-DROP      PIC 99 VALUE 12.
           77 WORD-SQL-CREATE    PIC 99 VALUE 13.
           77 WORD-SQL-IFEMPTY   PIC 99 VALUE 14.
           77 WORD-SQL-IFEXIST   PIC 99 VALUE 15.


       01  UNIX-VAR-ASSIGN       PIC X(99).
*     *
       01  TMP-UNIX-VAR-NAME     PIC X(30).
       01  TMP-UNIX-VAR-VALUE    PIC X(256).

       01  CBLTE-FILE-DETAILS.
           03 CBLTE-FE-FILESIZE       PIC X(08).
           03 CBLTE-FE-DATE.
              05 CBLTE-FE-DAY         PIC X(01).
              05 CBLTE-FE-MONTH       PIC X(01).
              05 CBLTE-FE-YEAR        PIC X(02).
           03 CBLTE-FE-TIME.
              05 CBLTE-FE-HOURS       PIC X(01).
              05 CBLTE-FE-MINUTES     PIC X(01).
              05 CBLTE-FE-SECONDS     PIC X(01).
              05 CBLTE-FE-HUNDREDTHS  PIC X(01).

       01  STATUS-CODE                PIC X(02).

       COPY MW-PARAM-TRACE.
       COPY MW-PARAM-TRACE-VAR.

       LINKAGE SECTION.

       COPY MW-PARAM-GETFILEINFO.

       PROCEDURE DIVISION USING GFI-INPUT-ASSIGN-NAME
                                GFI-OUTPUT-ARGS.
       P-START.
           MOVE "mwgetfileinfo" 
            TO META-DB-TRACE-ACCNAME OF META-DB-TRACE-PARAMETERS.

           IF MT-DBACS-TRACE NOT < 1
             MOVE GFI-INPUT-ASSIGN-NAME TO META-DB-TRACE-PARAMETERS
             CALL META-DB-TRACE-MODULE-NAME
              USING META-DB-TRACE-PARAMETERS
           END-IF.

           IF ( ADDRESS OF GFI-INPUT-ASSIGN-NAME = NULL )
 Error       DISPLAY "ERROR:"
 Error       DISPLAY "FILEDML-1001: mwgetfileinfo. "
 Error               "ASSIGN ARGUMENT IS NULL"
             SET ACCESS-TYPE-IS-ERROR TO TRUE
             GO TO P-GOBACK
           END-IF.
           
           MOVE SPACES TO DML-LOGICAL-NAME.
*     *
*     * Get the Assign value, setted by the JCL : export DD_<assign>=
*     * 
       GET-ASSIGN.
           MOVE SPACES TO TMP-UNIX-VAR-NAME.
           STRING 
            "DD_" GFI-INPUT-ASSIGN-NAME DELIMITED BY SPACE
            INTO TMP-UNIX-VAR-NAME
           END-STRING.
           MOVE SPACES TO TMP-UNIX-VAR-VALUE.
           DISPLAY TMP-UNIX-VAR-NAME UPON ENVIRONMENT-NAME. 
           ACCEPT TMP-UNIX-VAR-VALUE FROM ENVIRONMENT-VALUE.

           IF TMP-UNIX-VAR-VALUE = SPACE
 Error       DISPLAY "ERROR:"
 Error       DISPLAY "FILEDML-1002: mwgetfileinfo. "
 Error               "UNIX VARIABLE "
 Error               TMP-UNIX-VAR-NAME " IS EMPTY"
             SET ACCESS-TYPE-IS-ERROR TO TRUE
             GO TO P-GOBACK
           END-IF.


           MOVE TMP-UNIX-VAR-VALUE TO UNIX-VAR-ASSIGN.

*     *
*     *    get the module name written in the RDB

*     *    Add ".rdb" to UNIX-VAR_ASSIGN
*     *    check if a file exists 
*     *    IF a ".rdb"-exist then access to TABLE is assumed, and set 
*     *    dml-logical-name to last-part
*     *    else file-access is true
*     *
       CHECK-RDB-FILE.
           MOVE SPACES TO TMP-UNIX-VAR-VALUE.
           STRING 
            UNIX-VAR-ASSIGN DELIMITED BY SPACE
            ".rdb" DELIMITED BY SIZE
            INTO TMP-UNIX-VAR-VALUE
           END-STRING.

           MOVE 256 TO RDB-FILE-NAME-LG.
           MOVE 1 TO POINTER-Y.
           STRING TMP-UNIX-VAR-VALUE DELIMITED BY SPACE
            INTO  RDB-FILE-NAME
            POINTER POINTER-Y
           END-STRING.
           SUBTRACT 1 FROM POINTER-Y GIVING RDB-FILE-NAME-LG.

           CALL "CBL_CHECK_FILE_EXIST" 
            USING TMP-UNIX-VAR-VALUE
                  CBLTE-FILE-DETAILS
                RETURNING STATUS-CODE.

           IF STATUS-CODE NOT = ZEROS AND NOT = LOW-VALUES
             IF MT-DBACS-TRACE NOT < 7
 Trace         DISPLAY "TRACE:"
 Trace         DISPLAY "FILEDML-0001. mwgetfileinfo. "
 Trace                 "RDB FILE " 
 Trace                 RDB-FILE-NAME(1:RDB-FILE-NAME-LG)
 Trace                 " DOES NOT EXIST"
             END-IF
             IF RETURN-CODE = 35 
               MOVE 0 TO RETURN-CODE
             END-IF
             SET ACCESS-TYPE-IS-FILE TO TRUE
             GO TO END-CHECK-RDB-FILE
           END-IF.

*     * a RDB file exists -> this file is converted into table
           SET ACCESS-TYPE-IS-TABLE TO TRUE.

           OPEN INPUT RDBFILE.
           IF WS-RDB-STATUS NOT = "00"
 Error       DISPLAY "ERROR:"
 Error       DISPLAY "FILEDML-1003: mwgetfileinfo. "
 Error               "CAN NOT OPEN RDB FILE "
 Error               RDB-FILE-NAME " (STATUS=" WS-RDB-STATUS ")"
             SET ACCESS-TYPE-IS-ERROR TO TRUE
             GO TO END-CHECK-RDB-FILE
           END-IF.
               
           READ RDBFILE.
           IF WS-RDB-STATUS NOT = "00"
 Error       DISPLAY "ERROR:"
 Error       DISPLAY "FILEDML-1004: mwgetfileinfo. "
 Error               "CAN NOT READ RDB FILE "
 Error               RDB-FILE-NAME(1:RDB-FILE-NAME-LG)
 Error               " (LENGTH=" RDB-FILE-NAME-LG ")"
 Error       DISPLAY "STATUS=" WS-RDB-STATUS
             SET ACCESS-TYPE-IS-ERROR TO TRUE
             GO TO END-CHECK-RDB-FILE
           END-IF.

           INITIALIZE WORD-COUNT.
           MOVE SPACES TO WORD-LIST-ARRAY.
           UNSTRING RDBFILE-ART
            DELIMITED BY ALL SPACE
            INTO
                 SELECTED-WORD (WORD-PHYS-NAME)
                 SELECTED-WORD (WORD-MAX-SIZE)
                 SELECTED-WORD (WORD-ORGANIZATION)
                 SELECTED-WORD (WORD-SPEC-RECFM)
                 SELECTED-WORD (WORD-UNL-NAME)
                 SELECTED-WORD (WORD-UNL-ASG)
                 SELECTED-WORD (WORD-DWL-NAME)
                 SELECTED-WORD (WORD-DWL-ASG)
                 SELECTED-WORD (WORD-REL-MOD-NAME)
                 SELECTED-WORD (WORD-FIRST-TABLE)
                 SELECTED-WORD (WORD-SQL-CLEAN)
                 SELECTED-WORD (WORD-SQL-DROP)
                 SELECTED-WORD (WORD-SQL-CREATE)
                 SELECTED-WORD (WORD-SQL-IFEMPTY)
                 SELECTED-WORD (WORD-SQL-IFEXIST)
            TALLYING WORD-COUNT
           END-UNSTRING.
         
           IF WORD-COUNT < WORD-REL-MOD-NAME
 Error       DISPLAY "ERROR:"
 Error       DISPLAY "FILEDML-1005: mwgetfileinfo. "
 Error               "BAD COUNT REACHED IN RDB FILE " UNIX-VAR-ASSIGN
             SET ACCESS-TYPE-IS-ERROR TO TRUE
             GO TO P-GOBACK
           END-IF.
           MOVE SELECTED-WORD (WORD-REL-MOD-NAME) TO DML-LOGICAL-NAME.
       END-CHECK-RDB-FILE.

       P-GOBACK.
           IF MT-DBACS-TRACE NOT < 7
 Trace       DISPLAY "TRACE:"
 Trace       DISPLAY "FILEDML-0000. mwgetfileinfo. "
 Trace               "IN P-GOBACK:"
 Trace               "INPUT-ASSIGN=" GFI-INPUT-ASSIGN-NAME ";"
 Trace               "ACCESS-TYPE=" ACCESS-TYPE ";"
 Trace               "DML-NAME=" DML-LOGICAL-NAME
           END-IF.
           EXIT PROGRAM.
