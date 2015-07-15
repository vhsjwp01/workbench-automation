      * ------------------------------------------------------------
      * Customer's record description
      *  -Record's length : 269
      * ------------------------------------------------------------
       01 QS-ODCSF0-RECORD.
          05    QS-ACTION              PIC X(003).
          05    QS-CUSTDATA.
            10  QS-CUSTIDENT           PIC 9(006).
            10  QS-CUSTLNAME           PIC X(030).
            10  QS-CUSTFNAME           PIC X(020).
            10  QS-CUSTADDRS           PIC X(030).
            10  QS-CUSTCITY            PIC X(020).
            10  QS-CUSTSTATE           PIC X(002).
            10  QS-CUSTBDATE           PIC 9(008).
            10  QS-CUSTBDATE-G         REDEFINES QS-CUSTBDATE.
             15 QS-CUSTBDATE-CC PIC 9(002).
             15 QS-CUSTBDATE-YY PIC 9(002).
             15 QS-CUSTBDATE-MM PIC 9(002).
            15  QS-CUSTBDATE-DD PIC 9(002).
            10  QS-CUSTEMAIL           PIC X(040).
            10  QS-CUSTPHONE           PIC 9(010).
            10  QS-FILLER              PIC X(100).
      * ------------------------------------------------------------
      
