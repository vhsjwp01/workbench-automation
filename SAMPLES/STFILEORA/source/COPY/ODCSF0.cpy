      * ------------------------------------------------------------
      * Customer's record description
      *  -Record's length : 266
      * ------------------------------------------------------------
       01 QS-ODCSF0-RECORD.
          05 QS-CUSTIDENT           PIC 9(006).
          05 QS-CUSTLNAME           PIC X(030).
          05 QS-CUSTFNAME           PIC X(020).
          05 QS-CUSTADDRS           PIC X(030).
          05 QS-CUSTCITY            PIC X(020).
          05 QS-CUSTSTATE           PIC X(002).
          05 QS-CUSTBDATE           PIC 9(008).
          05 QS-CUSTBDATE-G         REDEFINES QS-CUSTBDATE.
           10 QS-CUSTBDATE-CC PIC 9(002).
           10 QS-CUSTBDATE-YY PIC 9(002).
           10 QS-CUSTBDATE-MM PIC 9(002).
           10 QS-CUSTBDATE-DD PIC 9(002).
          05 QS-CUSTEMAIL           PIC X(040).
          05 QS-CUSTPHONE           PIC 9(010).
          05 QS-FILLER              PIC X(100).
      * ------------------------------------------------------------
      
