       01  RSSAM00I.
           02  FILLER PIC X(12).
           02  VPGMSCRL    COMP  PIC  S9(4).
           02  VPGMSCRF    PICTURE X.
           02  FILLER REDEFINES VPGMSCRF.
             03 VPGMSCRA    PICTURE X.
           02  VPGMSCRI  PIC X(8).
           02  VDATEL    COMP  PIC  S9(4).
           02  VDATEF    PICTURE X.
           02  FILLER REDEFINES VDATEF.
             03 VDATEA    PICTURE X.
           02  VDATEI  PIC X(10).
           02  VTRANSL    COMP  PIC  S9(4).
           02  VTRANSF    PICTURE X.
           02  FILLER REDEFINES VTRANSF.
             03 VTRANSA    PICTURE X.
           02  VTRANSI  PIC X(4).
           02  CIDCUSTL    COMP  PIC  S9(4).
           02  CIDCUSTF    PICTURE X.
           02  FILLER REDEFINES CIDCUSTF.
             03 CIDCUSTA    PICTURE X.
           02  CIDCUSTI  PIC X(6).
           02  VMESSL    COMP  PIC  S9(4).
           02  VMESSF    PICTURE X.
           02  FILLER REDEFINES VMESSF.
             03 VMESSA    PICTURE X.
           02  VMESSI  PIC X(79).
       01  RSSAM00O REDEFINES RSSAM00I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  VPGMSCRO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  VDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  VTRANSO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  CIDCUSTO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  VMESSO  PIC X(79).
