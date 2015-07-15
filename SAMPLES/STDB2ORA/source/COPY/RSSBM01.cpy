       01  RSSBM01I.
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
           02  VIDCUSTL    COMP  PIC  S9(4).
           02  VIDCUSTF    PICTURE X.
           02  FILLER REDEFINES VIDCUSTF.
             03 VIDCUSTA    PICTURE X.
           02  VIDCUSTI  PIC X(6).
           02  VLNAMEL    COMP  PIC  S9(4).
           02  VLNAMEF    PICTURE X.
           02  FILLER REDEFINES VLNAMEF.
             03 VLNAMEA    PICTURE X.
           02  VLNAMEI  PIC X(30).
           02  VFNAMEL    COMP  PIC  S9(4).
           02  VFNAMEF    PICTURE X.
           02  FILLER REDEFINES VFNAMEF.
             03 VFNAMEA    PICTURE X.
           02  VFNAMEI  PIC X(20).
           02  VADDRSL    COMP  PIC  S9(4).
           02  VADDRSF    PICTURE X.
           02  FILLER REDEFINES VADDRSF.
             03 VADDRSA    PICTURE X.
           02  VADDRSI  PIC X(30).
           02  VCITYL    COMP  PIC  S9(4).
           02  VCITYF    PICTURE X.
           02  FILLER REDEFINES VCITYF.
             03 VCITYA    PICTURE X.
           02  VCITYI  PIC X(30).
           02  VSTATEL    COMP  PIC  S9(4).
           02  VSTATEF    PICTURE X.
           02  FILLER REDEFINES VSTATEF.
             03 VSTATEA    PICTURE X.
           02  VSTATEI  PIC X(2).
           02  VBDATEL    COMP  PIC  S9(4).
           02  VBDATEF    PICTURE X.
           02  FILLER REDEFINES VBDATEF.
             03 VBDATEA    PICTURE X.
           02  VBDATEI  PIC X(10).
           02  VEMAILL    COMP  PIC  S9(4).
           02  VEMAILF    PICTURE X.
           02  FILLER REDEFINES VEMAILF.
             03 VEMAILA    PICTURE X.
           02  VEMAILI  PIC X(50).
           02  VPHONEL    COMP  PIC  S9(4).
           02  VPHONEF    PICTURE X.
           02  FILLER REDEFINES VPHONEF.
             03 VPHONEA    PICTURE X.
           02  VPHONEI  PIC X(10).
           02  VMESSL    COMP  PIC  S9(4).
           02  VMESSF    PICTURE X.
           02  FILLER REDEFINES VMESSF.
             03 VMESSA    PICTURE X.
           02  VMESSI  PIC X(79).
       01  RSSBM01O REDEFINES RSSBM01I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  VPGMSCRO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  VDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  VTRANSO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  VIDCUSTO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  VLNAMEO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  VFNAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  VADDRSO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  VCITYO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  VSTATEO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  VBDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  VEMAILO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  VPHONEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  VMESSO  PIC X(79).
