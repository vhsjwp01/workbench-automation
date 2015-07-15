       01  RSSBM02I.
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
           02  VACTIONL    COMP  PIC  S9(4).
           02  VACTIONF    PICTURE X.
           02  FILLER REDEFINES VACTIONF.
             03 VACTIONA    PICTURE X.
           02  VACTIONI  PIC X(6).
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
           02  VADDRESL    COMP  PIC  S9(4).
           02  VADDRESF    PICTURE X.
           02  FILLER REDEFINES VADDRESF.
             03 VADDRESA    PICTURE X.
           02  VADDRESI  PIC X(30).
           02  VCITYL    COMP  PIC  S9(4).
           02  VCITYF    PICTURE X.
           02  FILLER REDEFINES VCITYF.
             03 VCITYA    PICTURE X.
           02  VCITYI  PIC X(20).
           02  VSTATEL    COMP  PIC  S9(4).
           02  VSTATEF    PICTURE X.
           02  FILLER REDEFINES VSTATEF.
             03 VSTATEA    PICTURE X.
           02  VSTATEI  PIC X(2).
           02  VDBIRTHL    COMP  PIC  S9(4).
           02  VDBIRTHF    PICTURE X.
           02  FILLER REDEFINES VDBIRTHF.
             03 VDBIRTHA    PICTURE X.
           02  VDBIRTHI  PIC X(10).
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
       01  RSSBM02O REDEFINES RSSBM02I.
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
           02  VACTIONO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  VLNAMEO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  VFNAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  VADDRESO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  VCITYO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  VSTATEO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  VDBIRTHO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  VEMAILO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  VPHONEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  VMESSO  PIC X(79).
