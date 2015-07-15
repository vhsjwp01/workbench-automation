      *@ @(#) MetaWare Technologies IBM-MF Cobol-Translater 5.0.9 05/01/
      *
      *
      * ‡BANNER_START
      * Licensed Materials - Property of IBM
      *
      * "Restricted Materials of IBM"
      *
      * 5697-E93
      *
      * (C) Copyright IBM Corp. 2001
      *
      * CICS
      * (Element of CICS Transaction Server
      *   for z/OS, Version 2 Release 2)
      * ‡BANNER_END
      *
      *
      *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
      *                                                               *
      *               METAWARE SPECIAL HEXADECIMAL VERSION            *
      *                                                               *
      *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
       01      DFHBMSCA.
         02    DFHBMPEM  PICTURE X   VALUE  IS X"19".
         02    DFHBMPNL  PICTURE X   VALUE  IS X"15".
         02    DFHBMPFF  PICTURE X   VALUE  IS X"0C".
         02    DFHBMPCR  PICTURE X   VALUE  IS X"0D".
         02    DFHBMASK  PICTURE X   VALUE  IS X"F0".
         02    DFHBMUNP  PICTURE X   VALUE  IS X"40".
         02    DFHBMUNN  PICTURE X   VALUE  IS X"50".
         02    DFHBMPRO  PICTURE X   VALUE  IS X"60".
         02    DFHBMBRY  PICTURE X   VALUE  IS X"C8".
         02    DFHBMDAR  PICTURE X   VALUE  IS X"4C".
         02    DFHBMFSE  PICTURE X   VALUE  IS X"C1".
         02    DFHBMPRF  PICTURE X   VALUE  IS X"61".
         02    DFHBMASF  PICTURE X   VALUE  IS X"F1".
         02    DFHBMASB  PICTURE X   VALUE  IS X"F8".
         02    DFHBMEOF  PICTURE X   VALUE  IS X"80".
         02    DFHBMCUR  PICTURE X   VALUE  IS X"02".
         02    DFHBMEC   PICTURE X   VALUE  IS X"82".
         02    DFHBMFLG  PICTURE X.
             88    DFHERASE VALUES ARE X"80", X"82".
             88    DFHCURSR VALUES ARE X"02", X"82".
         02    DFHBMDET  PICTURE X   VALUE  IS X"FF".
         02    DFHBMPSO-BIN PIC 9(4) COMP VALUE 3599.
      * ABOVE VALUE 3599 = X'0E0F' ADDED BY PTM 81385 (APAR PN23267)
         02    FILLER REDEFINES DFHBMPSO-BIN.
           03  DFHBMPSO  PICTURE X.
           03  DFHBMPSI  PICTURE X.
         02    DFHSA     PICTURE X   VALUE  IS X"28".
         02    DFHCOLOR  PICTURE X   VALUE  IS X"42".
         02    DFHPS     PICTURE X   VALUE  IS X"43".
         02    DFHHLT    PICTURE X   VALUE  IS X"41".
         02    DFH3270   PICTURE X   VALUE  IS X"C0".
         02    DFHVAL    PICTURE X   VALUE  IS X"C1".
         02    DFHOUTLN  PICTURE X   VALUE  IS X"C2".
         02    DFHBKTRN  PICTURE X   VALUE  IS X"46".
         02    DFHALL    PICTURE X   VALUE  IS X"00".
         02    DFHERROR  PICTURE X   VALUE  IS X"3F".
         02    DFHDFT    PICTURE X   VALUE  IS X"7D".
         02    DFHDFCOL  PICTURE X   VALUE  IS X"00".
         02    DFHBLUE   PICTURE X   VALUE  IS X"F1".
         02    DFHRED    PICTURE X   VALUE  IS X"F2".
         02    DFHPINK   PICTURE X   VALUE  IS X"F3".
         02    DFHGREEN  PICTURE X   VALUE  IS X"F4".
         02    DFHTURQ   PICTURE X   VALUE  IS X"F5".
         02    DFHYELLO  PICTURE X   VALUE  IS X"F6".
         02    DFHNEUTR  PICTURE X   VALUE  IS X"F7".
         02    DFHBASE   PICTURE X   VALUE  IS X"00".
         02    DFHDFHI   PICTURE X   VALUE  IS X"00".
         02    DFHBLINK  PICTURE X   VALUE  IS X"F1".
         02    DFHREVRS  PICTURE X   VALUE  IS X"F2".
         02    DFHUNDLN  PICTURE X   VALUE  IS X"F4".
         02    DFHMFIL   PICTURE X   VALUE  IS X"04".
         02    DFHMENT   PICTURE X   VALUE  IS X"02".
         02    DFHMFE    PICTURE X   VALUE  IS X"06".
         02    DFHUNNOD  PICTURE X   VALUE  IS X"4D".
         02    DFHUNIMD  PICTURE X   VALUE  IS X"C9".
         02    DFHUNNUM  PICTURE X   VALUE  IS X"D1".
         02    DFHUNNUB  PICTURE X   VALUE  IS X"D8".
      * ABOVE VALUE DFHUNNUB ADDED BY APAR PN67669
         02    DFHUNINT  PICTURE X   VALUE  IS X"D9".
         02    DFHUNNON  PICTURE X   VALUE  IS X"5D".
         02    DFHPROTI  PICTURE X   VALUE  IS X"E8".
         02    DFHPROTN  PICTURE X   VALUE  IS X"6C".
         02    DFHMT     PICTURE X   VALUE  IS X"01".
         02    DFHMFT    PICTURE X   VALUE  IS X"05".
         02    DFHMET    PICTURE X   VALUE  IS X"03".
         02    DFHMFET   PICTURE X   VALUE  IS X"07".
         02    DFHDFFR   PICTURE X   VALUE  IS X"00".
         02    DFHLEFT   PICTURE X   VALUE  IS X"08".
         02    DFHOVER   PICTURE X   VALUE  IS X"04".
         02    DFHRIGHT  PICTURE X   VALUE  IS X"02".
         02    DFHUNDER  PICTURE X   VALUE  IS X"01".
         02    DFHBOX-BIN  PIC 9(4) COMP VALUE 15.
      * ABOVE VALUE 15 = X'000F' ADDED BY PTM 81385 (APAR PN23267)
         02    FILLER REDEFINES DFHBOX-BIN.
           03  FILLER    PICTURE X.
           03  DFHBOX    PICTURE X.
         02    DFHSOSI   PICTURE X   VALUE  IS X"01".
         02    DFHTRANS  PICTURE X   VALUE  IS X"F0".
         02    DFHOPAQ   PICTURE X   VALUE  IS X"FF".
      
