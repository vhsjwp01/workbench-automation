//PJ01BBBP JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,                       00010008
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)                        00020006
//*-----------------------------------------------------------------    00030000
//* THIS JOB RUNS THE RSSABB01 BATCH PROGRAM WHICH PRODUCES A            0004100
//* CUSTOMERS LIST REPORT. THE REPORT IS SAVED IN A SEQUENTIEL FILE.    00042006
//*  > TABLE IN : PJ01DB2.ODCSF0                                           00043
//*  > FILE OUT : PJ01AAA.S2.QSAM.CUSTOMER.REPORT                          00044
//*-----------------------------------------------------------------    00050000
//JOBLIB  DD DSN=PJ01AAA.S2.BATCH.LOAD,DISP=SHR                         00062001
//*
//SDELETE  EXEC PGM=IEFBR14,REGION=0M                                   00061209
//TODELETE DD DSN=PJ01AAA.S2.QSAM.CUSTOMER.REPORT,                         00061
//            DISP=(MOD,DELETE),UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),     00061409
//            DCB=(RECFM=FB,LRECL=133)                                  00061609
//*-                                                                    00061709
//PRTCUST  EXEC PGM=IKJEFT01,DYNAMNBR=20,REGION=0M                       0006100
//SYSUDUMP DD SYSOUT=*                                                  01990000
//SYSOUT   DD SYSOUT=*                                                  00071002
//SYSTSPRT DD SYSOUT=*                                                  01930000
//SYSTSIN  DD *                                                         01940000
  DSN SYSTEM(DBP1)                                                      01950000
  RUN PROGRAM(RSSBBB01) PLAN (PLBATCH)                                  01960002
/*                                                                      00110000
//SYSPRINT DD DSN=PJ01AAA.S2.QSAM.CUSTOMER.REPORT,                         00070
//            DISP=(NEW,CATLG,DELETE),                                  00070206
//            UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),                       00070306
//            DCB=(RECFM=FB,LRECL=133,BLKSIZE=1330)                     00070406
//                                                                      00140009
