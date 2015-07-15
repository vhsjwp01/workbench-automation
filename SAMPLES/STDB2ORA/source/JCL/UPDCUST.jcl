//PJ01BBBU JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,                       00010003
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)                        00020003
//*-----------------------------------------------------------------    00050103
//* THIS JOB RUNS THE RSSABB02 BATCH PROGRAM WHICH UPDATES THE DB2       0005020
//* CUSTOMERS TABLE. ACTIONS TO PERFORM ARE OBTAINED FROM A SEQUENTIAL  00050303
//* INPUT FILE.                                                         00050403
//*  > FILE IN   : PJ01AAA.S2.QSAM.CUSTOMER.UPDATE                         00050
//*  > TABLE OUT : PJ01DB2.ODCSF0                                          00050
//*-----------------------------------------------------------------    00050703
//STEPSORT EXEC PGM=SORT                                                00051001
//SYSPRINT DD  SYSOUT=*                                                 00052001
//SYSOUT   DD  SYSOUT=*                                                 00053001
//SYSIN DD *                                                            00058001
 SORT  FIELDS=(4,6,CH,A,1,3,CH,D)                                       00059001
 END                                                                    00059101
/*                                                                      00059201
//SORTIN   DD  DSN=PJ01AAA.S2.QSAM.CUSTOMER.UPDATE,DISP=SHR                00054
//SORTOUT  DD  DSN=&&TEMP,DISP=(,PASS),                                 00055004
//             UNIT=SYSDA,SPACE=(TRK,(20,10),RLSE),                     00056001
//             DCB=(RECFM=FB,LRECL=269)                                 00057001
//*                                                                     00060000
//UPDCUST  EXEC PGM=IKJEFT01,DYNAMNBR=20,REGION=0M                       0006100
//STEPLIB  DD DSN=PJ01AAA.S2.BATCH.LOAD,DISP=SHR                        00062001
//QSAMCUST DD DSN=&&TEMP,DISP=(OLD,DELETE)                              00081004
//SYSPRINT DD SYSOUT=*                                                  00070000
//SYSUDUMP DD SYSOUT=*                                                  01990000
//SYSOUT   DD SYSOUT=*                                                  00071002
//SYSTSPRT DD SYSOUT=*                                                  01930000
//SYSTSIN  DD *                                                         01940000
  DSN SYSTEM(DBP1)                                                      01950000
  RUN PROGRAM(RSSBBB02) PLAN (PLBATCH)                                  01960002
/*                                                                      00110000
//                                                                      00110000
