//PJ01BBBL JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,REGION=0M,             00010004
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)                        00020004
//*-----------------------------------------------------------------    00030000
//* This job runs the RSSBBB00 batch program which load the DB2          0004000
//* customers table using data obtained from a sequential file.         00041004
//*  > File  In  : PJ01AAA.S2.QSAM.CUSTOMER                                00042
//*  > Table Out : PJ01DB2.ODCSF0                                          00043
//*-----------------------------------------------------------------    00050000
//LODVCUST EXEC PGM=IKJEFT01,DYNAMNBR=20,REGION=0M                       0006100
//STEPLIB  DD DSN=PJ01AAA.S2.BATCH.LOAD,DISP=SHR                        00062001
//QSAMCUST DD DSN=PJ01AAA.S2.QSAM.CUSTOMER,DISP=SHR                        00081
//SYSPRINT DD SYSOUT=*                                                  00070000
//SYSUDUMP DD SYSOUT=*                                                  01990000
//SYSOUT   DD SYSOUT=*                                                  00071002
//SYSTSPRT DD SYSOUT=*                                                  01930000
//SYSTSIN  DD *                                                         01940000
  DSN SYSTEM(DBP1)                                                      01950000
  RUN PROGRAM(RSSBBB00) PLAN (PLBATCH)                                  01960002
/*                                                                      00110000
