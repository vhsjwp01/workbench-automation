//PJ01BBBU JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,                       00010003
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)                        00020003
//*-----------------------------------------------------------------    00050103
//* This job runs the RSSABB02 batch program which updates the VSAM      0005020
//* customers file. Actions to perform are obtained from a sequential   00050303
//* input file.                                                         00050403
//*  > File In  : PJ01AAA.SS.QSAM.CUSTOMER.UPDATE                          00050
//*  > File Out : PJ01AAA.SS.VSAM.CUSTOMER                                 00050
//*-----------------------------------------------------------------    00050703
//STEPSORT EXEC PGM=SORT                                                00051001
//SYSPRINT DD  SYSOUT=*                                                 00052001
//SYSOUT   DD  SYSOUT=*                                                 00053001
//SORTIN   DD  DSN=PJ01AAA.SS.QSAM.CUSTOMER.UPDATE,DISP=SHR                00054
//SORTOUT  DD  DSN=&&TEMP,DISP=(,PASS),                                 00055004
//             UNIT=SYSDA,SPACE=(TRK,(20,10),RLSE),                     00056001
//             DCB=(RECFM=FB,LRECL=269)                                 00057001
//SYSIN DD *                                                            00058001
 SORT  FIELDS=(4,6,CH,A,1,3,CH,D)                                       00059001
 END                                                                    00059101
/*                                                                      00059201
//*                                                                     00060000
//LODVCUST  EXEC PGM=RSSABB02,REGION=1M                                  0006100
//STEPLIB  DD DSN=PJ01AAA.SS.BATCH.LOAD,DISP=SHR                        00062000
//SYSPRINT DD SYSOUT=*                                                  00070000
//SYSOUT   DD SYSOUT=*                                                  00071000
//VKSDCUST DD DSN=PJ01AAA.SS.VSAM.CUSTOMER,DISP=SHR                        00080
//QSAMCUST DD DSN=&&TEMP,DISP=(OLD,DELETE)                              00081004
/*                                                                      00110000
