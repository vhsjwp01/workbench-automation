//PJ01BBBP JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,                       00010008
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)                        00020006
//*-----------------------------------------------------------------    00030000
//* This job runs the RSSABB01 batch program which produces a            0004100
//* customers list report. The report is saved in a sequentiel file.    00042006
//*  > File In  : PJ01AAA.SS.VSAM.CUSTOMER                                 00043
//*  > File Out : PJ01AAA.SS.QSAM.CUSTOMER.REPORT                          00044
//*-----------------------------------------------------------------    00050000
//*-                                                                    00061109
//SDELETE   EXEC PGM=IEFBR14,REGION=0M                                  00061209
//TODELETE  DD DSN=PJ01AAA.SS.QSAM.CUSTOMER.REPORT,                        00061
//             DISP=(MOD,DELETE),                                       00061409
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),                      00061509
//             DCB=(RECFM=FB,LRECL=133)                                 00061609
//*-                                                                    00061709
//LODVCUST  EXEC PGM=RSSABB01,REGION=0M                                  0006181
//STEPLIB  DD DSN=PJ01AAA.SS.BATCH.LOAD,DISP=SHR                        00062000
//SYSPRINT DD DSN=PJ01AAA.SS.QSAM.CUSTOMER.REPORT,                         00070
//             DISP=(NEW,CATLG,DELETE),                                 00070206
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),                      00070306
//             DCB=(RECFM=FB,LRECL=133)                                 00070406
//SYSOUT   DD SYSOUT=*                                                  00071001
//VKSDCUST DD DSN=PJ01AAA.SS.VSAM.CUSTOMER,DISP=SHR                        00080
//*-                                                                    00140009
