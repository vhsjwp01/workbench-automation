//PJ01BBBL JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,                       00010004
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)                        00020004
//*-----------------------------------------------------------------    00030000
//* This job runs the RSSABB00 batch program which load the VSAM         0004000
//* customers file using data obtained from a sequential file.          00041004
//*  > File In  : PJ01AAA.SS.QSAM.CUSTOMER                                 00042
//*  > File Out : PJ01AAA.SS.VSAM.CUSTOMER                                 00043
//*-----------------------------------------------------------------    00050000
//*                                                                     00060000
//LODVCUST  EXEC PGM=RSSABB00,REGION=0M                                  0006100
//STEPLIB  DD DSN=PJ01AAA.SS.BATCH.LOAD,DISP=SHR                        00062001
//SYSPRINT DD SYSOUT=*                                                  00070000
//SYSOUT   DD SYSOUT=*                                                  00071002
//VKSDCUST DD DSN=PJ01AAA.SS.VSAM.CUSTOMER,DISP=SHR                        00080
//QSAMCUST DD DSN=PJ01AAA.SS.QSAM.CUSTOMER,DISP=SHR                        00081
/*                                                                      00110000
