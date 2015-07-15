//PJ01BBBC JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)
//*----------------------------------------------------------------
//* This job unload the VSAM records into a sequential file
//* Then, the sequential file may easily be viewed with z/OS
//* online browse facilities VSAM par RSSABB00.
//* This allows one to verify that VSAM initial load (or update)
//* was correctly performed by RSSABB00 and RSSABB02 batch programs
//*   File IN  :  PJ01AAA.SS.VSAM.CUSTOMER
//*   File OUT :  PJ01AAA.SS.QSAM.CUSTOMER.CHECK
//*----------------------------------------------------------------
//*-
//* Delete destination QSAM file
//SDELETE   EXEC PGM=IEFBR14,REGION=0M
//TODELETE  DD DSN=PJ01AAA.SS.QSAM.CUSTOMER.CHECK,
//             DISP=(MOD,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=266)
//*-
//CHECK    EXEC PGM=IDCAMS,REGION=0M
//DDOUT    DD DSN=PJ01AAA.SS.QSAM.CUSTOMER.CHECK,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),
//             DCB=(RECFM=FB,LRECL=266)
//DDIN     DD DSN=PJ01AAA.SS.VSAM.CUSTOMER,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO INFILE(DDIN) OUTFILE(DDOUT)
/*
