//PJ01BBBD JOB (ACCT#),'METAWARE',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)
//*-----------------------------------------------------------------
//* 1) 1st Step: DELVCUST
//*    Delete the existing file.
//* 2) 2nd Step: DEFVCUST
//*    Allocates the Simple Sample Application VSAM customers file
//*-----------------------------------------------------------------
//*
//*-Step 1: Delete...
//DELVCUST  EXEC PGM=IDCAMS,REGION=0M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE PJ01AAA.SS.VSAM.CUSTOMER
 SET MAXCC=0
/*
//*
//*-Step 2: Define...
//DEFVCUST EXEC PGM=IDCAMS,REGION=0M
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEF CLUSTER(                         -
   NAME(PJ01AAA.SS.VSAM.CUSTOMER)         -
            IXD                        -
            SHR(3)                     -
            REC(300)                   -
            RECORDSIZE(266 266)        -
            FREESPACE(5 5)             -
            KEYS(6 0))                 -
        DATA(NAME(PJ01AAA.SS.VSAM.CUSTOMER.DATA)   -
                   UNIQUE)                      -
        INDEX(NAME(PJ01AAA.SS.VSAM.CUSTOMER.INDEX) -
                   UNIQUE)
/*
