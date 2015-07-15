
# DESCRIPTION:

  This sample works as a Workbench template project for convert FILE to DB2
  database case. The files to be converted are configured at Datamap-STFILEDB2.re
  and mapper-STFILEDB2.re in directory param/file.

  Besides the FILE2DB2 convert, this project can also be used to convert general
  FILE, JCL, CICS, BATCH, etc resources.

# As precondition, before this project can been converted by Workbench,
# the following scripts and templates are needed.
#	batch.sh
#	reload.sh
#	tailor.sh
#	setenv.template
#	trf/config/tux/ubbconfig.template	

1. tailor setenv.template & trf/config/tux/ubbconfig.template (by tailor.sh)
# note: relevant parameters
#   COBDIR        MICROFOCUS COBOL directory path
#   DB2DIR        DB2 Connect directory path
#   DB2INSTANCE   DB2 instance name 
#   DB2BASE       DB2 DataBase name 
#   DBUSER        user name used to connect on DB2 DataBase
#   DBPASSWORD    password used to connect on DB2 DataBase
#   TUXDIR        TUXEDO directory path
#   ARTDIR        ART RUNTIME directory path
#   PDKSH         Public Domain ksh under Linux /Solaris, or ksh (version 88) under AIX

2. run . ./setenv
# note: You can use following command to test connection of DB2
#   $ db2 connect to $DBCONNECT
#   $ db2

3. Enter directores following to build binaries by gmake
	trf/reload/file/STFILEDB2/
	trf/BATCH/
	trf/CICS/
	trf/MAP/

4. execute reload by reload.sh
# Ensure ${ARTDIR}/Batch_RT/ejr is for DB2

5. Enter trf/DML, run gmake
# note: comment out "WHEN SPACE" statement of trf/DML/dml_locking.cbl in case of it may fail in compile

6. execute batch runtime by batch.sh, see the result under data/
# Ensure all relevant ksh scripts under trf/ have execute permission

7. enter trf/, setup CICS runtime by setup.sh

8. execute CICS runtime by tmboot, you can enter transcation code SA00 on 3270 terminal
