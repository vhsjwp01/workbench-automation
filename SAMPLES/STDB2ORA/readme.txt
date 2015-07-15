
# DESCRIPTION:

  This sample works as a Workbench template project for convert DB2 to ORACLE
  database case. The DB2 schema definitions are placed under directory source/DDL.

  Besides the DDL convert, this project can also be used to convert general FILE,
  JCL, CICS, BATCH, etc resources.

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
#   ORACLE_HOME   ORACLE directory path
#   ORACLE_SID    ORACLE instance name 
#   DBUSER        user name used to connect on ORACLE DataBase
#                 only if you don't use the Oracle OS Authentication (in this case
#                 set the variable to spaces or unset the variable)
#   DBPASSWORD    password used to connect on ORACLE DataBase
#                 only if you don't use the Oracle OS Authentication (in this case
#                 set the variable to spaces or unset the variable)
#   TUXDIR        TUXEDO directory path
#   ARTDIR        ART RUNTIME directory path
#   PDKSH         Public Domain ksh under Linux /Solaris, or ksh (version 88) under AIX

2. run . ./setenv
# note: if Oracle client has been installed for procob, please set proper ORACLE_CLIENT
# note: if you connect to DB by Oracle client, please append $ORACLE_SID to $DBCONNECT.
# note: for COBOL-IT instead of Microfocus COBOL, please validate COB="COBOL-IT"

3. Enter directores following to build binaries by make
	trf/reload/file/STDB2ORA/
	trf/reload/rdbms/PJ01DB2/src/
	trf/BATCH/
	trf/CICS/
	trf/DML/
	trf/MAP/
# note: add some statements under "WHEN SPACE" as same as under "WHEN OTHER" in trf/DML/dml_locking.cbl in case that it may fail in compile

4. execute reload by reload.sh
# Ensure ${ARTDIR}/Batch_RT/ejr is for ORACLE
# note: for COBOL-IT, need to link cobcrun to cobrun in $PATH, for example:
#   $ ln -s `which cobcrun` cobrun

5. execute batch runtime by batch.sh, see the result under data/

6. enter trf/, setup CICS runtime by setup.sh
# note: if you connect to DB by Oracle client, please switch OPENINFO entry in GROUPS section of trf/config/tux/ubbconfig 

7. execute CICS runtime by tmboot, you can enter transcation code SB00 on 3270 terminal
