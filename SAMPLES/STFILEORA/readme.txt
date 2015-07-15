
# DESCRIPTION:

  This sample works as a Workbench template project for convert FILE to ORACLE
  database case. The files to be converted are configured at Datamap-STFILEORA.re
  and mapper-STFILEORA.re in directory param/file.

  Besides the FILE2ORACLE convert, this project can also be used to convert general
  FILE, JCL, CICS, BATCH, etc resources.

# As precondition, before this project can been converted by Workbench,
# the following scripts and templates are needed.
#	batch.sh
#	reload.sh
#	tailor.sh
#	setenv.template
#	trf/config/tux/ubbconfig.template	

1. tailor setenv.template, trf/config/tux/ubbconfig.template, 
   trf-jcl/config/ubbconfig.template, and trf-jcl/config/jesconfig.template(by tailor.sh)
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
# note: if you connect to DB by Oracle server, please remove $ORACLE_SID from $DBCONNECT.
# note: for COBOL-IT instead of Microfocus COBOL, please validate COB="COBOL-IT"

3. Enter directores following to build binaries by gmake
	trf/reload/file/STFILEORA/
	trf/BATCH/
	trf/CICS/
	trf/DML/
	trf/MAP/
# note: comment out "WHEN SPACE" statement of trf/DML/dml_locking.cbl in case of it may fail in compile
# note: you can also build them by running "build.sh"

4. enter trf-jcl/, run ". ./setenv_bat" to set environment for Batch runtime.

5. execute reload by trf-jcl/reload.sh
# note: for COBOL-IT, need to link cobcrun to cobrun in $PATH, for example:
#   $ ln -s `which cobcrun` cobrun

6. enter trf-jcl/, setup Batch runtime by setupjes.sh

7. boot Batch runtime servers by running "tmboot".

8. execute Batch runtime by trf-jcl/batch.sh, see the result under data/
# note: you can verify the job status by running 
# 	artjesadmin --> printjob (ptj) 
# 	it is successful if all of the five jobs are with status "DONE".
# 	if it fails, please see the logs under trf-jcl/LOGS/jeslog

9. enter trf/, run ". ./setenv_kix" to setenvironment for CICS runtime.

10. enter trf/, setup CICS runtime by setup.sh
# note: if you connect to DB by Oracle server, please switch OPENINFO entry in GROUPS section of trf/config/tux/ubbconfig 

11. execute CICS runtime by tmboot, you can enter transcation code SA00 on 3270 terminal
