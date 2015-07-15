Root =  ${HOME}

#
# Define directory Projet
#
Find_Jcl = JCL
Find_Prg = BATCH
Find_Tpr = CICS
Find_Spg =
Find_Map = MAP
Find_Copy = COPY
FILE_SCHEMAS = STDB2ORA
RDBMS_SCHEMAS = PJ01DB2


#
# Define extensions Files to be process
#
ext_jcl = jcl
ext_prg = cbl
ext_tpr = cbl
ext_spg =
ext_map = bms

#
# Defined extensions Files traducted
#
ext_trad = cbl
ext_pco = pco
ext_trad_copy = cpy
ext_trad_ksh = ksh
ext_trad_map = bms

#
# Define Version variables
# Information
# with GLOBAL_VERSION=CURRENT all -v option in the makefile are ignored
#
GLOBAL_VERSION = CURRENT
CATALOG = $(GLOBAL_VERSION)
TRAD = $(GLOBAL_VERSION)
TRAD_JCLZ = $(GLOBAL_VERSION)
DATA_TOOLS = $(GLOBAL_VERSION)
RECONCIL_COPY = $(GLOBAL_VERSION)
TIMEOUT = 900
TIMEOUT_PARSE = 300

#
# Define Config and opt files for refine
#
FILE_TRAD_JCL = "$(PARAM)/config-trad-JCL.desc"
FILE_TRAD_COBOL = "$(PARAM)/config-cobol-MVS"
COMM_TRADJCL = "-c $(FILE_TRAD_JCL)"

COMM_RECONCIL_COPY = "reconcil-copy-opt-imbr"

SYSTEM = $(PARAM)/system.desc
PRINT_INFO_OPT = -time
PREPARSE_FILES_FLAGS = -time

#
# Define Usage Logs
#
TMPDIR                := $(TMPPROJECT)
LOGDIR                := $(TRAVAIL)
LOG_FILE_SUFFIX       := $(shell date +%d%m%y-%H%M%S)
#
# Defined Logs Prepare
#
LOG_FILE_PREFIX_PRE       := preparse-
LOG_FILE_PRE              := $(LOGDIR)/$(LOG_FILE_PREFIX_PRE)$(LOG_FILE_SUFFIX)
LOG_FILE_FLAGS_PRE        := -log-file-base $(LOG_FILE_PRE)
#
# Defined Logs Traduction Cobol
#
LOG_FILE_PREFIX_TRAD       := translate-cobol-
LOG_FILE_TRAD              := $(LOGDIR)/$(LOG_FILE_PREFIX_TRAD)$(LOG_FILE_SUFFIX)
LOG_FILE_FLAGS_TRAD        := -log-file-base $(LOG_FILE_TRAD)
#
# Defined Logs Traduction Jclz
#
LOG_FILE_PREFIX_JCLZ       := translate-jclz-
LOG_FILE_JCLZ              := $(LOGDIR)/$(LOG_FILE_PREFIX_JCLZ)$(LOG_FILE_SUFFIX)
LOG_FILE_FLAGS_JCLZ        := -log-file-base $(LOG_FILE_JCLZ)
