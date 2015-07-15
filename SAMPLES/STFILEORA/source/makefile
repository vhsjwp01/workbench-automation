MAKEFLAGS = k

include ${PARAM}/version.mk

#
# Search Architecture Type
#
TYPE_ARCHI = $(shell uname -m)

ifeq ($(TYPE_ARCHI), x86_64)
ARCHI=64
else
ARCHI=32
endif
#
# Affect Scripts PATH
#
ifeq ($(GLOBAL_VERSION), CURRENT)
TOOLS_SCRIPTS=${REFINEDIR}/scripts
TOOLS_DATA=${REFINEDIR}/convert-data
TOOLS_BIN=${REFINEDIR}/Linux$(ARCHI)
else
TOOLS_SCRIPTS=${REFINEDIR}/$(GLOBAL_VERSION)/scripts
TOOLS_DATA=${REFINEDIR}/$(GLOBAL_VERSION)/convert-data
TOOLS_BIN=${REFINEDIR}/$(GLOBAL_VERSION)/Linux$(ARCHI)
endif

#
# Verification before launch
#
CHECK_SCRIPTS=$(wildcard $(TOOLS_SCRIPTS))
CHECK_DATA=$(wildcard $(TOOLS_DATA))
CHECK_REFINE=$(wildcard ${REFINEDIR})

ifeq ($(CHECK_REFINE), )
$(error on REFINEDIR variable, not positionned or invalid directory ...)
endif

ifeq ($(CHECK_SCRIPTS), )
$(error Directory $(TOOLS_SCRIPTS) not present Abort makefile ...)
endif

ifeq ($(CHECK_DATA), )
$(error Directory $(TOOLS_DATA) not present Abort makefile ...)
endif

#
# Search directory in config Files for conversion
#
DIR_TRADJCL = $(shell grep '^ *root-skeleton *=' $(FILE_TRAD_JCL) | sed s:\"::g | awk '{ print $$NF }')
DIR_TRADCOBOL = $(shell grep '^ *target-dir:' $(FILE_TRAD_COBOL) | awk -F\" '{ print $$2 }' )
RECONCIL = $(shell cd $(DIR_TRADCOBOL); echo $$PWD)
ROOT_PROJECT = $(shell grep '^ *system ' $(SYSTEM)| awk '{ print $$2 }')
DIR_TRADDATA = $(DIR_TRADCOBOL)

#
# Search Jcls
#
ifdef Find_Jcl
dirs_jcl = $(patsubst %/,%,$(sort $(dir $(foreach dirs,$(Find_Jcl),$(wildcard $(dirs)/*.$(ext_jcl))))))
$(foreach dir,$(dirs_jcl),$(shell mkdir -p $(dir)/pob))

ifeq  ($(Pilote_jcl),)
LISTEJCL := $(foreach dir,$(dirs_jcl),$(wildcard $(dir)/*.$(ext_jcl)))
else
LISTEJCL := $(shell cat $(Pilote_jcl) | grep ".$(ext_jcl)" )
endif

DIR_POBJCL = $(patsubst %,%pob/,$(dir $(LISTEJCL)))
NAME_POBJCL = $(notdir $(patsubst %,%.pob,$(LISTEJCL)))
endif

#
# Search cobol batchs
#
ifdef Find_Prg
dirs_prg = $(patsubst %/,%,$(sort $(dir $(foreach dirs,$(Find_Prg),$(wildcard $(dirs)/*.$(ext_prg))))))
$(foreach dir,$(dirs_prg),$(shell mkdir -p $(dir)/pob))

ifeq  ($(Pilote_prg),)
LISTEPRG := $(foreach dir,$(dirs_prg),$(wildcard $(dir)/*.$(ext_prg)))
else
LISTEPRG := $(shell cat $(Pilote_prg) | grep ".$(ext_prg)" )
endif

LISTE_PRG_PCO := $(shell grep -l '^ *EXEC *SQL' $(LISTEPRG))
LISTE_PRG_CBL := $(filter-out $(LISTE_PRG_PCO), $(LISTEPRG))

DIR_POBPRG = $(patsubst %,%pob/,$(dir $(LISTEPRG)))
NAME_POBPRG = $(notdir $(patsubst %,%.pob,$(LISTEPRG)))
NAME_TRADPRG_CBL := $(addprefix $(DIR_TRADCOBOL),$(patsubst %.$(ext_prg),%.$(ext_trad),$(LISTE_PRG_CBL)))
NAME_TRADPRG_PCO := $(addprefix $(DIR_TRADCOBOL),$(patsubst %.$(ext_prg),%.$(ext_pco),$(LISTE_PRG_PCO)))
DIR_TRF_PRG = $(foreach dir,$(Find_Prg),$(DIR_TRADCOBOL)$(dir))
TRANSLATE_PURGE += $(NAME_TRADPRG_CBL) $(NAME_TRADPRG_PCO)
endif

#
# Search Cobol Cics
#
ifdef Find_Tpr
dirs_tpr = $(patsubst %/,%,$(sort $(dir $(foreach dirs,$(Find_Tpr),$(wildcard $(dirs)/*.$(ext_tpr))))))
$(foreach dir,$(dirs_tpr),$(shell mkdir -p $(dir)/pob))

ifeq  ($(Pilote_tpr),)
LISTETPR := $(foreach dir,$(dirs_tpr),$(wildcard $(dir)/*.$(ext_tpr)))
else
LISTETPR := $(shell cat $(Pilote_tpr) | grep ".$(ext_tpr)" )
endif

LISTE_TPR_PCO := $(shell grep -l '^ *EXEC *SQL' $(LISTETPR))
LISTE_TPR_CBL := $(filter-out $(LISTE_TPR_PCO), $(LISTETPR))

DIR_POBTPR = $(patsubst %,%pob/,$(dir $(LISTETPR)))
NAME_POBTPR = $(notdir $(patsubst %,%.pob,$(LISTETPR)))
NAME_TRADTPR_CBL = $(addprefix $(DIR_TRADCOBOL),$(patsubst %.$(ext_tpr),%.$(ext_trad),$(LISTE_TPR_CBL)))
NAME_TRADTPR_PCO = $(addprefix $(DIR_TRADCOBOL),$(patsubst %.$(ext_tpr),%.$(ext_pco),$(LISTE_TPR_PCO)))
DIR_TRF_TPR = $(foreach dir,$(Find_Tpr),$(DIR_TRADCOBOL)$(dir))
TRANSLATE_PURGE += $(NAME_TRADTPR_CBL) $(NAME_TRADTPR_PCO)
endif

#
# Search Cobol Subs
#
ifdef Find_Spg
dirs_spg = $(patsubst %/,%,$(sort $(dir $(foreach dirs,$(Find_Spg),$(wildcard $(dirs)/*.$(ext_spg))))))
$(foreach dir,$(dirs_spg),$(shell mkdir -p $(dir)/pob))

ifeq  ($(Pilote_sub),)
LISTESPG := $(foreach dir,$(dirs_spg),$(wildcard $(dir)/*.$(ext_spg)))
else
LISTESPG := $(shell cat $(Pilote_sub) | grep ".$(ext_spg)" )
endif

LISTE_SPG_PCO := $(shell grep -l '^ *EXEC *SQL' $(LISTESPG))
LISTE_SPG_CBL := $(filter-out $(LISTE_SPG_PCO), $(LISTESPG))

DIR_POBSPG = $(patsubst %,%pob/,$(dir $(LISTESPG)))
NAME_POBSPG = $(notdir $(patsubst %,%.pob,$(LISTESPG)))
NAME_TRADSPG_CBL = $(addprefix $(DIR_TRADCOBOL),$(patsubst %.$(ext_spg),%.$(ext_trad),$(LISTE_SPG_CBL)))
NAME_TRADSPG_PCO = $(addprefix $(DIR_TRADCOBOL),$(patsubst %.$(ext_spg),%.$(ext_pco),$(LISTE_SPG_PCO)))
DIR_TRF_SPG = $(foreach dir,$(Find_Spg),$(DIR_TRADCOBOL)$(dir))
TRANSLATE_PURGE += $(NAME_TRADSPG_CBL) $(NAME_TRADSPG_PCO)
endif

#
# Search cobol batchs
#
ifdef Find_Map
dirs_map = $(shell find $(Find_Map) -name "*.$(ext_map)" | xargs $(TOOLS_SCRIPTS)/dirname.pl 2> /dev/null | sort -u | sed ':a;N;$!ba;s/\n/ /g' ) 
$(foreach dir,$(dirs_map),$(shell mkdir -p $(dir)/pob))

ifeq  ($(Pilote_map),)
LISTEMAP := $(foreach dir,$(dirs_map),$(wildcard $(dir)/*.$(ext_map)))
else
LISTEMAP := $(shell cat $(Pilote_map) | grep ".$(ext_map)" )
endif
NAME_TRADMAP = $(addprefix $(DIR_TRADCOBOL),$(patsubst %.$(ext_map),%.$(ext_trad_map),$(LISTEMAP)))
DIR_TRF_MAP = $(foreach dir,$(Find_Map),$(DIR_TRADCOBOL)$(dir))
endif

#
# Search Copy files
#
ifdef Find_Copy
dirs_copy = $(patsubst %/,%,$(sort $(dir $(foreach dirs,$(Find_Copy),$(wildcard $(dirs)/*.$(ext_trad_copy))))))
$(foreach dir,$(dirs_copy),$(shell mkdir -p $(dir)/pob))

ifeq  ($(Pilote_copy),)
LISTECPY := $(foreach dir,$(dirs_copy),$(wildcard $(dir)/*.$(ext_trad_copy)))
else
LISTECPY := $(shell cat $(Pilote_copy) | grep ".$(ext_trad_copy)" )
endif
endif

#
# create directory necessary Used by R4Z
#
ifdef DIR_TRADCOBOL
$(shell mkdir -p $(DIR_TRF_PRG) $(DIR_TRF_TPR) $(DIR_TRF_SPG) $(DIR_TRF_MAP))
endif

ifdef TRAVAIL
$(shell mkdir -p $(TRAVAIL))
endif

ifdef TMPPROJECT
$(shell mkdir -p $(TMPPROJECT))
endif

# 
# Build lists with complexes hierarchies
#
PARSE = $(join $(DIR_POBPRG),$(NAME_POBPRG)) $(join $(DIR_POBTPR),$(NAME_POBTPR)) $(join $(DIR_POBSPG),$(NAME_POBSPG)) $(join $(DIR_POBJCL),$(NAME_POBJCL))
PARSE_FIRST = $(firstword $(PARSE))
PARSE_REST = $(filter-out $(PARSE_FIRST), $(PARSE))
TRADJCL = $(addprefix $(DIR_TRADJCL),$(patsubst %.$(ext_jcl),%.ksh,$(LISTEJCL)))
TRADPRG = $(NAME_TRADPRG_CBL)
TRADPRGPCO = $(NAME_TRADPRG_PCO)
TRADSPG = $(NAME_TRADSPG_CBL)
TRADSPGPCO = $(NAME_TRADSPG_PCO)
TRADTPR = $(NAME_TRADTPR_CBL)
TRADTPRPCO = $(NAME_TRADTPR_PCO)
TRADMAP = $(NAME_TRADMAP)
TRANSLATE_PURGE += $(TRADJCL)

#
# Build lists with simples hierarchies
#
DEPENDS = $(addsuffix .depends,$(LISTEPRG) $(LISTETPR) $(LISTESPG))
CBL = $(addsuffix .$(ext_prg),$(LISTEPRG)) $(addsuffix .$(ext_tpr),$(LISTETPR)) $(addsuffix .$(ext_spg),$(LISTESPG))


#
# Application Targets
#
usage:
	@cat ${PARAM}/usage.mk

help: usage

cleanpob:
	@echo "Clean Pob shrec cdm For all Files product by Refine ....."
	@find . -name "*.pob" -o -name "*.depends" -o -name "*.cdm" -o -name "*.shrec" | xargs rm -f

cleantranslate:
	@echo $(TRANSLATE_PURGE) | xargs rm -f
	@find $(DIR_TRADCOBOL)/Master-copy -name "*.$(ext_trad_copy)" -o -name "*.clean" | xargs rm -f
	@find $(DIR_TRADCOBOL)/reload -name "*.cbl" -o -name "*.ksh" -o -name "*.ctl" -o -name "*.sh" -o -name "*.pco" | xargs rm -f
	@find $(DIR_TRADCOBOL)/SQL -name "*.sql" -o -name "*.ksh" | xargs rm -f
	@find $(DIR_TRADCOBOL)/DML -name "*.cbl" -o -name "*.pco" | xargs rm -f

reconcil_copy:
	@echo "Use $(TOOLS_SCRIPTS)/$(COMM_RECONCIL_COPY) For reconcilation copies ...."
	@for file in `find $(RECONCIL) -name '*-copies'` ;\
	do \
		for copie in `find $$file -type f` ;\
		do \
			$(TOOLS_SCRIPTS)/coby.pl -om=0 $$copie $$copie.coby ;\
			ORIG=`echo $${copie##*-copies/}.orig | sed s:_V[0-9]*\.:\.:` ;\
			$(TOOLS_BIN)/addComment $$ORIG $$copie.coby $$copie ;\
			rm -f $$copie.coby ;\
		done ;\
		if test -f $${file%%.*}.$(ext_pco) ;\
		then \
			$(TOOLS_SCRIPTS)/$(COMM_RECONCIL_COPY) $(RECONCIL) $$file .$(ext_pco) ;\
		else \
			$(TOOLS_SCRIPTS)/$(COMM_RECONCIL_COPY) $(RECONCIL) $$file .$(ext_trad) ;\
		fi ;\
		echo "=========================================================" ;\
	done

buildmake:
	@for dir in $(DIR_TRF_PRG) $(DIR_TRF_TPR) $(DIR_TRF_SPG) ;\
	do \
		(cd $$dir || exit 5; buildmake.sh) ;\
	done
	@for schema in $(FILE_SCHEMAS) ;\
	do \
		(cd $(DIR_TRADCOBOL)reload/file/$$schema || exit 5; buildmake.sh) ;\
	done
	@for schema in $(RDBMS_SCHEMAS) ;\
	do \
		(cd $(DIR_TRADCOBOL)reload/rdbms/$$schema/src || exit 5; buildmake.sh) ;\
	done
	@(cd $(DIR_TRADCOBOL)DML; buildmake.sh)

$(PARSE_REST): $(PARSE_FIRST)
pob: $(PARSE_REST)

trad_jcl: $(TRADJCL)

trad_prg: $(TRADPRG) $(TRADPRGPCO)
trad_sub: $(TRADSPG) $(TRADSPGPCO)
trad_cics: $(TRADTPR) $(TRADTPRPCO)
trad_map: $(TRADMAP)
trad: trad_prg trad_sub trad_cics trad_map

ifeq '$(VERIF)' 'TRUE'
REAL_CMD := true
COMMENT := echo
else
REAL_CMD :=
COMMENT  := true
endif

protect_char = $(subst $$,$$$$,$(1))

#
# For generated the symtab
#
symtab-$(ROOT_PROJECT).pob:
	@$(COMMENT) Check : Build symtab-$(ROOT_PROJECT).pob Need Process .....
	@(cd $(LOGDIR);$(REAL_CMD) ${REFINEDIR}/refine analyze -v $(CATALOG) -s $(SYSTEM))

fast-final: symtab-$(ROOT_PROJECT).pob
	@(cd $(LOGDIR);$(REAL_CMD) ${REFINEDIR}/refine fast-final -v $(CATALOG) -s $(SYSTEM))

# Preprocess for cobol programs
prep:
	@echo "START PREPROCESSING" 
	@for file in $(LISTEPRG) $(LISTESPG) $(LISTETPR) $(LISTECPY);\
	do \
		if [ ! -f $$file.orig ]; then \
			if test "x$$FREE_FORMAT" = "xY" ; then \
				sed 's/\(.*\)/      \1/g' $$file > $$file.orig; \
			else \
				cp $$file $$file.orig; \
			fi; \
			sed 's/^.\{6\}//g' $$file.orig > $$file; \
		fi; \
	done
	@echo "END PREPROCESSING"

catalog: prep
	@(cd $(LOGDIR);$(REAL_CMD) ${REFINEDIR}/refine catalog -v $(CATALOG) -s $(SYSTEM))


#
# Global data Target
#
DataConvert: FileConvert RdbmsConvert

#
# -i Organize the delivery under $(DIR_TRADDATA)
# -g generate 
# -m post-Process
#
FileConvert: symtab-$(ROOT_PROJECT).pob
	@for schema in $(FILE_SCHEMAS) ;\
	do \
		echo "Generated $$schema Files ...." ;\
		(cd $(LOGDIR); $(TOOLS_DATA)/file.sh -gmi $(DIR_TRADDATA) $$schema) ;\
	done
	@if test ! -z "$(FILE_SCHEMAS)" ;\
	then \
		(cd $(LOGDIR);$(TOOLS_DATA)/file.sh -s $(DIR_TRADDATA) $(shell echo $(FILE_SCHEMAS) | sed 's/ /,/g' ));\
	fi

#
# -i Organize the delivery under $(DIR_TRADDATA)
# -g generate 
# -m post-Process
# -r remove schema name (remove qualification)
# -C DDL conversion (no error when "warning" occurs only)
#
RdbmsConvert: symtab-$(ROOT_PROJECT).pob 
	@for schema in $(RDBMS_SCHEMAS) ;\
	do \
		echo "Generated $$schema Rdbms ...." ;\
		(cd $(LOGDIR); $(TOOLS_DATA)/rdbms.sh -Cgrmi $(DIR_TRADDATA) $$schema) ;\
	done
	@if test ! -z "$(RDBMS_SCHEMAS)" ;\
	then \
		(cd $(LOGDIR);$(TOOLS_DATA)/rdbms.sh -s $(DIR_TRADDATA) $(shell echo $(RDBMS_SCHEMAS) | sed 's/ /,/g' ));\
	fi

#
# Force the Build or the rebuild Symtab with Analyse Target
#
Analyse:
	@$(COMMENT) Check : Build symtab-$(ROOT_PROJECT).pob Need Process .....
	@(cd $(LOGDIR);$(REAL_CMD) ${REFINEDIR}/refine analyze -v $(CATALOG) -s $(SYSTEM))

# Rules of depends ( make pob )
# PRG/pob/XXXX.pl1.pob: PRG/XXXX.pl1
define PARSE_template
$(call protect_char,$(1)): $(subst /pob,,$(basename $(call protect_char,$(1))))
	@$(COMMENT) Check : Parse of $$< Need Process .. To obtain $$@
	@$(REAL_CMD) ${REFINEDIR}/refine preparse-files -v $(CATALOG) $(PREPARSE_FILES_FLAGS) \
	-s $(SYSTEM) $(LOG_FILE_FLAGS_PRE) '$(CURDIR)/$$<'
endef

# Rules of depends ( make trad_jcl )
# ../trf/JCL/XXXX.ksh: JCL/XXXX.$(ext_jcl) JCL/pob/XXXX.$(ext_jcl).pob      ../trf defined in  $(DIR_TRADJCL)
define TRADJCL_template
$(call protect_char,$(1)): $(basename $(subst $(DIR_TRADJCL),,$(call protect_char,$(1)))).$(ext_jcl) $(subst $(DIR_TRADJCL),,$(patsubst %,%pob/,$(dir $(call protect_char,$(1))))$(notdir $(basename $(call protect_char,$(1))))).$(ext_jcl).pob
	@$(COMMENT) Check : Traduction of $$< Need Process .. To obtain $$@
	@$(REAL_CMD) ${REFINEDIR}/refine jclz-unix -v $(TRAD_JCLZ) -s $(SYSTEM) $(LOG_FILE_FLAGS_JCLZ) $(COMM_TRADJCL) '$$<'
endef


# Rules of depends ( make trad-cobol)
define TRADCOBOL_template
$(call protect_char,$(1)): $(basename $(subst $(DIR_TRADCOBOL),,$(call protect_char,$(1)))).$(ext_prg) $(subst $(DIR_TRADCOBOL),,$(patsubst %,%pob/,$(dir $(call protect_char,$(1))))$(notdir $(basename $(call protect_char,$(1))))).$(ext_prg).pob
	@$(COMMENT) Check : $(4) Trad of $$< Need Process .. To obtain $$@ With $(2) $(5) Type Option
	@$(REAL_CMD) ${REFINEDIR}/refine cobol-convert -v $(TRAD) -limit=50 -s $(SYSTEM) $(LOG_FILE_FLAGS_TRAD) -c $(2) -cobol-type $(3) -tpe $(4) $(5) '$$<'
	-@$(REAL_CMD) $(TOOLS_SCRIPTS)/coby.pl -om=0 $$@ $$@.coby
	-@$(REAL_CMD) $(TOOLS_BIN)/addComment $$<.orig $$@.coby $$@
	-@$(REAL_CMD) rm -f $$@.coby
endef

# Rules of depends ( make trad_map )
# ../trf/MAP/XXXX.bms: MAP/XXXX.$(ext_map)
define TRADMAP_template
$(call protect_char,$(1)): $(basename $(subst $(DIR_TRADCOBOL),,$(call protect_char,$(1)))).$(ext_map)
	@$(COMMENT) Check : Copy of $$< Need Process .. To obtain $$@
	@$(REAL_CMD) echo "Copy file $$< To $$@"
	@$(REAL_CMD) cp $$< $$@
endef

$(foreach e,$(PARSE),$(eval $(call PARSE_template,$(e))))
$(foreach e,$(TRADJCL),$(eval $(call TRADJCL_template,$(e))))
$(foreach e,$(TRADPRG),$(eval $(call TRADCOBOL_template,$(e),$(FILE_TRAD_COBOL),batch,$(ext_trad))))
$(foreach e,$(TRADPRGPCO),$(eval $(call TRADCOBOL_template,$(e),$(FILE_TRAD_COBOL),batch,$(ext_pco))))
$(foreach e,$(TRADSPG),$(eval $(call TRADCOBOL_template,$(e),$(FILE_TRAD_COBOL),sub,$(ext_trad))))
$(foreach e,$(TRADSPGPCO),$(eval $(call TRADCOBOL_template,$(e),$(FILE_TRAD_COBOL),sub,$(ext_pco))))
$(foreach e,$(TRADTPR),$(eval $(call TRADCOBOL_template,$(e),$(FILE_TRAD_COBOL),tpr,$(ext_trad),-cics)))
$(foreach e,$(TRADTPRPCO),$(eval $(call TRADCOBOL_template,$(e),$(FILE_TRAD_COBOL),tpr,$(ext_pco),-cics)))
$(foreach e,$(TRADMAP),$(eval $(call TRADMAP_template,$(e))))
