
============ Help usage =============================
This makefile process all or selected files for R4Z ..

Extentions of Files names available :
-----------------------------------
  .pob		: 	Result of parsing (Under the directory pob of the component)
  .cdm		: 	Result of parsing without rules, generated automaticly by the parser
  .shrec	:	Result of parsing without rules, generated automaticly by the parser

Available targets :
-----------------
  pob		: build the parse process to obtain pob files
  trad_prg	: Build the cobol conversion for all batchs programs defined in Find_Prg variable in version.mk
  trad_cics	: Build the cobol conversion for all cics programs defined in Find_Tpr variable in version.mk
  trad_sub	: Build the cobol conversion for all subs programs defined in Find_Spg variable in version.mk
  trad		: Build the cobol conversion and include theses target : trad_prg trad_spg trad_tpr
  trad_jcl	: Build the jcl conversion for all jcls defined in Find_Jcl variable in version.mk
  FileConvert	: Run the file converter for all schema defined in FILE_SCHEMAS variable in version.mk
  RdbmsConvert	: Run the Rdbms converter for all schema defined in RDBMS_SCHEMAS variable in version.mk
  DataConvert	: Build the Data converted global process ( FileConvert and RdbmsConvert )
  reconil_copy	: Run the reconciliation of copies files
  Analyse	: Build the symtab with force mode and take all modifications effects for all components
  fast-final	: Generate the Reports
  usage|help	: Display this content usage

Clean targets :
-------------
  cleanpob	: Clean all files *.pob *.cdm *.shrec *.depends
  cleantranslate: Clean all translates files under trf directory issues of all translates process

Detail Usage example :
--------------------
  Usage in check mode	: Use VERIF=TRUE on the command line (No command are executed, display only on the terminal)
  Sample		: make VERIF=TRUE trad
  Usage in per unit	: make PRG/pob/SAMPLE.batch.pob (For parse The PRG/SAMPLE.batch)
  Usage in global mode	: make pob

Configuration File :
------------------
The ${PARAM}/version.mk is used by the makefile, it contains all descriptions variables for all components.

All hierarchies that contains components to be translate mut be defined in Find_XXX variables:
Find_Jcl , Find_Prg , Find_Tpr , Find_Spg , Find_Map
If you don't have a kind of components you stay the varaibale empty : Find_Spg = 
If you have multiply hierarchies for one kind of components : Find_Prg = XX YY

For all hierarchies to be process, you must selected the extensions files in ext_XXX variables:
ext_jcl , ext_prg , ext_tpr , ext_spg , ext_map

You must defined the extensions results files after translations:
ext_trad , ext_pco , ext_trad_copy , ext_trad_ksh , ext_trad_map

