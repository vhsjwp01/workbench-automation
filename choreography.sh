#!/bin/bash
#set -x

################################################################################
#                      S C R I P T    D E F I N I T I O N
################################################################################
#

#-------------------------------------------------------------------------------
# Revision History
#-------------------------------------------------------------------------------
# 20150729     Jason W. Plummer          Original: A generic script to do 
#                                        something

################################################################################
# DESCRIPTION
################################################################################
#

# NAME: <name>
# 
# This script performs a <something> with <things>
#
# OPTIONS:
#
# --<option 1>        - <something>

################################################################################
# CONSTANTS
################################################################################
#

PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin
TERM=vt100
export TERM PATH

SUCCESS=0
ERROR=1

# This directory houses eclipse/ART WB harvested script assets
WB_AUTOMATE="/shared/wb_automate"

STDOUT_OFFSET="    "

SCRIPT_NAME="${0}"

USAGE_ENDLINE="\n${STDOUT_OFFSET}${STDOUT_OFFSET}${STDOUT_OFFSET}${STDOUT_OFFSET}"
USAGE="${SCRIPT_NAME}${USAGE_ENDLINE}"
USAGE="${USAGE}[ --<option 1> <something> ]${USAGE_ENDLINE}"

################################################################################
# VARIABLES
################################################################################
#

err_msg=""
exit_code=${SUCCESS}

sysin_ext="sysin"
cics_ext="cbl"
proc_ext="proc"
jcl_ext="jcl"
copy_ext="cpy"
map_ext="bms"
ddl_ext="sql"
batch_ext="cbl"

# The following comes from: /<eclipse workspace>/scripts/project.txt
ProjectName="Brad"                            # needs to be an input arg?
ParallelNum=1
WorkbenchPath="/opt/tuxedo/art_wb12.1.3.0.0"  # needs to be an input arg?
#export LocationOfAssets="/shared/WallyWB"            # needs to be an input arg?
export LocationOfAssets="${WB_AUTOMATE}/input"            # needs to be an input arg?
WBLOGLEVEL="INFO"
WBEXITONERROR="YES"
OnlyParsingPhase=""
TargetCOBOLCompiler="COBOL-IT"
TargetDataBase="ORACLE"

################################################################################
# SUBROUTINES
################################################################################
#

# WHAT: Subroutine f__check_command
# WHY:  This subroutine checks the contents of lexically scoped ${1} and then
#       searches ${PATH} for the command.  If found, a variable of the form
#       my_${1} is created.
# NOTE: Lexically scoped ${1} should not be null, otherwise the command for
#       which we are searching is not present via the defined ${PATH} and we
#       should complain
#
f__check_command() {
    return_code=${SUCCESS}
    my_command="${1}"

    if [ "${my_command}" != "" ]; then
        my_command_check=`unalias "${i}" 2> /dev/null ; which "${1}" 2> /dev/null`

        if [ "${my_command_check}" = "" ]; then
            return_code=${ERROR}
        else
            eval my_${my_command}="${my_command_check}"
        fi

    else
        echo "${STDOUT_OFFSET}ERROR:  No command was specified"
        return_code=${ERROR}
    fi

    return ${return_code}
}

#-------------------------------------------------------------------------------

# Append item to array
#
append_item() {
    return_code=${SUCCESS}

    if [ "${1}" != "" -a "${2}" != "" ]; then
        eval let element_count=\${#${1}[@]}
        eval ${1}[$element_count]="${2}"
    else
        /bin/false
    fi

    return ${return_code}
}

#-------------------------------------------------------------------------------

append_list() {
    return_code=${SUCCESS}

    if [ "${1}" != "" -a "${2}" != "" ]; then
        is_empty=`eval "echo \\$${1}"`

        if [ "${is_empty}" = "" ]; then
            eval "${1}=\"${2}\""
        else
            eval "${1}=\"`echo \\"\\$${1}\\"` ${2}\""
        fi

    else
        /bin/false
    fi

    return ${return_code}
}

#-------------------------------------------------------------------------------

################################################################################
# MAIN
################################################################################
#

# WHAT: Make sure we have some useful commands
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    for command in awk basename chmod curl date dirname egrep false file find host mkdir rm sed sort tail tee wc ; do
        unalias ${command} > /dev/null 2>&1
        f__check_command "${command}"

        if [ ${?} -ne ${SUCCESS} ]; then
            let exit_code=${exit_code}+1
        fi

    done

fi

#====

# WHAT: Import data
# WHY:  Asked to
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    processing_verb="import"
    umask 007
    script_dir="${WB_AUTOMATE}/scripts"
    input_dir="${WB_AUTOMATE}/input"
    import_dir="${WB_AUTOMATE}/imported"
    prepare_dir="${WB_AUTOMATE}/prepared"
    export import_dir prepare_dir

    if [ -d "${script_dir}" -a -d "${input_dir}" -a -d "${import_dir}" ]; then

        # Setup item lists for import
        raw_sysin_list=`cd "${input_dir}/SYSIN" && ls *.${sysin_ext}`
        raw_cics_list=`cd "${input_dir}/CICS" && ls *.${cics_ext}`
        raw_proc_list=`cd "${input_dir}/PROC" && ls *.${proc_ext}`
        raw_jcl_list=`cd "${input_dir}/JCL" && ls *.${jcl_ext}`
        raw_copy_list=`cd "${input_dir}/COPY" && ls *.${copy_ext}`
        raw_map_list=`cd "${input_dir}/MAP" && ls *.${map_ext}`
        raw_ddl_list=`cd "${input_dir}/DDL" && ls *.${ddl_ext}`
        raw_batch_list=`cd "${input_dir}/BATCH" && ls *.${batch_ext}`

        #echo "Raw ddl list for import: ${raw_ddl_list}"

        for list_name in sysin cics proc jcl copy map ddl batch ; do
            clean_list="${list_name}_list"
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name}_list\\""` ; do
                #echo "List item var: ${list_item}"
                eval "append_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in SYSIN CICS PROC JCL COPY MAP DDL BATCH ; do

            if [ ! -d "${import_dir}/${target_dir}" ]; then
                mkdir -p "${import_dir}/${target_dir}"
            fi

        done

        this_makefile="${script_dir}/makefile.import"

        # Try importing
        if [ -e "${this_makefile}" ]; then
            cd "${script_dir}" && make -d -f "${this_makefile}" all
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                err_msg="Failed to import targets"
            fi

        else
            err_msg="Could not locate makefile \"${this_makefile}\""
            exit_code=${ERROR}
        fi

    else
        err_msg="Could not find all needed directories for ${processing_verb} processing"
        exit_code=${ERROR}
    fi

fi

# WHAT: Prepare data
# WHY:  Asked to
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    processing_verb="prepare"
    prepare_dir="${WB_AUTOMATE}/prepared"
    export prepare_dir

    uc_sysin_ext=`echo "${sysin_ext}" | tr '[a-z]' '[A-Z]'`
    uc_cics_ext=`echo "${cics_ext}" | tr '[a-z]' '[A-Z]'`
    uc_proc_ext=`echo "${proc_ext}" | tr '[a-z]' '[A-Z]'`
    uc_jcl_ext=`echo "${jcl_ext}" | tr '[a-z]' '[A-Z]'`
    uc_copy_ext=`echo "${copy_ext}" | tr '[a-z]' '[A-Z]'`
    uc_map_ext=`echo "${map_ext}" | tr '[a-z]' '[A-Z]'`
    uc_ddl_ext=`echo "${ddl_ext}" | tr '[a-z]' '[A-Z]'`
    uc_batch_ext=`echo "${batch_ext}" | tr '[a-z]' '[A-Z]'`

    if [ -d "${script_dir}" -a -d "${import_dir}" -a -d "${prepare_dir}" ]; then

        # Setup item lists for prepare
        raw_sysin_list=`cd "${import_dir}/SYSIN" && ls *.${sysin_ext}`
        raw_cics_list=`cd "${import_dir}/CICS" && ls *.${cics_ext}`
        raw_proc_list=`cd "${import_dir}/PROC" && ls *.${proc_ext}`
        raw_jcl_list=`cd "${import_dir}/JCL" && ls *.${jcl_ext}`
        raw_copy_list=`cd "${import_dir}/COPY" && ls *.${copy_ext}`
        raw_map_list=`cd "${import_dir}/MAP" && ls *.${map_ext}`
        raw_ddl_list=`cd "${import_dir}/DDL" && ls *.${ddl_ext}`
        raw_batch_list=`cd "${import_dir}/BATCH" && ls *.${batch_ext}`

        # Try preparing
        for list_name in sysin cics proc jcl copy map ddl batch ; do
            eval "${list_name}_list=\"\""
        done

        for list_name in sysin cics proc jcl copy map ddl batch ; do
            clean_list="${list_name}_list"
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name}_list\\""` ; do
                #echo "List item var: ${list_item}"
                eval "append_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in SYSIN CICS PROC JCL COPY MAP DDL BATCH ; do

            if [ ! -d "${prepare_dir}/${target_dir}" ]; then
                mkdir -p "${prepare_dir}/${target_dir}"
            fi

        done

        this_makefile="${script_dir}/makefile.prepare"

        export uc_copy_list=`echo "${copy_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_copy_ext}/\.${copy_ext}/g"`
        export uc_sysin_list=`echo "${sysin_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_sysin_ext}/\.${sysin_ext}/g"`
        export uc_batch_list=`echo "${batch_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_batch_ext}/\.${batch_ext}/g"`
        export uc_cics_list=`echo "${cics_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_cics_ext}/\.${cics_ext}/g"`
        export uc_ddl_list=`echo "${ddl_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_ddl_ext}/\.${ddl_ext}/g"`
        export uc_map_list=`echo "${map_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_map_ext}/\.${map_ext}/g"`
        export uc_jcl_list=`echo "${jcl_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_jcl_ext}/\.${jcl_ext}/g"`
        export uc_proc_list=`echo "${proc_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_proc_ext}/\.${proc_ext}/g"`

        if [ -e "${this_makefile}" ]; then
            cd "${script_dir}" && make -d -f "${this_makefile}" all
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                err_msg="Failed to prepare targets"
            fi

        else
            err_msg="Could not locate makefile \"${this_makefile}\""
            exit_code=${ERROR}
        fi

    else
        err_msg="Could not find all needed directories for ${processing_verb} processing"
        exit_code=${ERROR}
    fi

fi

# WHAT: Perform character munging on the batch, cics, and copy Cobol targets
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "atta boy"

    # Look for any regex files located in <folder>
    # where filename indicates the prepared folder in which to operate

    
fi


#====

# WHAT: Complain if necessary and exit
# WHY:  Success or failure, either way we are through
#
if [ ${exit_code} -ne ${SUCCESS} ]; then

    if [ "${err_msg}" != "" ]; then
        echo
        echo -ne "${STDOUT_OFFSET}ERROR:  ${err_msg} ... processing halted\n"
        echo
    fi

    echo
    echo -ne "${STDOUT_OFFSET}USAGE:  ${USAGE}\n"
    echo
fi

exit ${exit_code}
