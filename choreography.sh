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
WB_AUTOMATE="/Users/jplumme/projects/wb_automate"
TARGETS="sysin cics proc jcl copy map ddl batch"

PROJECT="${WB_AUTOMATE}"
TRAVAIL="${PROJECT}/Logs"
LOGS="${TRAVAIL}"
TMPPROJECT="${PROJECT}/tmp"

export PROJECT TRAVAIL LOGS TMPPROJECT

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

# File extensions (lower case)
sysin_ext="sysin"
cics_ext="cbl"
proc_ext="proc"
jcl_ext="jcl"
copy_ext="cpy"
map_ext="bms"
ddl_ext="sql"
batch_ext="cbl"

# File extensions (upper case)
uc_sysin_ext=`echo "${sysin_ext}" | tr '[a-z]' '[A-Z]'`
uc_cics_ext=`echo "${cics_ext}" | tr '[a-z]' '[A-Z]'`
uc_proc_ext=`echo "${proc_ext}" | tr '[a-z]' '[A-Z]'`
uc_jcl_ext=`echo "${jcl_ext}" | tr '[a-z]' '[A-Z]'`
uc_copy_ext=`echo "${copy_ext}" | tr '[a-z]' '[A-Z]'`
uc_map_ext=`echo "${map_ext}" | tr '[a-z]' '[A-Z]'`
uc_ddl_ext=`echo "${ddl_ext}" | tr '[a-z]' '[A-Z]'`
uc_batch_ext=`echo "${batch_ext}" | tr '[a-z]' '[A-Z]'`

# The following comes from: /<eclipse workspace>/scripts/project.txt
ProjectName="Brad"                            # needs to be an input arg?
ucProjectName=`echo "${ProjectName}" | tr '[a-z]' '[A-Z]'`
ParallelNum=1
export WorkbenchPath="/opt/tuxedo/art_wb12.1.3.0.0"  # needs to be an input arg?
#export LocationOfAssets="/shared/WallyWB"            # needs to be an input arg?
export LocationOfAssets="${WB_AUTOMATE}/input"            # needs to be an input arg?
export PARAM="${WB_AUTOMATE}/param"            # needs to be an input arg?
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

# Add element to array
#
add_to_array() {
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

# Add element to list
#
add_to_list() {
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

    for command in awk basename chmod curl date dirname egrep false file find host mkdir rm sed sort tail tee uname wc ; do
        unalias ${command} > /dev/null 2>&1
        f__check_command "${command}"

        if [ ${?} -ne ${SUCCESS} ]; then
            let exit_code=${exit_code}+1
        fi

    done

fi

#====

# WHAT: Set some distro specific vars
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    SCRIPT_BASE=`dirname "${0}"`
    os_type=`uname -s | tr '[A-Z]' '[a-z]'`
    cpu_arch=`uname -m | tr '[A-Z]' '[a-z]' | sed -e 's/i[345]86/i686/g'`

    case ${os_type} in

        linux)

            case ${cpu_arch} in

                x86_64)
                    REFINEDISTRIB="Linux64"
                ;;

                i686)
                    REFINEDISTRIB="Linux32"
                ;;

                *)
                    err_msg="Unsupported Linux CPU Architecture: \"${cpu_arch}\""
                    exit_code=${ERROR}
                ;;

            esac

            ;;

        *)
            err_msg="Unsupported OS Type: \"${os_type}\""
            exit_code=${ERROR}
        ;;

    esac

    if [ ${exit_code} -eq ${SUCCESS} ]; then
        export REFINEDISTRIB
    fi

fi


# WHAT: Import data
# WHY:  Asked to
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "IMPORT"
    processing_verb="import"
    umask 007
    script_dir="${WB_AUTOMATE}/scripts"
    input_dir="${WB_AUTOMATE}/input"
    import_dir="${WB_AUTOMATE}/imported"
    prepare_dir="${WB_AUTOMATE}/prepared"
    export import_dir prepare_dir

    if [ -d "${script_dir}" -a -d "${input_dir}" -a -d "${import_dir}" ]; then

        # Setup item lists for import
        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | tr '[a-z]' '[A-Z]'`
            eval "file_ext=\$${target_dir}_ext"

            if [ -d "${input_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir}_list=\"`cd ${input_dir}/${uc_target_dir} && ls *.${file_ext}`\""
            else
                echo "    WARNING:  Directory \"${input_dir}/${uc_target_dir}\" does not exist"
            fi

        done

        #echo "Raw ddl list for import: ${raw_ddl_list}"

        for list_name in ${TARGETS} ; do
            clean_list="${list_name}_list"
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name}_list\\""` ; do
                #echo "List item var: ${list_item}"
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | tr '[a-z]' '[A-Z]'`
            echo "    INFO:  Refreshing ${processing_verb} directory \"${import_dir}/${uc_target_dir}\""
            rm -rf "${import_dir}/${uc_target_dir}"
            mkdir -p "${import_dir}/${uc_target_dir}"
        done

        this_makefile="${script_dir}/makefile.${processing_verb}"

        # Try importing
        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"make -f ${this_makefile} all\" ... "
            cd "${script_dir}" && make -f "${this_makefile}" all > /dev/null 2>&1
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "FAILED"
                err_msg="${processing_verb} processing of targets failed"
            else
                echo "SUCCESS"
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
    echo "PREPARE"
    processing_verb="prepare"
    prepare_dir="${WB_AUTOMATE}/prepared"
    export prepare_dir


    if [ -d "${script_dir}" -a -d "${import_dir}" -a -d "${prepare_dir}" ]; then

        # Setup item lists for prepare
        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | tr '[a-z]' '[A-Z]'`
            eval "file_ext=\$${target_dir}_ext"

            if [ -d "${import_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir}_list=\"`cd ${import_dir}/${uc_target_dir} && ls *.${file_ext}`\""
            else
                echo "    WARNING:  Directory \"${import_dir}/${uc_target_dir}\" does not exist"
            fi
    
        done

        # Try preparing
        for list_name in ${TARGETS} ; do
            clean_list="${list_name}_list"
            eval "${clean_list}=\"\""
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name}_list\\""` ; do
                #echo "List item var: ${list_item}"
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | tr '[a-z]' '[A-Z]'`
            echo "    INFO:  Refreshing ${processing_verb} directory \"${prepare_dir}/${uc_target_dir}\""
            rm -rf "${prepare_dir}/${uc_target_dir}"
            mkdir -p "${prepare_dir}/${uc_target_dir}"
        done

        this_makefile="${script_dir}/makefile.${processing_verb}"

        export uc_copy_list=`echo "${copy_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_copy_ext}/\.${copy_ext}/g"`
        export uc_sysin_list=`echo "${sysin_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_sysin_ext}/\.${sysin_ext}/g"`
        export uc_batch_list=`echo "${batch_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_batch_ext}/\.${batch_ext}/g"`
        export uc_cics_list=`echo "${cics_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_cics_ext}/\.${cics_ext}/g"`
        export uc_ddl_list=`echo "${ddl_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_ddl_ext}/\.${ddl_ext}/g"`
        export uc_map_list=`echo "${map_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_map_ext}/\.${map_ext}/g"`
        export uc_jcl_list=`echo "${jcl_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_jcl_ext}/\.${jcl_ext}/g"`
        export uc_proc_list=`echo "${proc_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_proc_ext}/\.${proc_ext}/g"`

        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"make -f ${this_makefile} all\" ... "
            cd "${script_dir}" && make -f "${this_makefile}" all > /dev/null 2>&1
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "FAILED"
                err_msg="${processing_verb} processing of targets failed"
            else
                echo "SUCCESS"
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

# WHAT: Perform pre-conversion character munging
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "PRE-PROCESSING"

    # Look for any regex files located in <folder>
    # where filename indicates the prepared folder in which to operate
    preconvert_dir="${WB_AUTOMATE}/param/regex/pre_conversion"
    export preconvert_dir

    for target in ${TARGETS} ; do
        eval "file_ext=\$${target}_ext"
        uc_target=`echo "${target}" | tr '[a-z]' '[A-Z]'`

        # awk '{print $0}' BATCH | egrep "^\(\"" | sed -e 's/[()]//g' -e 's/\"//g' -e 's/\ \.\ /::/g'
        # Read in regex lines from "${preconvert_dir}/${uc_target}"
        if [ -e "${preconvert_dir}/${uc_target}" -a -s "${preconvert_dir}/${uc_target}" ]; then
            regex_lines=`strings "${preconvert_dir}/${uc_target}" | awk '{print $0}' | egrep -v "^#" | egrep "^\(\"" | sed -e 's/[()]//g' -e 's/\"//g' -e 's/\ \.\ /::/g' -e 's/\ /:ZZqC:/g'`

            # Use the regexes to replace file contents in ${prepare_dir} targets
            for regex_line in ${regex_lines} ; do

                if [ "${regex_line}" != "" ]; then
                    real_regex_line=`echo "${regex_line}" | sed -e 's/:ZZqC:/\ /g'`
                    left_side=`echo "${real_regex_line}" | awk -F'::' '{print $1}'`
                    right_side=`echo "${real_regex_line}" | awk -F'::' '{print $NF}'`

                    # Here we slurp in each file in the target directory and plow through each
                    # line skipping comment lines
                    target_files=`cd "${prepare_dir}/${uc_target}" 2> /dev/null && ls *.${file_ext} 2> /dev/null`

                    tmp_dir="/tmp/${USER}/$$"

                    if [ ! -d "${tmp_dir}" ]; then
                        mkdir -p "${tmp_dir}"
                    fi

                    for target_file in ${target_files} ; do
                        echo -ne "    INFO:  Pre-Processing \"${prepare_dir}/${uc_target}/${target_file}\" for regular expression translation ... "
                        tmp_file="${tmp_dir}/translate-pre-processing-${uc_target}-${target_file}.$$"
                        rm -f "${tmp_file}"
    
                        ${SCRIPT_BASE}/processing.pl --input_file "${prepare_dir}/${uc_target}" --output_file "${tmp_file}" --type "${target}" --src_regex "${left_side}" --dst_regex "${right_side}"
                        #stdbuf -oL awk '{print $0}' "${prepare_dir}/${uc_target}/${target_file}" | while IFS='' read -r input_line ; do
                        #    let is_comment=0
                        #    #echo "input line is: ${input_line}"
    
                        #    # Comments look like:
                        #    # - BATCH, COPY, and CICS: byte 7 == "*"
                        #    # - JCL PROCS: ^//*
    
                        #    case ${target} in
    
                        #        batch|copy|cics)
                        #            is_comment=`echo "${input_line}" | cut -b7 | egrep -c "\*"`
                        #        ;;
    
                        #        jcl|procs)
                        #            is_comment=`echo "${input_line}" | egrep -c "^//\*"`
                        #        ;;
    
                        #        *)
                        #            is_comment=`echo "${input_line}" | egrep -c "^#"`
                        #        ;;
    
                        #    esac
    
                        #    #echo "IS COMMENT: ${is_comment}"
                        #    if [ "${input_line}" = "" ]; then
                        #        echo -ne "${input_line}\n" >> ${tmp_file}
                        #    else
    
                        #        if [ ${is_comment} -gt 0 ]; then
                        #            #echo "* * * * FOUND COMMENT LINE * * * *"
                        #            echo -ne "${input_line}\n" >> ${tmp_file}
                        #        else
                        #            #echo "* * * * FOUND REGULAR LINE * * * *"
    
                        #            if [ "${left_side}" = "" ]; then
                        #                echo -ne "${input_line}\n" >> ${tmp_file}
                        #            else
                        #                right_now=`date`
                        #                original_line=`echo "${input_line}"`
                        #                echo -ne "${comment_prefix} The following line was commented out\n"     >> ${tmp_file}
                        #                echo -ne "${comment_prefix} ${right_now}\n"                             >> ${tmp_file}
                        #                echo -ne "${comment_prefix} by automation script ${0} PRE-PROCESSING\n" >> ${tmp_file}
                        #                echo -ne "${comment_prefix} Original line was:\n"                       >> ${tmp_file}
                        #                echo -ne "${comment_prefix} ${original_line}\n"                         >> ${tmp_file}
                        #                new_input_line=`echo "${input_line}" | sed -e "s?${left_side}?${right_side}?g"`
                        #                echo -ne "${new_input_line}\n"                                          >> ${tmp_file}
                        #            fi
    
                        #        fi
    
                        #    fi
    
                        #done
                        
                        # Move ${tmp_file} to ${prepare_dir}/${uc_target}/${target_file}
                        if [ -e "${tmp_file}" -a -s "${tmp_file}" ]; then
                            rsync "${tmp_file}" "${prepare_dir}/${uc_target}/${target_file}" > /dev/null 2>&1
                            echo "DONE"
                        else
                            echo "FAILED"
                        fi

                    done

                fi

            done

        fi

    done
    
fi

# WHAT: Perform analysis operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "ANALYZE"
    processing_verb="analyze"
    source_dir="${WB_AUTOMATE}/source"
    export source_dir


    if [ -d "${script_dir}" -a -d "${prepare_dir}" -a -d "${source_dir}" ]; then

        # Setup item lists for source
        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | tr '[a-z]' '[A-Z]'`
            eval "file_ext=\$${target_dir}_ext"

            if [ -d "${prepare_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir}_list=\"`cd ${prepare_dir}/${uc_target_dir} && ls *.${file_ext}`\""
            else
                echo "    WARNING:  Directory \"${prepare_dir}/${uc_target_dir}\" does not exist"
            fi
    
        done

        # Try analyzing
        for list_name in ${TARGETS} ; do
            clean_list="${list_name}_list"
            eval "${clean_list}=\"\""
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name}_list\\""` ; do
                #echo "List item var: ${list_item}"
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | tr '[a-z]' '[A-Z]'`
            echo "    INFO:  Refreshing ${processing_verb} directory \"${source_dir}/${uc_target_dir}\""
            rm -rf "${source_dir}/${uc_target_dir}"
            mkdir -p "${source_dir}/${uc_target_dir}"
        done

        this_makefile="${script_dir}/makefile.${processing_verb}"

        export uc_copy_list=`echo "${copy_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_copy_ext}/\.${copy_ext}/g"`
        export uc_sysin_list=`echo "${sysin_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_sysin_ext}/\.${sysin_ext}/g"`
        export uc_batch_list=`echo "${batch_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_batch_ext}/\.${batch_ext}/g"`
        export uc_cics_list=`echo "${cics_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_cics_ext}/\.${cics_ext}/g"`
        export uc_ddl_list=`echo "${ddl_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_ddl_ext}/\.${ddl_ext}/g"`
        export uc_map_list=`echo "${map_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_map_ext}/\.${map_ext}/g"`
        export uc_jcl_list=`echo "${jcl_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_jcl_ext}/\.${jcl_ext}/g"`
        export uc_proc_list=`echo "${proc_list}" | tr '[a-z]' '[A-Z]' | sed -e "s/\.${uc_proc_ext}/\.${proc_ext}/g"`

        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"make -f ${this_makefile} all\" ... "
            cd "${script_dir}" && make -f "${this_makefile}" all > /dev/null 2>&1
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "FAILED"
                err_msg="${processing_verb} processing of targets failed"
            else
                echo "SUCCESS"
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

# WHAT: Perform catalog operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "CATALOG"
    processing_verb="catalog"
    source_dir="${WB_AUTOMATE}/source"
    param_dir="${PARAM}"
    report_dir="${source_dir}/Reports-${ucProjectName}"
    export source_dir

    if [ -d "${script_dir}" -a -d "${source_dir}" -a -d "${param_dir}" ]; then

        # Refresh reporting dir
        if [ -d "${report_dir}" ]; then
            echo "    INFO:  Removing reporting directory \"${report_dir}\""
            rm -rf "${report_dir}"
        fi

        this_makefile="${source_dir}/makefile.${processing_verb}"

        if [ -e "${this_makefile}" ]; then

            # Reconstruct "${param_dir}/system.desc"
            if [ -e "${param_dir}/system.desc" ]; then
                rm -f "${param_dir}/system.desc"
            fi

            # Clean out pob subfolders
            for target_dir in ${TARGETS} ; do
                uc_target_dir=`echo "${target_dir}" | tr '[a-z]' '[A-Z]'`

                if [ -d "${source_dir}/${uc_target_dir}/pob" ]; then
                    rm -rf "${source_dir}/${uc_target_dir}/pob"
                fi

            done

            # Create ${param_dir}/system.desc from template, then run make
            if [ -e "${param_dir}/system.desc.template" ]; then
                cp -p "${param_dir}/system.desc.template" "${param_dir}/system.desc"
                sed -i -e "s?::PROJECT_NAME::?${ProjectName}?g" -e "s?::SOURCE_DIR::?${source_dir}?g" "${param_dir}/system.desc"
                echo -ne "    INFO:  Running \"make -f ${this_makefile} cleanpob\" ... "
                cd "${source_dir}" && make -f "${this_makefile}" cleanpob 
                echo -ne "    INFO:  Running \"make -f ${this_makefile} ${processing_verb}\" ... "
                cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
                exit_code=${?}

                if [ ${exit_code} -ne ${SUCCESS} ]; then
                    echo "FAILED"
                    err_msg="${processing_verb} processing of targets failed"
                else
                    echo "SUCCESS"
                fi

            else
                err_msg="Could not locate template file \"${param_dir}/system.desc.template\" which is used to generate \"${param_dir}/system.desc\" file"
                exit_code=${ERROR}
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

# WHAT: Look for items of concern in the Anomalies report
# WHY:  Cannot proceed without some analysis of previous operation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "ANOMALIES"
    report_dir="${source_dir}/Reports-${ucProjectName}"

    if [ -d "${report_dir}" ]; then
        anomaly_report="${report_dir}/report-${ucProjectName}-Anomalies"

        if [ -e "${anomaly_report}" -a -s "${anomaly_report}" ]; then
            echo -ne "    INFO:  Checking Anomaly report for issues of concern ... "

            let max_warnings=20
            let max_missing=1

            let fatal_count=`egrep -c ";FATAL;" "${anomaly_report}"`
            let error_count=`egrep -c ";ERROR;" "${anomaly_report}"`
            let warning_count=`egrep -c ";WARNING;" "${anomaly_report}"`
            let missing_count=`egrep -c ";MISSING;" "${anomaly_report}"`

            if [ ${fatal_count} -gt 0 -o ${error_count} -gt 0 -o ${warning_count} -ge ${max_warnings} -o ${missing_count} -ge ${max_missing} ]; then
                echo "issues found [FAILED]"
            else
                echo "none found [SUCCESS]"
            fi

            if [ ${fatal_count} -gt 0 ]; then
                echo "    ERROR:  ${fatal_count} FATAL message(s) found in Anomaly report \"${anomaly_report}\""
                let exit_code=${exit_code}+1
            fi

            if [ ${error_count} -gt 0 ]; then
                echo "    ERROR:  ${error_count} ERROR message(s) found in Anomaly report \"${anomaly_report}\""
                let exit_code=${exit_code}+1
            fi

            if [ ${warning_count} -ge ${max_warnings} ]; then
                echo "    ERROR:  ${warning_count} WARNING message(s) found in Anomaly report \"${anomaly_report}\""
                let exit_code=${exit_code}+1
            fi

            if [ ${missing_count} -ge ${max_missing} ]; then
                echo "    ERROR:  ${missing_count} MISSING messages found in Anomaly report \"${anomaly_report}\""
                let exit_code=${exit_code}+1
            fi

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                err_msg="Problems detected in Anomaly report \"${anomaly_report}\""
            fi
        
        else
            err_msg="Anomalie report file \"${anomaly_report}\" is either missing or contains no data"
            exit_code=${ERROR}
        fi

    else
        err_msg="Could not locate reporting directory \"${report_dir}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Look for items labeled as MISSING in the Cobol-Copy report
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "MISSING"
    cobol_copy_report="${report_dir}/report-${ucProjectName}-Cobol-Copy"

    if [ -e "${cobol_copy_report}" -a -s "${cobol_copy_report}" ]; then
        echo -ne "    INFO:  Checking Cobol-Copy report for issues of concern ... "
        let cobol_copy_missing_count=`egrep -c ";MISSING;" "${cobol_copy_report}"`

        if [ ${cobol_copy_missing_count} -ge ${max_missing} ]; then
            echo "issues found [FAILED]"
            echo "    ERROR:  ${missing_count} MISSING messages found in Cobol-Copy report \"${cobol_copy_report}\""
            let exit_code=${exit_code}+1
        else
            echo "none found [SUCCESS]"
        fi

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            err_msg="Problems detected in Cobol-Copy report \"${cobol_copy_report}\""
        fi

    else
        err_msg="Cobol-Copy report file \"${cobol_copy_report}\" is either missing or contains no data"
        exit_code=${ERROR}
    fi

fi

# WHAT: Perform FileConvert operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "FILECONVERT"
    processing_verb="FileConvert"
    this_makefile="${source_dir}/makefile.${processing_verb}"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"make -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed"
        else
            echo "SUCCESS"
        fi

    else
        err_msg="Could not locate makefile \"${this_makefile}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Perform RdbmsConvert operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "RDBMSCONVERT"
    processing_verb="RdbmsConvert"
    this_makefile="${source_dir}/makefile.${processing_verb}"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"make -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
        #cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} 
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed"
        else
            echo "SUCCESS"
        fi

    else
        err_msg="Could not locate makefile \"${this_makefile}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Perform cobolConvert operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "COBOLCONVERT"
    processing_verb="cobolConvert"
    this_makefile="${source_dir}/makefile.${processing_verb}"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"make -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
        #cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} 
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed"
        else
            echo "SUCCESS"
        fi

    else
        err_msg="Could not locate makefile \"${this_makefile}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Perform trad_jcl operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "TRAD_JCL"
    processing_verb="trad_jcl"
    this_makefile="${source_dir}/makefile.${processing_verb}"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"make -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
        #cd "${source_dir}" && make -f "${this_makefile}" ${processing_verb} 
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed"
        else
            echo "SUCCESS"
        fi

    else
        err_msg="Could not locate makefile \"${this_makefile}\""
        exit_code=${ERROR}
    fi

fi

#====
# Post Conversion
#====
# WHAT: Perform post-conversion character munging
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "POST-PROCESSING"

    # Look for any regex files located in <folder>
    # where filename indicates the prepared folder in which to operate
    pcTarget_dir="${WB_AUTOMATE}/target"
    postconvert_dir="${WB_AUTOMATE}/param/regex/post_conversion"
    export postconvert_dir

    for target in ${TARGETS} ; do
        eval "file_ext=\$${target}_ext"
        uc_target=`echo "${target}" | tr '[a-z]' '[A-Z]'`

        # awk '{print $0}' BATCH | egrep "^\(\"" | sed -e 's/[()]//g' -e 's/\"//g' -e 's/\ \.\ /::/g'
        # Read in regex lines from "${postconvert_dir}/${uc_target}"
        if [ -e "${postconvert_dir}/${uc_target}" -a -s "${postconvert_dir}/${uc_target}" ]; then
            regex_lines=`strings "${postconvert_dir}/${uc_target}" | awk '{print $0}' | egrep -v "^#" | egrep "^\(\"" | sed -e 's/[()]//g' -e 's/\"//g' -e 's/\ \.\ /::/g' -e 's/\ /:ZZqC:/g'`

            # Use the regexes to replace file contents in ${pcTarget_dir} targets
            for regex_line in ${regex_lines} ; do

                if [ "${regex_line}" != "" ]; then
                    real_regex_line=`echo "${regex_line}" | sed -e 's/:ZZqC:/\ /g'`
                    left_side=`echo "${real_regex_line}" | awk -F'::' '{print $1}'`
                    right_side=`echo "${real_regex_line}" | awk -F'::' '{print $NF}'`

                    # Here we slurp in each file in the target directory and plow through each
                    # line skipping comment lines
                    source_code_dir="${pcTarget_dir}/${uc_target}"

                    case ${target} in 

                        sysin)
                            source_code_dir="${pcTarget_dir}/Master-${uc_target}/${uc_target}"
                        ;;

                        copy)
                            source_code_dir="${pcTarget_dir}/Master-${target}/${uc_target}"
                        ;;

                    esac

                    case ${target} in

                        batch|cics)
                            target_files=`cd "${source_code_dir}" 2> /dev/null && ls *.${file_ext} 2> /dev/null ; ls *.pco 2> /dev/null`
                        ;;

                        jcl)
                            file_ext="ksh"
                            target_files=`cd "${source_code_dir}" 2> /dev/null && ls *.{file_ext} 2> /dev/null`
                        ;;

                        *)
                            target_files=`cd "${source_code_dir}" 2> /dev/null && ls *.${file_ext} 2> /dev/null`
                        ;;

                    esac

                    tmp_dir="/tmp/${USER}/$$"

                    if [ ! -d "${tmp_dir}" ]; then
                        mkdir -p "${tmp_dir}"
                    fi

                    for target_file in ${target_files} ; do
                        echo -ne "    INFO:  Post-Processing \"${source_code_dir}/${target_file}\" for regular expression translation ... "
                        tmp_file="${tmp_dir}/translate-post-processing-${uc_target}-${target_file}.$$"
                        rm -f "${tmp_file}"

                        ${SCRIPT_BASE}/processing.pl --input_file "${source_code_dir}/${target_file}" --output_file "${tmp_file}" --type "${target}" --src_regex "${left_side}" --dst_regex "${right_side}"
                        #stdbuf -oL awk '{print $0}' "${source_code_dir}/${target_file}" | while IFS='' read -r input_line ; do
                        #    let is_comment=0
                        #    #echo "input line is: ${input_line}"

                        #    # Comments look like:
                        #    # - BATCH, COPY, and CICS: byte 7 == "*"
                        #    # - JCL PROCS: ^//*

                        #    case ${target} in

                        #        batch|copy|cics)
                        #            is_comment=`echo "${input_line}" | cut -b7 | egrep -c "\*"`
                        #            comment_prefix="      *"
                        #        ;;

                        #        *)
                        #            is_comment=`echo "${input_line}" | egrep -c "^#"`
                        #            comment_prefix="#"
                        #        ;;

                        #    esac

                        #    #echo "IS COMMENT: ${is_comment}"

                        #    if [ "${input_line}" = "" ]; then
                        #        echo -ne "${input_line}\n" >> ${tmp_file}
                        #    else

                        #        if [ ${is_comment} -gt 0 ]; then
                        #            #echo "* * * * FOUND COMMENT LINE * * * *"
                        #            echo -ne "${input_line}\n" >> ${tmp_file}
                        #        else
                        #            #echo "* * * * FOUND REGULAR LINE * * * *"

                        #            if [ "${left_side}" = "" ]; then
                        #                echo -ne "${input_line}\n" >> ${tmp_file}
                        #            else
                        #                right_now=`date`
                        #                original_line=`echo "${input_line}"`
                        #                echo -ne "${comment_prefix} The following line was commented out\n"      >> ${tmp_file}
                        #                echo -ne "${comment_prefix} ${right_now}\n"                              >> ${tmp_file}
                        #                echo -ne "${comment_prefix} by automation script ${0} POST-PROCESSING\n" >> ${tmp_file}
                        #                echo -ne "${comment_prefix} Original line was:\n"                        >> ${tmp_file}
                        #                echo -ne "${comment_prefix} ${original_line}\n"                          >> ${tmp_file}
                        #                new_input_line=`echo "${input_line}" | sed -e "s?${left_side}?${right_side}?g"`
                        #                echo -ne "${new_input_line}\n"                                           >> ${tmp_file}
                        #            fi

                        #        fi

                        #    fi

                        #done

                        # Move ${tmp_file} to ${source_code_dir}/${target_file}
                        if [ -e "${tmp_file}" -a -s "${tmp_file}" ]; then
                            rsync "${tmp_file}" "${source_code_dir}/${target_file}" > /dev/null 2>&1
                            echo "DONE"
                        else
                            echo "FAILED"
                        fi
                        
                    done

                fi

            done

        fi

    done
    
fi

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
