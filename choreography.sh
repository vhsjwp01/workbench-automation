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

# NAME: choreography.sh
# 
# This script performs a <something> with <things>
#
# OPTIONS:
#
# --wb_dir        - Work Bench base directory

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
#WB_AUTOMATE="/Users/jplumme/projects/wb_automate"
WB_AUTOMATE="/shared/wb_automate"
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
USAGE="${USAGE}[ --wb_dir <Path to Work Bench base directory> ]${USAGE_ENDLINE}"

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
    script_dirname=`dirname "${SCRIPT_NAME}"`
    SCRIPT_BASE=`cd "${script_dirname}" && pwd`
    
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

            # awk '{print $0}' BATCH | egrep "^\(\"" | sed -e 's/[()]//g' -e 's/\"//g' -e 's/\ \.\ /::/g'
            # Read in regex lines from "${preconvert_dir}/${uc_target}"
            if [ -e "${preconvert_dir}/${uc_target}" -a -s "${preconvert_dir}/${uc_target}" ]; then
                #echo "Running: ${SCRIPT_BASE}/processor.pl --input_file \"${prepare_dir}/${uc_target}/${target_file}\" --output_file \"${tmp_file}\" --data_type \"${target}\" --regex_file \"${preconvert_dir}/${uc_target}\" --mode \"pre\""
                ${SCRIPT_BASE}/processor.pl --input_file "${prepare_dir}/${uc_target}/${target_file}" --output_file "${tmp_file}" --data_type "${target}" --regex_file "${preconvert_dir}/${uc_target}" --mode "pre"
                        
                # Move ${tmp_file} to ${prepare_dir}/${uc_target}/${target_file}
                if [ -e "${tmp_file}" -a -s "${tmp_file}" ]; then
                    rsync "${tmp_file}" "${prepare_dir}/${uc_target}/${target_file}" > /dev/null 2>&1
                    let exit_code=${exit_code}+${?}
                else
                    let exit_code=${exit_code}+1
                fi

                if [ ${exit_code} -eq ${SUCCESS} ]; then
                    echo "SUCCESS"
                else
                    echo "FAILED"
                fi

            else
                echo "N/A"
            fi

        done

        # Extra pre-processing by type
        case ${target} in 

            copy|batch|cics)
                comment_prefix='      *'
                echo "    INFO:  Extra Pre-Processing ${prepare_dir}/${uc_target} files for ASIS translation:"
                target_files=`cd "${prepare_dir}/${uc_target}" 2> /dev/null && ls *.${file_ext} 2> /dev/null`
                
                for target_file in ${target_files} ; do
                    echo -ne "        Processing file ${prepare_dir}/${uc_target}/${target_file} for ASIS keyword munging ... "
                    # This egrep sequence should yield the proper triplet of info
                    valid_lines=($(egrep -n "EXEC CICS RECEIVE|ASIS|END-EXEC" "${prepare_dir}/${uc_target}/${target_file}" | egrep -A2 "EXEC CICS RECEIVE" | egrep "^[0-9]*:" | sed -e 's/\ /:ZZqC:/g'))
                    let line_counter=0
                
                    # Valid inputs occur in threes: a line with "EXEC CICS RECEIVE", followed by a line with "ASIS", followed by "END-EXEC"
                    while [ ${line_counter} -lt ${#valid_lines[@]} ]; do
                        exec_line_number=`echo "${valid_lines[$line_counter]}" | awk -F':' '{print $1}'`
                        real_exec_line=`echo "${valid_lines[$line_counter]}" | sed -e 's/:ZZqC:/\ /g' -e "s/^${exec_line_number}://g"`
                        let is_exec_line=`echo "${real_exec_line}" | egrep -c "EXEC CICS RECEIVE"`
                
                        let line_counter=${line_counter}+1
                
                        asis_line_number=`echo "${valid_lines[$line_counter]}" | awk -F':' '{print $1}'`
                        real_asis_line=`echo "${valid_lines[$line_counter]}" | sed -e 's/:ZZqC:/\ /g' -e "s/^${asis_line_number}://g"`
                        let is_asis_line=`echo "${real_asis_line}" | egrep -c "ASIS"`
                
                        let line_counter=${line_counter}+1
                
                        end_line_number=`echo "${valid_lines[$line_counter]}" | awk -F':' '{print $1}'`
                        real_end_line=`echo "${valid_lines[$line_counter]}" | sed -e 's/:ZZqC:/\ /g' -e "s/^${end_line_number}://g"`
                        let is_end_line=`echo "${real_end_line}" | egrep -c "END-EXEC"`
                
                        # If all three tests are equal to 1, then we found the properly ordered triplet
                        # And we need to comment out the line number containing ASIS
                        if [ ${is_exec_line} -eq 1 -a ${is_asis_line} -eq 1 -a ${is_end_line} -eq 1 ]; then
                            line_prefix=`echo "${real_asis_line}" | cut -b 1-7`
                            line_remainder=`echo "${real_asis_line}" | sed -e "s/^${line_prefix}//g"`
                            sed -i -e "${asis_line_number}s/^${real_asis_line}/${comment_prefix}${line_remainder}/g" "${prepare_dir}/${uc_target}/${target_file}"
                            #echo "sed -e \"${asis_line_number}s/^${real_asis_line}/${comment_prefix}${line_remainder}/g\" \"${prepare_dir}/${uc_target}/${target_file}\""
                        fi

                        let line_counter=${line_counter}+1
                    done

                    echo "DONE"
                done

                if [ "${target}" = "cics" ]; then
                    echo -ne "    INFO:  Extra Pre-Processing ${prepare_dir}/${uc_target} files for regular expression translation ... "
                    sed -i 's/                 15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./ptfix *          15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./ a\ptfix            15 CURSOR-ATTR-1     PIC X(4) VALUE X'\''00FFFF20'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 FILLER            PIC S9(9) COMP VALUE +16777160./ptfix *          15 FILLER            PIC S9(9) COMP VALUE +16777160./' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 FILLER            PIC S9(9) COMP VALUE +16777160./ a\ptfix            15 FILLER            PIC X(4) VALUE X'\''00FFFF48'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. /ptfix *          15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. / a\ptfix            15 OK-ATTR-1         PIC X(4) VALUE X'\''00000020'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. /ptfix *          15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. / a\ptfix            15 PROT-ATTR-1       PIC X(4) VALUE X'\''00000030'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. /ptfix *          15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. / a\ptfix            15 BLANK-ATTR-1      PIC X(4) VALUE X'\''00000025'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./ptfix *          15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./ a\ptfix            15 CBLANK-ATTR-1     PIC X(4) VALUE X'\''00FFFF3C'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. /ptfix *          15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. / a\ptfix            15 IBLANK-ATTR-1     PIC X(4) VALUE X'\''0000003C'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. /ptfix *          15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. / a\ptfix            15 OHIGH-ATTR-1      PIC X(4) VALUE X'\''00000038'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i 's/                 15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. /ptfix *          15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                    sed -i '/ptfix \*          15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. / a\ptfix            15 IHIGH-ATTR-1      PIC X(4) VALUE X'\''00000048'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}

                    if [ ${?} -eq ${SUCCESS} ]; then
                        echo "SUCCESS"
                    else
                        echo "FAILED"
                        err_msg="Extra PracTrans sed munging failed on ${prepare_dir}/${uc_target} Cobol files"
                        exit_code=${ERROR}
                    fi

                fi

            ;;

            jcl|proc)
                comment_prefix='//*'
                echo "    INFO:  Extra Pre-Processing ${prepare_dir}/${uc_target} files for SUBSYS and INCLUDE translation:"
                target_files=`cd "${prepare_dir}/${uc_target}" 2> /dev/null && ls *.${file_ext} 2> /dev/null`

                for target_file in ${target_files} ; do
                    #set +x

                    #=====
                    echo -ne "        Processing file ${prepare_dir}/${uc_target}/${target_file} for SUBSYS keyword munging ... "
                    this_line_count=`wc -l "${prepare_dir}/${uc_target}/${target_file}" | awk '{print $1}'`
                    subsys_lines=($(egrep -n "SUBSYS=" "${prepare_dir}/${uc_target}/${target_file}" | awk -F':' '{print $1}'))

                    for subsys_line in ${subsys_lines[*]} ; do
                        alt_label=""
                    
                        # Get the original label
                        let orig_label_line=${subsys_line}
                        orig_label=`egrep -n "SUBSYS=" "${prepare_dir}/${uc_target}/${target_file}" | egrep "^${subsys_line}:" | awk '{print $1}' | awk -F'/' '{print $NF}'`
                        alt_label=`egrep -A${this_line_count} "//${orig_label}.*SUBSYS=" "${prepare_dir}/${uc_target}/${target_file}" | egrep "DDNAME=" | head -1 | awk -F',' '{print $1}' | awk -F'=' '{print $NF}'`
                        let alt_label_line=`egrep -n "^//${alt_label}" "${prepare_dir}/${uc_target}/${target_file}" | awk -F':' '{print $1}'`
                    
                        # Munge the labels if we have enough pieces
                        if [ "${orig_label}" != "" -a "${alt_label}" != ""  ]; then
                            let label_line_counter=${orig_label_line}
                    
                            # Comment out all the lines between SUBSYS start and the line above "^//${alt_label}"
                            while [ ${label_line_counter} -lt ${alt_label_line}  ]; do
                                this_line=`egrep -n "^.*$" "${prepare_dir}/${uc_target}/${target_file}" | egrep "^${label_line_counter}:" | sed -e "s/^${label_line_counter}://g"`
                                first3_chars=`echo "${this_line}" | cut -b 1-3 | sed -e 's/\*/\\\*/g'`
                                line_remainder=`echo "${this_line}" | sed -e "s?^${first3_chars}??g" -e 's/\*/\\\*/g'`
                                sed -i -e "${label_line_counter}s?^${this_line}\$?${comment_prefix}${line_remainder}?g" "${prepare_dir}/${uc_target}/${target_file}"
                                let label_line_counter=${label_line_counter}+1
                            done
                    
                            # Fix the alt label line
                            sed -i -e "s?^//${alt_label}?//${orig_label}?g" "${prepare_dir}/${uc_target}/${target_file}"
                        else
                            echo -ne " Missing label(s): ORIG LABEL=${orig_label}, ALT_LABEL=${alt_label} ... "
                        fi
                    
                    done

                    echo "DONE"

                    #=====
                    #set -x

                    echo -ne "        Processing file ${prepare_dir}/${uc_target}/${target_file} for INCLUDE keyword munging ... "
                    target_lines=($(egrep -n "INCLUDE.*MEMBER=|//[^\*|\ ]" "${prepare_dir}/${uc_target}/${target_file}" | egrep -A1 "INCLUDE" | egrep "^[0-9]*:" | sed -e 's/\ /:ZZqC:/g'))
                    element_count=${#target_lines[@]}

                    # There should always be pairs of lines found, a start line and an end line
                    let line_counter=0
                
                    while [ ${line_counter} -lt ${element_count} ]; do
                
                        # Get the start line number
                        let start_line=`echo "${target_lines[$line_counter]}" | sed -e 's/:ZZqC:/\ /g' | awk -F':' '{print $1}'`
                
                        # Get the real line
                        real_start_line=`echo "${target_lines[$line_counter]}" | sed -e 's/:ZZqC:/\ /g' -e "s/^${start_line}://g"`
                
                        # Increment the counter to find the end_line
                        let line_counter=${line_counter}+1
                
                        # Get the end line number
                        let end_line=`echo "${target_lines[$line_counter]}" | sed -e 's/:ZZqC:/\ /g' | awk -F':' '{print $1}'`
                
                        # Subtract 1 from ${end_line} to avoid inclusivity
                        let end_line=${end_line}-1
                
                        # Figure out the integer for -A argument in egrep
                        let lines_after=${end_line}-${start_line}
                
                        # Get the actual lines to be munged
                        real_target_lines=($(egrep -A${lines_after} "^${real_start_line}$" "${prepare_dir}/${uc_target}/${target_file}" | sed -e 's/\ /:ZZqC:/g'))
                
                        # Munge the lines in question
                        for real_target_line in ${real_target_lines[*]} ; do
                            real_target_line=`echo "${real_target_line}" | sed -e 's/:ZZqC:/\ /g'`
                            first3_chars=`echo "${real_target_line}" | cut -b 1-3 | sed -e 's/\*/\\\*/g'`
                            line_remainder=`echo "${real_target_line}" | sed -e "s?^${first3_chars}??g"`
                            #echo "sed -i -e \"s:^${real_target_line}\$:${comment_prefix}${line_remainder}:g\" \"${prepare_dir}/${uc_target}/${target_file}\"" 

                            if [ "${real_target_line}" != "${comment_prefix}${line_remainder}" ]; then
                                eval "sed -i -e 's?^${real_target_line}\$?${comment_prefix}${line_remainder}?g' \"${prepare_dir}/${uc_target}/${target_file}\""
                            fi

                        done
                
                        # Munge the start line
                        first3_chars=`echo "${real_start_line}" | cut -b 1-3 | sed -e 's/\*/\\\*/g'`
                        line_remainder=`echo "${real_start_line}" | sed -e "s?^${first3_chars}??g"`
                        #echo "sed -i -e \"s:^${real_start_line}\$:${comment_prefix}${line_remainder}:g\" \"${prepare_dir}/${uc_target}/${target_file}\""
                        eval "sed -i -e 's?^${real_start_line}\$?${comment_prefix}${line_remainder}?g' \"${prepare_dir}/${uc_target}/${target_file}\""
                
                        # Increment line_counter to start next pair
                        let line_counter=${line_counter}+1
                    done
                
                    echo "DONE"
                done

            ;;

        esac
 
    done
    
fi

###
#exit
###

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

        #if [ -e "${anomaly_report}" -a -s "${anomaly_report}" ]; then
        if [ -e "${anomaly_report}" ]; then
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

    #if [ -e "${cobol_copy_report}" -a -s "${cobol_copy_report}" ]; then
    if [ -e "${cobol_copy_report}" ]; then
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

                ${SCRIPT_BASE}/processor.pl --input_file "${source_code_dir}/${target_file}" --output_file "${tmp_file}" --data_type "${target}" --regex_file "${postconvert_dir}/${uc_target}" --mode "post"

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
