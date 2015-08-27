#!/bin/bash
#set -x

################################################################################
#                      S C R I P T    D E F I N I T I O N
################################################################################
#

#-------------------------------------------------------------------------------
# Revision History
#-------------------------------------------------------------------------------
# 20150708     Jason W. Plummer          Original: A script to automate Main
#                                        Frame rehost code conversion
# 20150709     Jason W. Plummer          Added raw log output from eclipse, 
#                                        courtesy of Paul Gauthier, 20150707.
#                                        Fist iteration to transmogrify 
#                                        ORIG/convert_5.log into an operable 
#                                        shell script 
# 20150715     Jason W. Plummer          Acquired command line choreography 
#                                        ART WB readme file
# 20150729     Jason W. Plummer          Refactored choreography script into
#                                        this format
# 20150805     Jason W. Plummer          Got end to end working (sans pre and 
#                                        post conversion).  Added comment 
#                                        capability in pre and post processing
# 20150811     Jason W. Plummer          Created mating PERL script for pattern 
#                                        munging.  Completed PERL script testing
#                                        of functionality.  Added shell out to 
#                                        PERL script for pattern munging
# 20150812     Jason W. Plummer          Started code beautification
# 20150818     Jason W. Plummer          Got INCLUDE munging working for JCL and
#                                        PROC files
# 20150819     Jason W. Plummer          Successful run, sans post-conversion 
#                                        rules ... Mommy I'm scared
# 20150820     Jason W. Plummer          Added FTP put and get munging
# 20150823     Jason W. Plummer          Added POST processing of SMTP macros 
#                                        and COBOL SQL timestamp munging
# 20150824     Jason W. Plummer          Code Cleanup.  Added smarter comment 
#                                        logic.  Fixed comparison syntax error.
#                                        Fixed wrong comment position character 
#                                        for Cobol programs.  Fixed Cobol 
#                                        comment funtionality.
# 20150825     Jason W. Plummer          Added command line argument support.
#                                        Added feedback during check_command.
#                                        Added code to replace '.' with '_' for
#                                        mapping folder name => variable name.
#                                        Code fixes to deal with sibc_cardliba
# 20150826     Jason W. Plummer          Added support to turn on what source
#                                        directories contain data in system.desc
#                                        Added support to modify version.mk
#                                        based on targets present
# 20150827     Jason W. Plummer          Created CONFIG section for modifying
#                                        system.desc and version.mk

################################################################################
# DESCRIPTION
################################################################################
#

# NAME: choreography.sh
# 
# This script performs an automated ART WorkBench project code conversion
#
# OPTIONS:
#
# --wb_workdir            - Work Bench base directory (REQUIRED)
# --project_name          - The Project Name to use in reporting (OPTIONAL)
# --input_targets         - A list of directories from which to draw input 
#                           (OPTIONAL)
# --wb_toolpath           - The ART Work Bench tool path (Default: ART 13)
# --target_COBOL_compiler - The COBOL compiler to use    (Default: COBOL-IT)
# --rdbms_schemas         - A comma separated list of RDBMS schemas
# --file_schemas          - A comma separated list of file schemas
# --target_DB             - Target DataBase to use       (Default: ORACLE)

################################################################################
# CONSTANTS
################################################################################
#

PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin
TERM=vt100
export TERM PATH

SUCCESS=0
ERROR=1

STDOUT_OFFSET="    "

SCRIPT_NAME="${0}"

USAGE_ENDLINE="\n${STDOUT_OFFSET}${STDOUT_OFFSET}${STDOUT_OFFSET}${STDOUT_OFFSET}"
USAGE="${SCRIPT_NAME}${USAGE_ENDLINE}"
USAGE="${USAGE}[ --wb_workdir            <Path to Work Bench base directory              *REQUIRED*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --file_schemas          <A comma separated list of file schemas         *REQUIRED*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --project_name          <The Project Name to use in reporting           *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --input_targets         <A list of directories from which to draw input *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --wb_toolpath           <The ART Work Bench tool path (Default: ART 13) *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --target_COBOL_compiler <The COBOL compiler to use (Default: COBOL-IT)  *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --rdbms_schemas         <A comma separated list of RDBMS schemas        *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --target_DB             <Target DataBase to use (Default: ORACLE)       *OPTIONAL*> ]"

################################################################################
# VARIABLES
################################################################################
#

err_msg=""
exit_code=${SUCCESS}

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

    for command in awk cp cut diff dirname egrep find ls make mkdir pwd rm rsync sed sort tr uname wc ; do
        unalias ${command} > /dev/null 2>&1
        f__check_command "${command}"

        if [ ${?} -ne ${SUCCESS} ]; then
            let exit_code=${exit_code}+1

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "    ERROR:  Could not locate command \"${command}\""
            fi

        fi

    done

fi

# WHAT: Make sure we have a WB_AUTOMATE directory in which to operate
# WHY:  Cannot continue otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    while (( "${#}"  )); do
        key=`echo "${1}" | ${my_sed} -e 's?\`??g'`
        value=`echo "${2}" | ${my_sed} -e 's?\`??g'`

        case "${key}" in

            --wb_workdir|--project_name|--input_targets|--wb_toolpath|--target_COBOL_compiler|--target_DB)
                key=`echo "${key}" | ${my_sed} -e 's?^--??g'`

                if [ "${value}" != ""  ]; then
                    eval ${key}="${value}"
                    shift
                    shift
                else
                    echo "${STDOUT_OFFSET}ERROR:  No value assignment can be made for command line argument \"--${key}\""
                    exit_code=${ERROR}
                    shift
                fi

            ;;

            --rdbms_schemas|--file_schemas)
                key=`echo "${key}" | ${my_sed} -e 's?^--??g'`

                if [ "${value}" != ""  ]; then
                    value=`echo "${value}" | ${my_sed} 's/,/\ /g'`
                    eval ${key}="${value}"
                    shift
                    shift
                else
                    echo "${STDOUT_OFFSET}ERROR:  No value assignment can be made for command line argument \"--${key}\""
                    exit_code=${ERROR}
                    shift
                fi

            ;;

            *)
                # We bail immediately on unknown or malformed inputs
                echo "${STDOUT_OFFSET}ERROR:  Unknown command line argument ... exiting"
                exit
            ;;

        esac

    done

    if [ "${wb_workdir}" = "" ]; then
        err_msg="No workbench work directory provided"
        exit_code=${ERROR}
    else

        if [ ! -d "${wb_workdir}" ]; then
            err_msg="Could not locate workbench work directory: \"${wb_workdir}\""
            exit_code=${ERROR}
        fi

    fi

    if [ "${wb_toolpath}" != "" ]; then

        if [ ! -d "${wb_toolpath}" ]; then
            err_msg="Could not locate workbench tool path: \"${wb_toolpath}\""
            exit_code=${ERROR}
        fi

    fi

    if [ "${file_schemas}" = "" ]; then
        err_msg="No file schema was provided.  Please set file schema with --file_schemas \"<schema name>\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Set some distro specific vars
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    script_dirname=`${my_dirname} "${SCRIPT_NAME}"`
    SCRIPT_BASE=`cd "${script_dirname}" && ${my_pwd}`
    
    os_type=`${my_uname} -s | ${my_tr} '[A-Z]' '[a-z]'`
    cpu_arch=`${my_uname} -m | ${my_tr} '[A-Z]' '[a-z]' | ${my_sed} -e 's/i[345]86/i686/g'`

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

        # Set the newline variable (needed for sed operations later on)
        NL='
        '

        # Known file extensions (lower case)
        sysin_ext="sysin"
        cics_ext="cbl"
        proc_ext="proc"
        jcl_ext="jcl"
        copy_ext="cpy"
        map_ext="bms"
        ddl_ext="sql"
        batch_ext="cbl"
        sibc_cardliba_ext="sysin"

        # Known file extensions (upper case)
        uc_sibc_cardliba_ext=`echo "${sibc_cardliba_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_sysin_ext=`echo "${sysin_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_cics_ext=`echo "${cics_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_proc_ext=`echo "${proc_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_jcl_ext=`echo "${jcl_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_copy_ext=`echo "${copy_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_map_ext=`echo "${map_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_ddl_ext=`echo "${ddl_ext}" | ${my_tr} '[a-z]' '[A-Z]'`
        uc_batch_ext=`echo "${batch_ext}" | ${my_tr} '[a-z]' '[A-Z]'`

        # This directory houses eclipse/ART WB harvested script assets
        WB_AUTOMATE="${wb_workdir}"

        # Set the default project name
        ProjectName="AUTOMATE"

        # Override ProjectName is ${project_name} has been set
        if [ "${project_name}" != "" ]; then
            ProjectName="${project_name}"
        fi

        ucProjectName=`echo "${ProjectName}" | ${my_tr} '[a-z]' '[A-Z]'`

        # Set the default Work Bench tool path
        WorkbenchPath="/opt/tuxedo/art_wb12.1.3.0.0"

        # Override WorkbenchPath is ${wb_toolpath} has been set
        if [ "${wb_toolpath}" != "" ]; then
            WorkbenchPath="${wb_toolpath}"
        fi
        
        PROJECT="${WB_AUTOMATE}"
        TRAVAIL="${PROJECT}/Logs"
        LOGS="${TRAVAIL}"
        TMPPROJECT="${PROJECT}/tmp"
        LocationOfAssets="${WB_AUTOMATE}/input"
        PARAM="${WB_AUTOMATE}/param"
        
        WBLOGLEVEL="INFO"
        WBEXITONERROR="YES"
        OnlyParsingPhase=""

        # Set default TargetCOBOLCompiler
        TargetCOBOLCompiler="COBOL-IT"

        # Override TargetCOBOLCompiler is ${target_COBOL_compiler} has been set
        if [ "${target_COBOL_compiler}" != "" ]; then
            TargetCOBOLCompiler="${target_COBOL_compiler}"
        fi

        # Set default TargetDataBase
        TargetDataBase="ORACLE"

        # Override TargetDataBase is ${target_DB} has been set
        if [ "${target_DB}" != "" ]; then
            TargetDataBase="${target_DB}"
        fi

        DB_TYPE="${TargetDataBase}"

        ParallelNum=1
        
        ksh_offset="       "

        # Default targets
        TARGETS=`cd "${LocationOfAssets}" 2> /dev/null && ${my_ls} -d * 2> /dev/null | ${my_tr} '[A-Z]' '[a-z]'`

        # Override TARGETS if ${input_targets} has been set
        if [ "${input_targets}" != "" ]; then
            TARGETS="${input_targets}"
        fi

        export PROJECT TRAVAIL LOGS TMPPROJECT WorkbenchPath LocationOfAssets PARAM REFINEDISTRIB DB_TYPE
    fi

fi

# WHAT: Perform config operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "CONFIG"
    processing_verb="config"
    param_dir="${PARAM}"
    script_dir="${WB_AUTOMATE}/scripts"

    if [ -d "${param_dir}" -a -d "${script_dir}" ]; then
        echo "    INFO:  Initializing ${processing_verb} resources in directory \"${param_dir}\""

        # Create ${script_dir}/project.txt
        echo "ProjectName=${ProjectName}"                  > "${script_dir}/project.txt"
        echo "ParallelNum=${ParallelNum}"                 >> "${script_dir}/project.txt"
        echo "WorkbenchPath=${WorkbenchPath}"             >> "${script_dir}/project.txt"
        echo "LocationOfAssets=${LocationOfAssets}"       >> "${script_dir}/project.txt"
        echo "WBLOGLEVEL=${WBLOGLEVEL}"                   >> "${script_dir}/project.txt"
        echo "WBEXITONERROR=${WBEXITONERROR}"             >> "${script_dir}/project.txt"
        echo "OnlyParsingPhase=${OnlyParsingPhase}"       >> "${script_dir}/project.txt"
        echo "TargetCOBOLCompiler=${TargetCOBOLCompiler}" >> "${script_dir}/project.txt"
        echo "TargetDataBase=${TargetDataBase}"           >> "${script_dir}/project.txt"

        # Create ${param_dir}/system.desc from template
        if [ -e "${param_dir}/system.desc.template" ]; then

            # Reconstruct "${param_dir}/system.desc"
            if [ -e "${param_dir}/system.desc" ]; then
                ${my_rm} -f "${param_dir}/system.desc"
            fi

            ${my_cp} -p "${param_dir}/system.desc.template" "${param_dir}/system.desc"
            ${my_sed} -i -e "s?::PROJECT_NAME::?${ProjectName}?g" -e "s?::SOURCE_DIR::?${source_dir}?g" "${param_dir}/system.desc"

            # Enable targets based on whether or files exist in those targets
            for target_dir in ${TARGETS} ; do
                uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`
                target_dir_var=`echo "${target_dir}" | ${my_sed} -e 's/\./_/g'`
                eval "file_ext=\$${target_dir_var}_ext"

                file_count=`${my_find} ${source_dir}/${uc_target_dir}/*.${file_ext} -maxdepth 1 2> /dev/null | ${my_wc} -l | ${my_awk} '{print $1}'`

                if [ ${file_count} -gt 0 ]; then
                    let is_commented_out=`${my_egrep} -c "^%directory \"${uc_target_dir}\" type" "${param_dir}/system.desc"`

                    if [ ${is_commented_out} -gt 0 ]; then
                        begin_text="% BEGIN: ${uc_target_dir}-DIRECTORY-TARGETS"
                        end_text="% END: ${uc_target_dir}-DIRECTORY-TARGETS"
                        ${my_sed} -i "/${begin_text}/,/${end_text}/{/${begin_text}/n;/${end_text}/!{s/^%\(.*\)$/\1/g}}" "${param_dir}/system.desc"
                    fi

                    # Clear out begin and end text vars so that library dependencies can be turned on 
                    begin_text=""
                    end_text=""

                    case ${target_dir} in

                        batch|cics)
                            # Must also include COPY files with BATCH and CICS targets
                            begin_text="% BEGIN: COPY-DIRECTORY-TARGETS"
                            end_text="% END: COPY-DIRECTORY-TARGETS"

                            # Now set values in {param_dir}/version.mk
                            case ${target_dir} in 

                                batch)
                                    ${my_sed} -i -e 's/^Find_Prg =.*$/Find_Prg = BATCH/g' "${param_dir}/version.mk"
                                ;;

                                cics)
                                    ${my_sed} -i -e 's/^Find_Tpr =.*$/Find_Tpr = CICS/g' "${param_dir}/version.mk"
                                ;;

                            esac

                        ;;

                        jcl)
                            # Must also include PROC files with JCL target
                            begin_text="% BEGIN: PROC-DIRECTORY-TARGETS"
                            end_text="% END: PROC-DIRECTORY-TARGETS"

                            # Now set values in {param_dir}/version.mk
                            ${my_sed} -i -e 's/^Find_Jcl =.*$/Find_Jcl = JCL/g' "${param_dir}/version.mk"
                        ;;

                        map)
                            # Now set values in {param_dir}/version.mk
                            ${my_sed} -i -e 's/^Find_Map =.*$/Find_Map = MAP/g' "${param_dir}/version.mk"
                        ;;

                    esac

                    # Turn these descriptions on in the ${param_dir}/system.desc file
                    if [ "${begin_text}" != "" -a "${end_text}" != "" ]; then
                        ${my_sed} -i "/${begin_text}/,/${end_text}/{/${begin_text}/n;/${end_text}/!{s/^%\(.*\)$/\1/g}}" "${param_dir}/system.desc"
                    fi

                fi

            done

            if [ -e "${param_dir}/version.mk" ]; then
                # Enable RDBMS and file schemas if set
                ${my_sed} -i -e 's/^#\(RDBMS_SCHEMAS =\).*$/\1/g' "${param_dir}/version.mk"
                ${my_sed} -i -e "s/^\(FILE_SCHEMAS =\).*\$/\1 ${file_schemas}/g" "${param_dir}/version.mk"
                
                if [ "${rdbms_schemas}" != "" ]; then
                    ${my_sed} -i -e "s/^\(RDBMS_SCHEMAS =\).*\$/\1 ${rdbms_schemas}/g" "${param_dir}/version.mk"
                else
                    ${my_sed} -i -e 's/^\(RDBMS_SCHEMAS =.*$\)/#\1/g' "${param_dir}/version.mk"
                fi

            else
                err_msg="Could not locate \"${param_dir}/version.mk\""
                exit_code=${ERROR}
            fi

        else
            err_msg="Could not locate system description template file \"${param_dir}/system.desc.template\""
            exit_code=${ERROR}
        fi

    else
        err_msg="Could not locate both parameter file directory \"${PARAM}\" and script directory \"${script_dir}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Import data
# WHY:  Asked to
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "IMPORT"
    processing_verb="import"
    umask 007
    input_dir="${WB_AUTOMATE}/input"
    import_dir="${WB_AUTOMATE}/imported"
    prepare_dir="${WB_AUTOMATE}/prepared"
    export import_dir prepare_dir

    if [ -d "${script_dir}" -a -d "${input_dir}" -a -d "${import_dir}" ]; then

        # Setup item lists for import
        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`
            target_dir_var=`echo "${target_dir}" | ${my_sed} -e 's/\./_/g'`
            eval "file_ext=\$${target_dir_var}_ext"

            if [ -d "${input_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir_var}_list=\"`cd ${input_dir}/${uc_target_dir} 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null`\""
            else
                echo "    WARNING:  Directory \"${input_dir}/${uc_target_dir}\" does not exist"
            fi

        done

        for list_name in ${TARGETS} ; do
            list_name_var=`echo "${list_name}" | ${my_sed} -e 's/\./_/g'`
            clean_list="${list_name_var}_list"
            eval "${clean_list}=\"\""
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name_var}_list\\""` ; do
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`
            echo "    INFO:  Refreshing ${processing_verb} directory \"${import_dir}/${uc_target_dir}\""
            ${my_rm} -rf "${import_dir}/${uc_target_dir}"
            ${my_mkdir} -p "${import_dir}/${uc_target_dir}"
        done

        this_makefile="${script_dir}/makefile.${processing_verb}"

        # Try importing
        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} all\" ... "
            cd "${script_dir}" && ${my_make} -f "${this_makefile}" all > /dev/null 2>&1
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
            uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`
            target_dir_var=`echo "${target_dir}" | ${my_sed} -e 's/\./_/g'`
            eval "file_ext=\$${target_dir_var}_ext"

            if [ -d "${import_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir_var}_list=\"`cd ${import_dir}/${uc_target_dir} 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null`\""
            else
                echo "    WARNING:  Directory \"${import_dir}/${uc_target_dir}\" does not exist"
            fi
    
        done

        # Try preparing
        for list_name in ${TARGETS} ; do
            list_name_var=`echo "${list_name}" | ${my_sed} -e 's/\./_/g'`
            clean_list="${list_name_var}_list"
            eval "${clean_list}=\"\""
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name_var}_list\\""` ; do
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`
            echo "    INFO:  Refreshing ${processing_verb} directory \"${prepare_dir}/${uc_target_dir}\""
            ${my_rm} -rf "${prepare_dir}/${uc_target_dir}"
            ${my_mkdir} -p "${prepare_dir}/${uc_target_dir}"
        done

        this_makefile="${script_dir}/makefile.${processing_verb}"

        # Explcitly declare the uppercase filenames (with lowercase extensions) file lists
        export uc_copy_list=`echo "${copy_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_copy_ext}/\.${copy_ext}/g"`
        export uc_sysin_list=`echo "${sysin_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_sysin_ext}/\.${sysin_ext}/g"`
        export uc_sibc_cardliba_list=`echo "${sibc_cardliba_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_sibc_cardliba_ext}/\.${sibc_cardliba_ext}/g"`
        export uc_batch_list=`echo "${batch_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_batch_ext}/\.${batch_ext}/g"`
        export uc_cics_list=`echo "${cics_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_cics_ext}/\.${cics_ext}/g"`
        export uc_ddl_list=`echo "${ddl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_ddl_ext}/\.${ddl_ext}/g"`
        export uc_map_list=`echo "${map_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_map_ext}/\.${map_ext}/g"`
        export uc_jcl_list=`echo "${jcl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_jcl_ext}/\.${jcl_ext}/g"`
        export uc_proc_list=`echo "${proc_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_proc_ext}/\.${proc_ext}/g"`

        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} all\" ... "
            cd "${script_dir}" && ${my_make} -f "${this_makefile}" all > /dev/null 2>&1
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
        uc_target=`echo "${target}" | ${my_tr} '[a-z]' '[A-Z]'`

        # Here we slurp in each file in the target directory and plow through each
        # line skipping comment lines
        target_files=`cd "${prepare_dir}/${uc_target}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null`
        tmp_dir="/tmp/${USER}/$$"

        if [ ! -d "${tmp_dir}" ]; then
            ${my_mkdir} -p "${tmp_dir}"
        fi

        for target_file in ${target_files} ; do
            echo -ne "    INFO:  Pre-Processing \"${prepare_dir}/${uc_target}/${target_file}\" for regular expression translation ... "
            tmp_file="${tmp_dir}/translate-pre-processing-${uc_target}-${target_file}.$$"
            ${my_rm} -f "${tmp_file}"

            # Read in regex lines from "${preconvert_dir}/${uc_target}"
            if [ -e "${preconvert_dir}/${uc_target}" -a -s "${preconvert_dir}/${uc_target}" ]; then
                ${SCRIPT_BASE}/processor.pl --input_file "${prepare_dir}/${uc_target}/${target_file}" --output_file "${tmp_file}" --data_type "${target}" --regex_file "${preconvert_dir}/${uc_target}" --mode "pre"
                        
                # Move ${tmp_file} to ${prepare_dir}/${uc_target}/${target_file}
                if [ -e "${tmp_file}" -a -s "${tmp_file}" ]; then
                    ${my_diff} -q "${tmp_file}" "${prepare_dir}/${uc_target}/${target_file}" > /dev/null 2>&1

                    if [ ${?} -ne ${SUCCESS} ]; then
                        ${my_rsync} "${tmp_file}" "${prepare_dir}/${uc_target}/${target_file}" > /dev/null 2>&1
                        let exit_code=${exit_code}+${?}
                    fi

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
                target_files=`cd "${prepare_dir}/${uc_target}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null`
                
                for target_file in ${target_files} ; do
                    echo -ne "        Processing file ${prepare_dir}/${uc_target}/${target_file} for ASIS keyword munging ... "
                    # This command sequence should yield the proper triplet of info
                    valid_lines=($(${my_egrep} -n "EXEC CICS RECEIVE|ASIS|END-EXEC" "${prepare_dir}/${uc_target}/${target_file}" | ${my_egrep} -A2 "EXEC CICS RECEIVE" | ${my_egrep} "^[0-9]*:" | ${my_sed} -e 's/\ /:ZZqC:/g'))
                    let line_counter=0
                
                    # Valid inputs occur in threes: a line with "EXEC CICS RECEIVE", followed by a line with "ASIS", followed by "END-EXEC"
                    while [ ${line_counter} -lt ${#valid_lines[@]} ]; do
                        exec_line_number=`echo "${valid_lines[$line_counter]}" | ${my_awk} -F':' '{print $1}'`
                        real_exec_line=`echo "${valid_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${exec_line_number}://g"`
                        let is_exec_line=`echo "${real_exec_line}" | ${my_egrep} -c "EXEC CICS RECEIVE"`
                
                        let line_counter=${line_counter}+1
                
                        asis_line_number=`echo "${valid_lines[$line_counter]}" | ${my_awk} -F':' '{print $1}'`
                        real_asis_line=`echo "${valid_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${asis_line_number}://g"`
                        let is_asis_line=`echo "${real_asis_line}" | ${my_egrep} -c "ASIS"`
                
                        let line_counter=${line_counter}+1
                
                        end_line_number=`echo "${valid_lines[$line_counter]}" | ${my_awk} -F':' '{print $1}'`
                        real_end_line=`echo "${valid_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${end_line_number}://g"`
                        let is_end_line=`echo "${real_end_line}" | ${my_egrep} -c "END-EXEC"`
                
                        # If all three tests are equal to 1, then we found the properly ordered triplet
                        # And we need to comment out the line number containing ASIS
                        if [ ${is_exec_line} -eq 1 -a ${is_asis_line} -eq 1 -a ${is_end_line} -eq 1 ]; then
                            line_prefix=`echo "${real_asis_line}" | ${my_cut} -b 1-7`
                            line_remainder=`echo "${real_asis_line}" | ${my_sed} -e "s/^${line_prefix}//g"`
                            ${my_sed} -i -e "${asis_line_number}s/^${real_asis_line}/${comment_prefix}${line_remainder}/g" "${prepare_dir}/${uc_target}/${target_file}"
                        fi

                        let line_counter=${line_counter}+1
                    done

                    echo "DONE"
                done

                if [ "${target}" = "cics" ]; then
                    echo -ne "    INFO:  Extra Pre-Processing ${prepare_dir}/${uc_target} files for regular expression translation ... "

                    files_to_touch=`${my_egrep} -H "15 CURSOR-ATTR-1     PIC S9\(9\) COMP VALUE \+16777152\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./ptfix *          15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./ a\ptfix            15 CURSOR-ATTR-1     PIC X(4) VALUE X'\''00FFFF20'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "15 FILLER            PIC S9\(9\) COMP VALUE \+16777160\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 FILLER            PIC S9(9) COMP VALUE +16777160./ptfix *          15 FILLER            PIC S9(9) COMP VALUE +16777160./' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 FILLER            PIC S9(9) COMP VALUE +16777160./ a\ptfix            15 FILLER            PIC X(4) VALUE X'\''00FFFF48'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "OK-ATTR-1         PIC S9\(9\) COMP VALUE \+4210880\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. /ptfix *          15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. / a\ptfix            15 OK-ATTR-1         PIC X(4) VALUE X'\''00000020'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "15 PROT-ATTR-1       PIC S9\(9\) COMP VALUE \+4210928\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. /ptfix *          15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. / a\ptfix            15 PROT-ATTR-1       PIC X(4) VALUE X'\''00000030'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "15 BLANK-ATTR-1      PIC S9\(9\) COMP VALUE \+4210940\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. /ptfix *          15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. / a\ptfix            15 BLANK-ATTR-1      PIC X(4) VALUE X'\''00000025'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "15 CBLANK-ATTR-1     PIC S9\(9\) COMP VALUE \+16777164\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./ptfix *          15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./ a\ptfix            15 CBLANK-ATTR-1     PIC X(4) VALUE X'\''00FFFF3C'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "15 IBLANK-ATTR-1     PIC S9\(9\) COMP VALUE \+4210892\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. /ptfix *          15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. / a\ptfix            15 IBLANK-ATTR-1     PIC X(4) VALUE X'\''0000003C'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "15 OHIGH-ATTR-1      PIC S9\(9\) COMP VALUE \+4210936\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. /ptfix *          15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. / a\ptfix            15 OHIGH-ATTR-1      PIC X(4) VALUE X'\''00000038'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

                    files_to_touch=`${my_egrep} -H "15 IHIGH-ATTR-1      PIC S9\(9\) COMP VALUE \+4210888\." "${prepare_dir}/${uc_target}"/*.${file_ext} 2> /dev/null | ${my_awk} -F':' '{print $1}' | ${my_sort} -u`

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. /ptfix *          15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. /' "${prepare_dir}/${uc_target}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. / a\ptfix            15 IHIGH-ATTR-1      PIC X(4) VALUE X'\''00000048'\''.' "${prepare_dir}/${uc_target}"/*.${file_ext}
                    done

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
                target_files=`cd "${prepare_dir}/${uc_target}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null`

                for target_file in ${target_files} ; do
                    echo -ne "        Processing file ${prepare_dir}/${uc_target}/${target_file} for SUBSYS keyword munging ... "
                    this_line_count=`${my_wc} -l "${prepare_dir}/${uc_target}/${target_file}" | ${my_awk} '{print $1}'`
                    subsys_lines=($(${my_egrep} -n "SUBSYS=" "${prepare_dir}/${uc_target}/${target_file}" | ${my_awk} -F':' '{print $1}'))

                    for subsys_line in ${subsys_lines[*]} ; do
                        alt_label=""
                    
                        # Get the original label
                        let orig_label_line=${subsys_line}
                        orig_label=`${my_egrep} -n "SUBSYS=" "${prepare_dir}/${uc_target}/${target_file}" | ${my_egrep} "^${subsys_line}:" | ${my_awk} '{print $1}' | ${my_awk} -F'/' '{print $NF}'`
                        alt_label=`${my_egrep} -A${this_line_count} "//${orig_label}.*SUBSYS=" "${prepare_dir}/${uc_target}/${target_file}" | ${my_egrep} "DDNAME=" | head -1 | ${my_awk} -F',' '{print $1}' | ${my_awk} -F'=' '{print $NF}'`
                        let alt_label_line=`${my_egrep} -n "^//${alt_label}" "${prepare_dir}/${uc_target}/${target_file}" | ${my_awk} -F':' '{print $1}'`
                    
                        # Munge the labels if we have enough pieces
                        if [ "${orig_label}" != "" -a "${alt_label}" != ""  ]; then
                            let label_line_counter=${orig_label_line}
                    
                            # Comment out all the lines between SUBSYS start and the line above "^//${alt_label}"
                            while [ ${label_line_counter} -lt ${alt_label_line}  ]; do
                                this_line=`${my_egrep} -n "^.*$" "${prepare_dir}/${uc_target}/${target_file}" | ${my_egrep} "^${label_line_counter}:" | ${my_sed} -e "s/^${label_line_counter}://g"`
                                first3_chars=`echo "${this_line}" | ${my_cut} -b 1-3 | ${my_sed} -e 's/\*/\\\*/g'`
                                line_remainder=`echo "${this_line}" | ${my_sed} -e "s?^${first3_chars}??g" -e 's/\*/\\\*/g'`
                                ${my_sed} -i -e "${label_line_counter}s?^${this_line}\$?${comment_prefix}${line_remainder}?g" "${prepare_dir}/${uc_target}/${target_file}"
                                let label_line_counter=${label_line_counter}+1
                            done
                    
                            # Fix the alt label line
                            ${my_sed} -i -e "s?^//${alt_label}?//${orig_label}?g" "${prepare_dir}/${uc_target}/${target_file}"
                        else
                            echo -ne " Missing label(s): ORIG LABEL=${orig_label}, ALT_LABEL=${alt_label} ... "
                        fi
                    
                    done

                    echo "DONE"

                    echo -ne "        Processing file ${prepare_dir}/${uc_target}/${target_file} for INCLUDE keyword munging ... "
                    target_lines=($(${my_egrep} -n "INCLUDE.*MEMBER=|//[^\*|\ ]" "${prepare_dir}/${uc_target}/${target_file}" | ${my_egrep} -A1 "INCLUDE" | ${my_egrep} "^[0-9]*:" | ${my_sed} -e 's/\ /:ZZqC:/g'))
                    element_count=${#target_lines[@]}

                    # There should always be pairs of lines found, a start line and an end line
                    let line_counter=0
                
                    while [ ${line_counter} -lt ${element_count} ]; do
                
                        # Get the start line number
                        let start_line=`echo "${target_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' | ${my_awk} -F':' '{print $1}'`
                
                        # Get the real line
                        real_start_line=`echo "${target_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${start_line}://g"`
                
                        # Increment the counter to find the end_line
                        let line_counter=${line_counter}+1
                
                        # Get the end line number
                        let end_line=`echo "${target_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' | ${my_awk} -F':' '{print $1}'`
                
                        # Subtract 1 from ${end_line} to avoid inclusivity
                        let end_line=${end_line}-1
                
                        # Figure out the integer for -A argument in egrep
                        let lines_after=${end_line}-${start_line}
                
                        # Get the actual lines to be munged
                        real_target_lines=($(${my_egrep} -A${lines_after} "^${real_start_line}$" "${prepare_dir}/${uc_target}/${target_file}" | ${my_sed} -e 's/\ /:ZZqC:/g'))
                
                        # Munge the lines in question
                        for real_target_line in ${real_target_lines[*]} ; do
                            real_target_line=`echo "${real_target_line}" | ${my_sed} -e 's/:ZZqC:/\ /g'`
                            first3_chars=`echo "${real_target_line}" | ${my_cut} -b 1-3 | ${my_sed} -e 's/\*/\\\*/g'`
                            line_remainder=`echo "${real_target_line}" | ${my_sed} -e "s?^${first3_chars}??g"`

                            if [ "${real_target_line}" != "${comment_prefix}${line_remainder}" ]; then
                                eval "${my_sed} -i -e 's?^${real_target_line}\$?${comment_prefix}${line_remainder}?g' \"${prepare_dir}/${uc_target}/${target_file}\""
                            fi

                        done
                
                        # Munge the start line
                        first3_chars=`echo "${real_start_line}" | ${my_cut} -b 1-3 | ${my_sed} -e 's/\*/\\\*/g'`
                        line_remainder=`echo "${real_start_line}" | ${my_sed} -e "s?^${first3_chars}??g"`
                        eval "${my_sed} -i -e 's?^${real_start_line}\$?${comment_prefix}${line_remainder}?g' \"${prepare_dir}/${uc_target}/${target_file}\""
                
                        # Increment line_counter to start next pair
                        let line_counter=${line_counter}+1
                    done
                
                    echo "DONE"
                done

            ;;

        esac
 
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
            uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`
            target_dir_var=`echo "${target_dir}" | ${my_sed} -e 's/\./_/g'`
            eval "file_ext=\$${target_dir_var}_ext"

            if [ -d "${prepare_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir_var}_list=\"`cd ${prepare_dir}/${uc_target_dir} 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null`\""
            else
                echo "    WARNING:  Directory \"${prepare_dir}/${uc_target_dir}\" does not exist"
            fi
    
        done

        # Try analyzing
        for list_name in ${TARGETS} ; do
            list_name_var=`echo "${list_name}" | ${my_sed} -e 's/\./_/g'`
            clean_list="${list_name_var}_list"
            eval "${clean_list}=\"\""
        
            for list_item in `eval "echo -ne \\"\\$raw_${list_name_var}_list\\""` ; do
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        for target_dir in ${TARGETS} ; do
            uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`
            echo "    INFO:  Refreshing ${processing_verb} directory \"${source_dir}/${uc_target_dir}\""
            ${my_rm} -rf "${source_dir}/${uc_target_dir}"
            ${my_mkdir} -p "${source_dir}/${uc_target_dir}"
        done

        this_makefile="${script_dir}/makefile.${processing_verb}"

        export uc_copy_list=`echo "${copy_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_copy_ext}/\.${copy_ext}/g"`
        export uc_sysin_list=`echo "${sysin_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_sysin_ext}/\.${sysin_ext}/g"`
        export uc_batch_list=`echo "${batch_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_batch_ext}/\.${batch_ext}/g"`
        export uc_cics_list=`echo "${cics_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_cics_ext}/\.${cics_ext}/g"`
        export uc_ddl_list=`echo "${ddl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_ddl_ext}/\.${ddl_ext}/g"`
        export uc_map_list=`echo "${map_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_map_ext}/\.${map_ext}/g"`
        export uc_jcl_list=`echo "${jcl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_jcl_ext}/\.${jcl_ext}/g"`
        export uc_proc_list=`echo "${proc_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_proc_ext}/\.${proc_ext}/g"`

        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} all\" ... "
            cd "${script_dir}" && ${my_make} -f "${this_makefile}" all > /dev/null 2>&1
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
            ${my_rm} -rf "${report_dir}"
        fi

        this_makefile="${source_dir}/makefile.${processing_verb}"

        if [ -e "${this_makefile}" ]; then

            # Clean out pob subfolders
            for target_dir in ${TARGETS} ; do
                uc_target_dir=`echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]'`

                if [ -d "${source_dir}/${uc_target_dir}/pob" ]; then
                    ${my_rm} -rf "${source_dir}/${uc_target_dir}/pob"
                fi

            done

            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} cleanpob\" ... "
            cd "${source_dir}" && ${my_make} -f "${this_makefile}" cleanpob 
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
            cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
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

# WHAT: Look for items of concern in the Anomalies report
# WHY:  Cannot proceed without some analysis of previous operation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "ANOMALIES"
    report_dir="${source_dir}/Reports-${ucProjectName}"

    if [ -d "${report_dir}" ]; then
        anomaly_report="${report_dir}/report-${ucProjectName}-Anomalies"

        if [ -e "${anomaly_report}" ]; then
            echo -ne "    INFO:  Checking Anomaly report for issues of concern ... "

            let max_warnings=20
            let max_missing=1

            let fatal_count=`${my_egrep} -c ";FATAL;" "${anomaly_report}"`
            let error_count=`${my_egrep} -c ";ERROR;" "${anomaly_report}"`
            let warning_count=`${my_egrep} -c ";WARNING;" "${anomaly_report}"`
            let missing_count=`${my_egrep} -c ";MISSING;" "${anomaly_report}"`

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

    if [ -e "${cobol_copy_report}" ]; then
        echo -ne "    INFO:  Checking Cobol-Copy report for issues of concern ... "
        let cobol_copy_missing_count=`${my_egrep} -c ";MISSING;" "${cobol_copy_report}"`

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
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
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
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
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
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
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
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > /dev/null 2>&1
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
        uc_target=`echo "${target}" | ${my_tr} '[a-z]' '[A-Z]'`

        # Read in regex lines from "${postconvert_dir}/${uc_target}"
        if [ -e "${postconvert_dir}/${uc_target}" -a -s "${postconvert_dir}/${uc_target}" ]; then

            # Here we set the default source_code_dir
            source_code_dir="${pcTarget_dir}/${uc_target}"

            # Here we set the exceptions for source_code_dir
            case ${target} in 

                sysin)
                    source_code_dir="${pcTarget_dir}/Master-${uc_target}/${uc_target}"
                ;;

                copy)
                    source_code_dir="${pcTarget_dir}/Master-${target}/${uc_target}"
                ;;

            esac

            # Here we grab the target files from source_code_dir
            case ${target} in

                batch|cics)
                    target_files=`cd "${source_code_dir}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null ; ${my_ls} *.pco 2> /dev/null`
                ;;

                jcl)
                    file_ext="ksh"
                    target_files=`cd "${source_code_dir}" 2> /dev/null && ${my_ls} *.{file_ext} 2> /dev/null`
                    comment_prefix="#"
                ;;

                *)
                    target_files=`cd "${source_code_dir}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null`
                ;;

            esac

            tmp_dir="/tmp/${USER}/$$"

            if [ ! -d "${tmp_dir}" ]; then
                ${my_mkdir} -p "${tmp_dir}"
            fi

            for target_file in ${target_files} ; do
                echo -ne "    INFO:  Post-Processing \"${source_code_dir}/${target_file}\" for regular expression translation ... "
                tmp_file="${tmp_dir}/translate-post-processing-${uc_target}-${target_file}.$$"
                ${my_rm} -f "${tmp_file}"
                line_count=`${my_wc} -l "${target_dir}/${uc_target}/${target_file}" | ${my_awk} '{print $1}'`

                ${SCRIPT_BASE}/processor.pl --input_file "${source_code_dir}/${target_file}" --output_file "${tmp_file}" --data_type "${target}" --regex_file "${postconvert_dir}/${uc_target}" --mode "post"

                # Move ${tmp_file} to ${source_code_dir}/${target_file}
                if [ -e "${tmp_file}" -a -s "${tmp_file}" ]; then
                    ${my_rsync} "${tmp_file}" "${source_code_dir}/${target_file}" > /dev/null 2>&1
                    echo "DONE"
                else
                    echo "FAILED"
                fi

                # Munge SQL statements in Cobol programs
                if [ "${target}" = "cics" -o "${target}" = "batch" -o "${target}" = "copy" ]; then
                    comment_prefix='      *'
                    echo -ne "    INFO:  Extra Post-Processing of \"${source_code_dir}/${target_file}\" SQL timestamp conversion ... "

                    # Rule #1: MWDB2ORA.MAKE_TIME change to use SYSTIMESTAMP
                    ${my_sed} -i -e "s?MWDB2ORA.MAKE_TIME(.*)?MNTC_LST_TM = SYSTIMESTAMP?g" "${source_code_dir}/${target_file}"

                    # Rule #2: MWDB2ORA.TIME2HOST change to TO_CHAR
                    ${my_sed} -i -e "s?MWDB2ORA.TIME2HOST(\(.*\))?TO_CHAR(\1,'HH24.MI.SS')?g" "${source_code_dir}/${target_file}"

                    # Rule #3: MWDB2ORA.STR2TIME change to use TO_TIMESTAMP
                    ${my_sed} -i -e "s?MWDB2ORA.STR2TIME(\(.*\))?TO_TIMESTAMP(\1,'HH24.MI.SS')?g" "${source_code_dir}/${target_file}"

                    # Rule #4: MWDB2ORA.STR2TMS remove
                    ${my_sed} -i -e "s?MWDB2ORA.STR2TMS(\(.*\))?\1?g" "${source_code_dir}/${target_file}"

                    # Rule #5: MWDB2ORA.STR2DATE change to use TO_CHAR
                    ${my_sed} -i -e "s?BETWEEN MWDB2ORA.STR2DATE(\(.*\) AND \(.*\))?BETWEEN TO_DATE(\1, 'MM/DD/YYYY') AND TO_DATE(\2, 'MM/DD/YYYY')?g" "${source_code_dir}/${target_file}"
                    ${my_sed} -i -e "s?EXEC SQL SELECT MWDB2ORA.STR2DATE(\(.*\), \(.*\), .*)?EXEC SQL SELECT TO_CHAR(\1, \2, 'MM-DD-YYYY')?g" "${source_code_dir}/${target_file}"

                fi

                # Now munge for IEFBR14 file deletion optimization
                if [ "${target}" = "jcl" ]; then
                    comment_prefix='#'
                    echo -ne "    INFO:  Extra Post-Processing of \"${source_code_dir}/${target_file}\" for MOD,DELETE,DELETE optimization ... "
                    iefbr14_lines=($(${my_egrep} -n "^\(|IEFBR14" "${source_code_dir}/${target_file}" | ${my_egrep} -B1 "IEFBR14" | ${my_egrep} "^[0-9]*:" | ${my_awk} -F':' '{print $1}'))

                    # Blocks of code can be detected as line number pairs
                    let line_counter=${#iefbr14_lines[@]}-1

                    # Valid inputs occur in pairs: a line beginning with "(", and a line ending with "IEFBR14"
                    while [ ${line_counter} -gt 0 ]; do
                        let ending_line_number=${iefbr14_lines[$line_counter]}
                        let line_counter=${line_counter}-1
                        let starting_line_number=${iefbr14_lines[$line_counter]}
                        let line_counter=${line_counter}-1

                        # Comment out ending_line_number
                        ending_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} "^${ending_line_number}:" | ${my_sed} -e "s/^${ending_line_number}://g"`
                        ${my_sed} -i -e "${ending_line_number}s?^${ending_line}\$?#${ending_line}?g" "${source_code_dir}/${target_file}"

                        # Move up one element in the array
                        let next_line_up=${ending_line_number}-1

                        # Munge all lines that match between ${starting_line_number} and ${ending_line_number}
                        while [ ${next_line_up} -gt ${starting_line_number} ]; do

                            # Capture this line
                            line_to_munge=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} "^${next_line_up}:" | ${my_sed} -e "s/^${next_line_up}://g"`
                            let mod_del_del_check=`echo "${line_to_munge}" | ${my_egrep} -c "m_FileAssign.*MOD,DELETE,DELETE"`

                            if [ ${mod_del_del_check} -gt 0 ]; then
                                data_file=`echo "${line_to_munge}" | ${my_awk} -F'/' '{print "${DATA}/" $NF}'`

                                if [ "${data_file}" != "" ]; then
                                    ${my_sed} -i -e "${next_line_up}s?^${line_to_munge}\$?${ksh_offset}m_FileExist -r DELSW1 ${data_file}\\${NL}${ksh_offset}if [[ \${DELSW1} = true ]]; then\\${NL}${ksh_offset}   m_FileDelete ${data_file}\\${NL}${ksh_offset}fi?g" "${source_code_dir}/${target_file}"
                                fi

                            fi

                            let next_line_up=${next_line_up}-1
                        done

                    done

                    # Now munge for FTP get commands in ksh JCL conversion 
                    let has_ftpbatch=`${my_egrep} -c "m_ProcInclude.*FTPBATCH" "${source_code_dir}/${target_file}"`

                    # Make sure we have an FTPBATCH section upon which to operate
                    if [ ${has_ftpbatch} -gt 0  ]; then
                        # FTPBATCH put or get?
                        let put_block=`${my_egrep} -c "^put " "${source_code_dir}/${target_file}"`
                        let get_block=`${my_egrep} -c "^get " "${source_code_dir}/${target_file}"`

                        if [ ${put_block} -gt 0  ]; then
                            echo "Found put block"
                            line_count=`${my_wc} -l "${source_code_dir}/${target_file}" | ${my_awk} '{print $1}'`
                            let has_cnvtls=`${my_egrep} -c "^\(CNVTLS[0-9]*\)" "${source_code_dir}/${target_file}"`

                            # Make sure the file hasn't already been converted for FTPBATCH put operations
                            if [ ${has_cnvtls} -eq 0  ]; then
                                let cnvtls_data_counter=0
                                put_batch_lines=($(${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -B${line_count} "^[0-9]*:put " | ${my_egrep} "^[0-9]*:.*m_ProcInclude.*\ FTPBATCH" | tail -1 | ${my_awk} -F':' '{print $1}'))
                                let put_batch_line_count=${#put_batch_lines[@]}-1

                                # Start operating on the last FTPBATCH line, so as to not disturb line positions after processing
                                while [ ${put_batch_line_count} -ge 0  ]; do
                                    function_start_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -B${line_count} "^${put_batch_lines[$put_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:\(" | tail -1`
                                    function_start_line_number=`echo "${function_start_line}" | ${my_awk} -F':' '{print $1}'`
                                    function_start_line=`echo "${function_start_line}" | ${my_sed} -e "s/^${function_start_line_number}://g"`

                                    function_end_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -A${line_count} "^${put_batch_lines[$put_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:_end" | head -1`
                                    function_end_line_number=`echo "${function_end_line}" | ${my_awk} -F':' '{print $1}'`
                                    function_end_line=`echo "${function_end_line}" | ${my_sed} -e "s/^${function_end_line_number}://g"`

                                    jump_label=`echo "${function_start_line}" | ${my_sed} -e 's/[\(|\)]//g'`
                                    jump_label_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -B${line_count} "^[0-9]*:\(${jump_label}\)$" | ${my_egrep} "^[0-9]*:.*JUMP_LABEL=${jump_label}$" | tail -1`
                                    jump_label_line_number=`echo "${jump_label_line}" | ${my_awk} -F':' '{print $1}'`
                                    jump_label_line=`echo "${jump_label_line}" | ${my_sed} -e "s/^${jump_label_line_number}://g"`

                                    let function_line_counter=${function_start_line_number}+1
                                    let ftp_put_counter=0

                                    # Figure out all the lines containing DATA file names
                                    while [ ${function_line_counter} -lt ${function_end_line_number}  ]; do
                                        next_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} "^${function_line_counter}:" | ${my_sed} -e "s/^${function_line_counter}://g"`
                                        let is_data_line=`echo "${next_line}" | ${my_egrep} -c "m_FileOverride.*\ FTP\ .*{DATA}/"`

                                        if [ ${is_data_line} -gt 0   ]; then
                                            data_file[${ftp_put_counter}]=`echo "${next_line}" | ${my_awk} '{print $NF}'`

                                            # Rename the ${}data_file[]} to ${data_file[]}.ftp
                                            ${my_sed} -i -e "${function_line_counter}s?${data_file[$ftp_put_counter]}\$?${data_file[$ftp_put_counter]}.ftp?g" "${source_code_dir}/${target_file}"

                                            let ftp_put_counter=${ftp_put_counter}+1
                                        fi

                                        let function_line_counter=${function_line_counter}+1
                                    done

                                    cnvtls_line=""
                                    let data_line_counter=0

                                    while [ ${data_line_counter} -lt ${ftp_put_counter}  ]; do

                                        if [ "${cnvtls_line}" = ""  ]; then
                                            cnvtls_line="###### CONVERT FILE TO LINE SEQUENTIAL ######\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}JUMP_LABEL=CNVTLS${cnvtls_data_counter}\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset};;\\${NL}"
                                            cnvtls_line="${cnvtls_line}\\${NL}"
                                            cnvtls_line="${cnvtls_line}(CNVTLS${cnvtls_data_counter})\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}m_OutputAssign -c \"\*\" SYSPRINT\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}export DD_INRSF=${data_file[$data_line_counter]}\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}export DD_OUTLSF=${data_file[$data_line_counter]}.ftp\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}export DD_INDCB=${data_file[$data_line_counter]}.dcb\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}m_ProgramExec LSCONV"
                                        else
                                            cnvtls_line="${cnvtls_line}\\${NL}"
                                            cnvtls_line="${cnvtls_line}###### CONVERT FILE TO LINE SEQUENTIAL ######\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}JUMP_LABEL=CNVTLS${cnvtls_data_counter}\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset};;\\${NL}"
                                            cnvtls_line="${cnvtls_line}\\${NL}"
                                            cnvtls_line="${cnvtls_line}(CNVTLS${cnvtls_data_counter})\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}m_OutputAssign -c \"\*\" SYSPRINT\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}export DD_INRSF=${data_file[$data_line_counter]}\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}export DD_OUTLSF=${data_file[$data_line_counter]}.ftp\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}export DD_INDCB=${data_file[$data_line_counter]}.dcb\\${NL}"
                                            cnvtls_line="${cnvtls_line}${ksh_offset}m_ProgramExec LSCONV"
                                        fi

                                        let data_line_counter=${data_line_counter}+1
                                        let cnvtls_data_counter=${cnvtls_data_counter}+1
                                    done

                                    # Complete cnvtls_line
                                    cnvtls_line="${cnvtls_line}\\${NL}"
                                    cnvtls_line="${cnvtls_line}# -----------------------------------------------------------------\*\\${NL}"
                                    cnvtls_line="${cnvtls_line}${ksh_offset}JUMP_LABEL=${jump_label}"

                                    ${my_sed} -i -e "${jump_label_line_number}s?^${jump_label_line}\$?${cnvtls_line}?g" "${source_code_dir}/${target_file}"
                                    let put_batch_line_count=${put_batch_line_count}-1
                                done

                            fi

                        fi

                        if [ ${get_block} -gt 0  ]; then
                            line_count=`${my_wc} -l "${source_code_dir}/${target_file}" | ${my_awk} '{print $1}'`
                            echo "Found get block"
                            let has_cnvtrs=`${my_egrep} -c "^\(CNVTRS\)" "${source_code_dir}/${target_file}"`

                            # Make sure the file hasn't already been converted for FTPBATCH get operations
                            if [ ${has_cnvtrs} -eq 0  ]; then
                                let cnvtrs_data_counter=0
                                get_batch_lines=($(${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -B${line_count} "^[0-9]*:get " | ${my_egrep} "^[0-9]*:.*m_ProcInclude.*\ FTPBATCH" | tail -1 | ${my_awk} -F':' '{print $1}'))
                                let get_batch_line_count=${#get_batch_lines[@]}-1

                                # Start operating on the last FTPBATCH line, so as to not disturb line positions after processing
                                while [ ${get_batch_line_count} -ge 0  ]; do
                                    function_start_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -B${line_count} "^${get_batch_lines[$get_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:\(" | tail -1`
                                    function_start_line_number=`echo "${function_start_line}" | ${my_awk} -F':' '{print $1}'`
                                    function_start_line=`echo "${function_start_line}" | ${my_sed} -e "s/^${function_start_line_number}://g"`

                                    function_end_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -A${line_count} "^${get_batch_lines[$get_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:_end" | head -1`
                                    function_end_line_number=`echo "${function_end_line}" | ${my_awk} -F':' '{print $1}'`
                                    function_end_line=`echo "${function_end_line}" | ${my_sed} -e "s/^${function_end_line_number}://g"`

                                    let function_line_counter=${function_start_line_number}+1
                                    let ftp_get_counter=0

                                    # Figure out all the lines containing DATA file names
                                    while [ ${function_line_counter} -lt ${function_end_line_number}  ]; do
                                        next_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} "^${function_line_counter}:" | ${my_sed} -e "s/^${function_line_counter}://g"`
                                        let is_data_line=`echo "${next_line}" | ${my_egrep} -c "m_FileOverride.*\ FTP\ .*{DATA}/"`
                                        let is_get_line=`echo "${next_line}" | ${my_egrep} -c "^get"`

                                        if [ ${is_data_line} -gt 0   ]; then
                                            data_file[${ftp_get_counter}]=`echo "${next_line}" | ${my_awk} '{print $NF}'`

                                            # Rename the ${}data_file[]} to ${data_file[]}.ftp
                                            ${my_sed} -i -e "${function_line_counter}s?${data_file[$ftp_get_counter]}\$?${data_file[$ftp_get_counter]}.ftp?g" "${source_code_dir}/${target_file}"

                                            let ftp_get_counter=${ftp_get_counter}+1
                                        fi

                                        if [ ${is_get_line} -gt 0   ]; then
                                            ${my_sed} -i -e "${function_line_counter}s?^get.*\$?${next_line} \[REPLACE\]?g" "${source_code_dir}/${target_file}"
                                        fi

                                        let function_line_counter=${function_line_counter}+1
                                    done

                                    cnvtrs_line=""
                                    let data_line_counter=0

                                    while [ ${data_line_counter} -lt ${ftp_get_counter}  ]; do

                                        if [ "${cnvtrs_line}" = ""  ]; then
                                            cnvtrs_line="${function_end_line}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${comment_prefix}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}JUMP_LABEL=CNVTRS${cnvtrs_data_counter}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset};;\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}(CNVTRS${cnvtrs_data_counter})\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${comment_prefix} CONVERT FTP FILE TO RECORD SEQUENTIAL\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}m_OutputAssign -c \"\*\" SYSPRINT\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}export DD_INLSF=${data_file[$data_line_counter]}.ftp\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}export DD_OUTRSF=${data_file[$data_line_counter]}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}m_ProgramExec RSCONV"
                                        else
                                            cnvtrs_line="${cnvtrs_line}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}JUMP_LABEL=CNVTRS${cnvtrs_data_counter}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset};;\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}(CNVTRS${cnvtrs_data_counter})\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${comment_prefix} CONVERT FTP FILE TO RECORD SEQUENTIAL\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}m_OutputAssign -c \"\*\" SYSPRINT\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}export DD_INLSF=${data_file[$data_line_counter]}.ftp\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}export DD_OUTRSF=${data_file[$data_line_counter]}\\${NL}"
                                            cnvtrs_line="${cnvtrs_line}${ksh_offset}m_ProgramExec RSCONV"
                                        fi

                                        let data_line_counter=${data_line_counter}+1
                                        let cnvtrs_data_counter=${cnvtrs_data_counter}+1
                                    done

                                    # Replace _end with cnvtrs_lines
                                    ${my_sed} -i -e "${function_end_line_number}s?^${function_end_line}\$?${cnvtrs_line}?g" "${source_code_dir}/${target_file}"

                                    let get_batch_line_count=${get_batch_line_count}-1
                                done

                            fi

                        fi

                    fi

                    # Now munge DFDSS tar backup conversion
                    let has_dfdss=`${my_egrep} -c "m_ProcInclude.*DFDSS\ " "${source_code_dir}/${target_file}"`

                    # Make sure we have an DFDSS section upon which to operate
                    if [ ${has_dfdss} -gt 0   ]; then
                        echo "TAR Conversion"
                    fi

                    # Now munge SMTP tasks
                    let has_smtp=`${my_egrep} -c "m_OutputAssign.*\ SMTP2\ " "${source_code_dir}/${target_file}"`

                    # Make sure we have an SMTP section upon which to operate
                    if [ ${has_smtp} -gt 0   ]; then
                        echo "SMTP Conversion"
                        line_count=`${my_wc} -l "${source_code_dir}/${target_file}" | ${my_awk} '{print $1}'`
                        let has_cnvsmtp=`${my_egrep} -c "^\(CNVSMTP[0-9]*\)" "${source_code_dir}/${target_file}"`

                        # Make sure the file hasn't already been converted for SMTP operations
                        if [ ${has_cnvsmtp} -eq 0  ]; then
                            let cnvsmtp_data_counter=0
                            smtp2_lines=($(${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} "^[0-9]*:.*m_OutputAssign.*\ SMTP2\ " | ${my_awk} -F':' '{print $1}'))
                            let smtp2_line_count=${#smtp2_lines[@]}-1

                            # Start operating on the last SMTP2 line, so as to not disturb line positions during processing
                            while [ ${smtp2_line_count} -ge 0  ]; do
                                function_start_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -B${line_count} "^${smtp2_lines[$smtp2_line_count]}:.*m_OutputAssign.*\ SMTP2\ " | ${my_egrep} "^[0-9]*:\(" | tail -1`
                                function_start_line_number=`echo "${function_start_line}" | ${my_awk} -F':' '{print $1}'`
                                function_start_line=`echo "${function_start_line}" | ${my_sed} -e "s/^${function_start_line_number}://g"`

                                function_end_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -A${line_count} "^${smtp2_lines[$smtp2_line_count]}:.*m_OutputAssign.*\ SMTP2\ " | ${my_egrep} "^[0-9]*:\ *;;$" | head -1`
                                function_end_line_number=`echo "${function_end_line}" | ${my_awk} -F':' '{print $1}'`
                                function_end_line=`echo "${function_end_line}" | ${my_sed} -e "s/^${function_end_line_number}://g"`

                                jump_label=`echo "${function_start_line}" | ${my_sed} -e 's/[\(|\)]//g'`
                                jump_label_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -B${line_count} "^[0-9]*:\(${jump_label}\)$" | ${my_egrep} "^[0-9]*:.*JUMP_LABEL=${jump_label}$" | tail -1`
                                jump_label_line_number=`echo "${jump_label_line}" | ${my_awk} -F':' '{print $1}'`
                                jump_label_line=`echo "${jump_label_line}" | ${my_sed} -e "s/^${jump_label_line_number}://g"`

                                filerepro_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} -A${line_count} "^${smtp2_lines[$smtp2_line_count]}:.*m_OutputAssign.*\ SMTP2\ " | ${my_egrep} "^[0-9]*:.*m_FileRepro" | head -1`
                                filerepro_line_number=`echo "${filerepro_line}" | ${my_awk} -F':' '{print $1}'`
                                filerepro_line=`echo "${filerepro_line}" | ${my_sed} -e "s/^${filerepro_line_number}://g"`

                                # Now we munge the filepro line
                                ${my_sed} -i -e "${filerepro_line_number}s?^${filerepro_line}\$?${ksh_offset}m_Smtp -i SYSUT1?g" "${source_code_dir}/${target_file}"

                                # Now we comment out the SMTP2 line
                                ${my_sed} -i -e "${smtp2_lines[$smtp2_line_count]}s?\(^.*$\)?${comment_prefix}\1?g" "${source_code_dir}/${target_file}"

                                let function_line_counter=${function_start_line_number}+1
                                let smtp_counter=0

                                # Figure out all the lines containing DATA file names
                                while [ ${function_line_counter} -lt ${function_end_line_number}  ]; do
                                    next_line=`${my_egrep} -n "^.*$" "${source_code_dir}/${target_file}" | ${my_egrep} "^${function_line_counter}:" | ${my_sed} -e "s/^${function_line_counter}://g"`
                                    let is_data_line=`echo "${next_line}" | ${my_egrep} -c "m_FileAssign.*\ *[{DATA}{TMP}]/"`

                                    if [ ${is_data_line} -gt 0   ]; then
                                        data_file[${smtp_counter}]=`echo "${next_line}" | ${my_awk} '{print $NF}'`

                                        # Rename the ${}data_file[]} to ${data_file[]}.ls
                                        ${my_sed} -i -e "${function_line_counter}s?${data_file[$smtp_counter]}\$?${data_file[$smtp_counter]}.ls?g" "${source_code_dir}/${target_file}"

                                        let smtp_counter=${smtp_counter}+1
                                    fi

                                    let function_line_counter=${function_line_counter}+1
                                done

                                cnvsmtp_line=""
                                let data_line_counter=0

                                while [ ${data_line_counter} -lt ${smtp_counter}  ]; do

                                    if [ "${cnvsmtp_line}" = ""  ]; then
                                        cnvsmtp_line="###### CONVERT FILE TO LINE SEQUENTIAL ######\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}JUMP_LABEL=CNVSMTP${cnvsmtp_data_counter}\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset};;\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}(CNVSMTP${cnvsmtp_data_counter})\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}m_OutputAssign -c \"\*\" SYSPRINT\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}export DD_INRSF=${data_file[$data_line_counter]}\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}export DD_OUTLSF=${data_file[$data_line_counter]}.ls\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}export DD_INDCB=${data_file[$data_line_counter]}.dcb\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}m_ProgramExec LSCONV"
                                    else
                                        cnvsmtp_line="${cnvsmtp_line}\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}###### CONVERT FILE TO LINE SEQUENTIAL ######\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}JUMP_LABEL=CNVSMTP${cnvsmtp_data_counter}\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset};;\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}(CNVSMTP${cnvsmtp_data_counter})\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}m_OutputAssign -c \"\*\" SYSPRINT\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}export DD_INRSF=${data_file[$data_line_counter]}\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}export DD_OUTLSF=${data_file[$data_line_counter]}.ls\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}export DD_INDCB=${data_file[$data_line_counter]}.dcb\\${NL}"
                                        cnvsmtp_line="${cnvsmtp_line}${ksh_offset}m_ProgramExec LSCONV"
                                    fi

                                    let data_line_counter=${data_line_counter}+1
                                    let cnvsmtp_data_counter=${cnvsmtp_data_counter}+1
                                done

                                # Complete cnvsmtp_line
                                cnvsmtp_line="${cnvsmtp_line}\\${NL}"
                                cnvsmtp_line="${cnvsmtp_line}# -----------------------------------------------------------------\*\\${NL}"
                                cnvsmtp_line="${cnvsmtp_line}${ksh_offset}JUMP_LABEL=${jump_label}"

                                ${my_sed} -i -e "${jump_label_line_number}s?^${jump_label_line}\$?${cnvsmtp_line}?g" "${source_code_dir}/${target_file}"
                                let smtp2_line_count=${smtp2_line_count}-1
                            done

                        fi

                    fi

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
