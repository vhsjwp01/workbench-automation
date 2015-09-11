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
#                                        system.desc and version.mk.  Also
#                                        added data mapper file detection and
#                                        munging for project name inclusion
# 20150828     Jason W. Plummer          Added TARGETS modififcation based
#                                        dependency.  Added report alert 
#                                        toggles
# 20150831     Jason W. Plummer          Made library dependency detection
#                                        more intelligent.  Added line length
#                                        management to MWDB2ORA post ops
# 20150908     Jason W. Plummer          Added option to disable directory
#                                        refresh.  Added code for cleanpob
#                                        rather than call makefile directive for
#                                        same.  Added support run being run
#                                        as user/group "tuxedo" ONLY
# 20150910     Jason W. Plummer          Added support for Find_<var> detection
#                                        and config in ${param}/version.mk

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
# --file_schemas          - The Project Name to use in reporting (OPTIONAL)
# --rdbms_schemas         - A space or comma separated list of RDBMS schemas
# --file_schemas          - A space or comma separated list of file schemas
# --fatal_check           - An on/off toggle for FATAL report checking (OPTIONAL)
# --error_check           - An on/off toggle for ERROR report checking (OPTIONAL)
# --warning_check         - An on/off toggle for WARNING report checking (OPTIONAL)
# --missing_check         - An on/off toggle for MISSING report checking (OPTIONAL)
# --clean_dirs            - An on/off toggle for expunging target dirs (OPTIONAL)
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

# Set the newline variable (needed for sed operations later on)
NL='
'

CLEAN_DIRS="yes"

STDOUT_OFFSET="    "

SCRIPT_NAME="${0}"

USAGE_ENDLINE="\n${STDOUT_OFFSET}${STDOUT_OFFSET}${STDOUT_OFFSET}${STDOUT_OFFSET}"
USAGE="${SCRIPT_NAME}${USAGE_ENDLINE}"
USAGE="${USAGE}[ --wb_workdir            <Path to Work Bench base directory                *REQUIRED*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --file_schemas          <A space or comma separated list of file schemas  *REQUIRED*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --project_name          <The Project Name to use in reporting             *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --input_targets         <A list of directories from which to draw input   *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --wb_toolpath           <The ART Work Bench tool path (Default: ART 13)   *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --target_COBOL_compiler <The COBOL compiler to use (Default: COBOL-IT)    *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --rdbms_schemas         <A space or comma separated list of RDBMS schemas *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --fatal_check           <on/off toggle for FATAL report checking)         *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --error_check           <on/off toggle for ERROR report checking)         *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --warning_check         <on/off toggle for WARNING report checking)       *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --missing_check         <on/off toggle for MISSING report checking)       *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --clean_dirs            <on/off toggle for expunging target dirs)         *OPTIONAL*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --target_DB             <Target DataBase to use (Default: ORACLE)         *OPTIONAL*> ]"

################################################################################
# VARIABLES
################################################################################
#

err_msg=""
exit_code=${SUCCESS}

possible_targets="sysin cics proc jcl copy map ddl batch sibc.cardliba"
refresh_dirs="prepared source"
real_targets=""

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
    this_sed=$(unalias sed > /dev/null 2>&1 ; which sed 2> /dev/null)

    if [ "${this_sed}" = ""  ]; then
        echo "${STDOUT_OFFSET}ERROR:  The command \"sed\" cannot be found"
        return_code=${ERROR}
    else
        if [ "${my_command}" != ""  ]; then
            my_command_check=$(unalias "${my_command}" 2> /dev/null ; which "${my_command}" 2> /dev/null)

            if [ "${my_command_check}" = ""  ]; then
                return_code=${ERROR}
            else
                my_command=$(echo "${my_command}" | ${this_sed} -e 's/[^a-zA-Z0-9]/_/g')
                eval "my_${my_command}=\"${my_command_check}\""
            fi

        else
            echo "${STDOUT_OFFSET}ERROR:  No command was specified"
            return_code=${ERROR}
        fi

    fi

    return ${return_code}
}

#-------------------------------------------------------------------------------

# Add element to array
#
add_to_array() {
    return_code=${SUCCESS}

    if [ "${1}" != "" -a "${2}" != ""  ]; then
        eval "let element_count=\${#${1}[@]}"
        eval "${1}[$element_count]=${2}"
    else
        echo "${STDOUT_OFFSET}ERROR:  Not enough arguments specified to add to an array"
        return_code=${ERROR}
    fi

    return ${return_code}
}

#-------------------------------------------------------------------------------

# Add element to list
#
add_to_list() {
    return_code=${SUCCESS}

    if [ "${1}" != "" -a "${2}" != ""  ]; then
        eval "is_empty=\$${1}"

        if [ "${is_empty}" = ""  ]; then
            eval "${1}=\"${2}\""
        else
            eval "${1}=\"\$${1} ${2}\""
        fi

    else
        echo "${STDOUT_OFFSET}ERROR:  Not enough arguments specified to add to a list"
        return_code=${ERROR}
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

    for command in awk basename cp cut diff dirname egrep find id ls make mkdir pcregrep pwd rm rsync sed sort strings tee tr uname wc ; do
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

# WHAT: Make sure we are the proper user
# WHY:  Cannot continue otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    exit_code=${ERROR}

    this_username=$(${my_id} -un)

    case ${this_username} in

        tuxedo)
            exit_code=${SUCCESS}
        ;;

        *)
            let in_tuxedo_group=$(${my_id} | ${my_egrep} -ci "\(\btuxedo\b\)")

            if [ ${in_tuxedo_group} -gt 0 ]; then
                exit_code=${SUCCESS}
            fi
        
        ;;

    esac

    if [ ${exit_code} -ne ${SUCCESS} ]; then
        err_msg="This account does not have enough privilege to run this script"
    fi

fi

# WHAT: Make sure we have a WB_AUTOMATE directory in which to operate
# WHY:  Cannot continue otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    while (( "${#}"  )); do
        key=$(echo "${1}" | ${my_sed} -e 's?\`??g')
        value=$(echo "${2}" | ${my_sed} -e 's?\`??g')

        case "${key}" in

            --wb_workdir|--project_name|--input_targets|--wb_toolpath|--target_COBOL_compiler|--target_DB|--fatal_check|--error_check|--warning_check|--missing_check|--clean_dirs)
                key=$(echo "${key}" | ${my_sed} -e 's?^--??g')

                if [ "${value}" != "" ]; then
                    eval "${key}=\"${value}\""
                    shift
                    shift
                else
                    echo "${STDOUT_OFFSET}ERROR:  No value assignment can be made for command line argument \"--${key}\""
                    exit_code=${ERROR}
                    shift
                fi

            ;;

            --rdbms_schemas|--file_schemas)
                key=$(echo "${key}" | ${my_sed} -e 's?^--??g')

                if [ "${value}" != "" ]; then
                    value=$(echo "${value}" | ${my_sed} -e 's/,/\ /g')
                    eval "${key}=\"${value}\""
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

    if [ "${clean_dirs}" != "" ]; then
        lc_clean_dirs=$(echo "${clean_dirs}" | ${my_tr} '[A-Z]' '[a-z]')

        case ${lc_clean_dirs} in 

            n|no|false)
                CLEAN_DIRS="no"
            ;;

        esac

    fi

fi

# WHAT: Set some distro specific vars
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    script_dirname=$(${my_dirname} "${SCRIPT_NAME}")
    SCRIPT_BASE=$(cd "${script_dirname}" && ${my_pwd})
    
    os_type=$(${my_uname} -s | ${my_tr} '[A-Z]' '[a-z]')
    cpu_arch=$(${my_uname} -m | ${my_tr} '[A-Z]' '[a-z]' | ${my_sed} -e 's/i[345]86/i686/g')

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

        # Known file extensions (lower case)
        batch_ext="cbl"
        cics_ext="cbl"
        copy_ext="cpy"
        ddl_ext="sql"
        jcl_ext="jcl"
        map_ext="bms"
        proc_ext="proc"
        sibc_cardliba_ext="sysin"
        sysin_ext="sysin"

        all_file_extensions="${batch_ext} ${cics_ext} ${copy_ext} ${ddl_ext} ${jcl_ext} ${map_ext} ${proc_ext} ${sibc_cardliba_ext} ${sysin_ext}"

        file_extensions=$(for i in ${all_file_extensions}; do echo "${i}" ; done | ${my_sort} -u)

        # Known file extensions (upper case)
        uc_batch_ext=$(echo "${batch_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_cics_ext=$(echo "${cics_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_copy_ext=$(echo "${copy_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_ddl_ext=$(echo "${ddl_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_jcl_ext=$(echo "${jcl_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_map_ext=$(echo "${map_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_proc_ext=$(echo "${proc_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_sibc_cardliba_ext=$(echo "${sibc_cardliba_ext}" | ${my_tr} '[a-z]' '[A-Z]')
        uc_sysin_ext=$(echo "${sysin_ext}" | ${my_tr} '[a-z]' '[A-Z]')

        # This directory houses eclipse/ART WB harvested script assets
        WB_AUTOMATE="${wb_workdir}"

        # Set the default project name
        ProjectName="AUTOMATE"

        # Override ProjectName is ${project_name} has been set
        if [ "${project_name}" != "" ]; then
            ProjectName="${project_name}"
        fi

        ucProjectName=$(echo "${ProjectName}" | ${my_tr} '[a-z]' '[A-Z]')

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
        cbl_offset="       "

        # Default targets
        TARGETS=$(cd "${LocationOfAssets}" 2> /dev/null && ${my_ls} -d * 2> /dev/null | ${my_tr} '[A-Z]' '[a-z]')

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
    source_dir="${WB_AUTOMATE}/source"
    script_dir="${WB_AUTOMATE}/scripts"

    if [ -d "${param_dir}" -a -d "${script_dir}" ]; then
        echo "    INFO:  Initializing ${processing_verb} resources in directory \"${param_dir}\""

        # Create ${script_dir}/project.txt
        echo "ProjectName=${ProjectName}"                  > "${script_dir}/project.txt"

        for attrib in ParallelNum WorkbenchPath LocationOfAssets WBLOGLEVEL WBEXITONERROR OnlyParsingPhase TargetCOBOLCompiler TargetDataBase ; do
            eval "echo ${attrib}=\"\$${attrib}\" >> \"${script_dir}/project.txt\""
        done

        # Create ${param_dir}/system.desc from template
        if [ -e "${param_dir}/system.desc.template" ]; then

            # Reconstruct "${param_dir}/system.desc"
            if [ -e "${param_dir}/system.desc" ]; then
                ${my_rm} -f "${param_dir}/system.desc"
            fi

            ${my_cp} -p "${param_dir}/system.desc.template" "${param_dir}/system.desc"
            ${my_sed} -i -e "s?::PROJECT_NAME::?${ProjectName}?g" -e "s?::SOURCE_DIR::?${source_dir}?g" "${param_dir}/system.desc"

            # Enable targets based on whether or not files exist in those targets
            for target_dir in ${TARGETS} ; do
                uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')
                target_dir_var=$(echo "${target_dir}" | ${my_sed} -e 's/\./_/g')
                eval "file_ext=\$${target_dir_var}_ext"

                let file_count=$(${my_find} "${source_dir}/${uc_target_dir}"/*.${file_ext} -maxdepth 1 2> /dev/null | ${my_wc} -l | ${my_awk} '{print $1}')

                if [ ${file_count} -gt 0 ]; then
                    let is_commented_out=$(${my_egrep} -c -a "^%directory \"${uc_target_dir}\" type" "${param_dir}/system.desc")

                    if [ ${is_commented_out} -gt 0 ]; then
                        begin_text="% BEGIN: ${uc_target_dir}-DIRECTORY-TARGETS"
                        end_text="% END: ${uc_target_dir}-DIRECTORY-TARGETS"
                        ${my_sed} -i "/${begin_text}/,/${end_text}/{/${begin_text}/n;/${end_text}/!{s/^%\(.*\)$/\1/g}}" "${param_dir}/system.desc"

                        # Find any Library dependencies
                        library_dependencies=$(${my_egrep} -A1 -a "directory.*\"${uc_target_dir}\"" "${param_dir}/system.desc" | ${my_strings} | ${my_egrep} "libraries" | ${my_sed} -e 's/[^A-Z,]//g' -e 's/,/\ /g')

                        for library_dependency in ${library_dependencies} ; do
                            library_dependency_var=$(echo "${library_dependency}" | ${my_sed} -e 's/\./_/g' | ${my_tr} '[A-Z]' '[a-z]')
                            eval "dep_file_ext=\$${library_dependency_var}_ext"
                            let dep_file_count=$(${my_find} "${source_dir}/${library_dependency}"/*.${dep_file_ext} -maxdepth 1 2> /dev/null | ${my_wc} -l | ${my_awk} '{print $1}')

                            if [ ${dep_file_count} -gt 0 ]; then
                                begin_text="% BEGIN: ${library_dependency}-DIRECTORY-TARGETS"
                                end_text="% END: ${library_dependency}-DIRECTORY-TARGETS"
                                ${my_sed} -i "/${begin_text}/,/${end_text}/{/${begin_text}/n;/${end_text}/!{s/^%\(.*\)$/\1/g}}" "${param_dir}/system.desc"

                                # Now add to TARGETS if absent
                                let is_a_target=$(echo "${TARGETS}" | ${my_sed} -e 's/\./_/g' | ${my_egrep} -c '\bcopy\b')

                                if [ ${is_a_target} -eq 0 ]; then
                                    add_to_list TARGETS library_dependency
                                fi

                            fi

                        done

                    fi

                fi

            done

            if [ -e "${param_dir}/version.mk" ]; then

                # Prep SCHEMA* variables for detection
                ${my_sed} -i -e 's/^#\(RDBMS_SCHEMAS =\).*$/\1/g' "${param_dir}/version.mk"
                ${my_sed} -i -e "s/^#\(FILE_SCHEMAS =\).*\$/\1/g" "${param_dir}/version.mk"
                
                # If found, set value, otherwise comment out
                if [ "${rdbms_schemas}" != "" ]; then
                    ${my_sed} -i -e "s/^\(RDBMS_SCHEMAS =\).*\$/\1 ${rdbms_schemas}/g" "${param_dir}/version.mk"
                else
                    ${my_sed} -i -e 's/^\(RDBMS_SCHEMAS =.*$\)/#\1/g' "${param_dir}/version.mk"
                fi

                # If found, set value, otherwise comment out
                if [ "${file_schemas}" != "" ]; then
                    ${my_sed} -i -e "s/^\(FILE_SCHEMAS =\).*\$/\1 ${file_schemas}/g" "${param_dir}/version.mk"
                else
                    ${my_sed} -i -e 's/^\(FILE_SCHEMAS =.*$\)/#\1/g' "${param_dir}/version.mk"
                fi

                # Enable extensions based on TARGETS present
                # Find_Jcl = JCL 
                # Find_Prg = BATCH 
                # Find_Tpr = CICS 
                # Find_Map = MAP 
                version_mk_find_extensions="Find_Jcl Find_Prg Find_Tpr Find_Map"

                # First, disable all of them
                for i in ${version_mk_find_extensions} ; do
                    ${my_sed} -i -e "s?^${i} =.*\$?${i} =?g" "${param_dir}/version.mk"
                done

                # Now, enable them by TARGET
                for target in ${TARGETS}; do
                    find_var=""
                    uc_target=$(echo "${target}" | ${my_tr} '[a-z]' '[A-Z]')

                    case ${target} in

                        jcl)
                            find_var="Find_Jcl"
                        ;;

                        batch)
                            find_var="Find_Prg"

                        ;;

                        cics)
                            find_var="Find_Tpr"

                        ;;

                        map)
                            find_var="Find_Map"
                        ;;

                    esac

                    if [ "${find_var}" != "" ]; then
                        ${my_sed} -i -e "s?^\(${find_var} =\).*\$?\1 ${uc_target}?g" "${param_dir}/version.mk"
                    fi

                done

            else
                err_msg="Could not locate \"${param_dir}/version.mk\""
                exit_code=${ERROR}
            fi

            # Clean out 

        else
            err_msg="Could not locate system description template file \"${param_dir}/system.desc.template\""
            exit_code=${ERROR}
        fi

    else
        err_msg="Could not locate both parameter file directory \"${PARAM}\" and script directory \"${script_dir}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Check for Mapper files
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    if [ "${file_schemas}" != "" ]; then
        data_map_dir="${param_dir}/file"
        
        for file_schema in ${file_schemas} ; do
            
            if [ ! -e "${data_map_dir}/mapper-${file_schema}.re" -o ! -e "${data_map_dir}/Datamap-${file_schema}.re" ]; then
                echo "    ERROR:  Could not locate both \"${data_map_dir}/mapper-${file_schema}.re\" and \"${data_map_dir}/Datamap-${file_schema}.re\" mapper files"
                let exit_code=${exit_code}+1
            else 

                # Fix Project name in mapper files
                ${my_sed} -i -e "s?^\(data map ${file_schema}-map system cat::\).*\$?\1${ProjectName}?g" "${data_map_dir}/Datamap-${file_schema}.re"
            fi

        done

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
    prepared_dir="${WB_AUTOMATE}/prepared"
    export import_dir prepared_dir

    if [ -d "${script_dir}" -a -d "${input_dir}" -a -d "${import_dir}" ]; then

        # Setup item lists for import
        for target_dir in ${TARGETS} ; do
            uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')
            target_dir_var=$(echo "${target_dir}" | ${my_sed} -e 's/\./_/g')
            eval "file_ext=\$${target_dir_var}_ext"

            if [ -d "${input_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir_var}_list=\"\$(cd \"${input_dir}/${uc_target_dir}\" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null)\""
            else
                echo "    WARNING:  Directory \"${input_dir}/${uc_target_dir}\" does not exist"
            fi

        done

        for list_name in ${TARGETS} ; do
            list_name_var=$(echo "${list_name}" | ${my_sed} -e 's/\./_/g')
            clean_list="${list_name_var}_list"
            eval "${clean_list}=\"\""
            raw_list="raw_${list_name_var}_list"
        
            for list_item in $(eval "echo \$${raw_list}") ; do
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        if [ "${CLEAN_DIRS}" = "yes" ]; then

            for target_dir in ${TARGETS} ; do
                uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')
                echo "    INFO:  Refreshing ${processing_verb} directory \"${import_dir}/${uc_target_dir}\""
                ${my_rm} -rf "${import_dir}/${uc_target_dir}"
                ${my_mkdir} -p "${import_dir}/${uc_target_dir}"
            done

        fi

        this_makefile="${script_dir}/makefile.${processing_verb}"
        log_file="${LOGS}/makefile.${processing_verb}.log"

        # Try importing
        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} all\" ... "
            cd "${script_dir}" && ${my_make} -f "${this_makefile}" all > "${log_file}" 2>&1
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "FAILED"
                err_msg="${processing_verb} processing of targets failed.  See \${log_file}\" for details"
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
    prepared_dir="${WB_AUTOMATE}/prepared"
    export prepared_dir

    if [ -d "${script_dir}" -a -d "${import_dir}" -a -d "${prepared_dir}" ]; then

        # Find the files
        for target_dir in ${TARGETS} ; do
            uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')
            target_dir_var=$(echo "${target_dir}" | ${my_sed} -e 's/\./_/g')
            eval "file_ext=\$${target_dir_var}_ext"

            if [ -d "${import_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir_var}_list=\"\$(cd \"${import_dir}/${uc_target_dir}\" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null)\""
            else
                echo "    WARNING:  Directory \"${import_dir}/${uc_target_dir}\" does not exist"
            fi
    
        done

        # Build a list
        for list_name in ${TARGETS} ; do
            list_name_var=$(echo "${list_name}" | ${my_sed} -e 's/\./_/g')
            clean_list="${list_name_var}_list"
            eval "${clean_list}=\"\""
            raw_list="raw_${list_name_var}_list"
        
            for list_item in $(eval "echo \$${raw_list}") ; do
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        # Clean all *POSSIBLE* target directories in prepared, source, and target under
        # ${WB_AUTOMATE}

        if [ "${CLEAN_DIRS}" = "yes" ]; then

            for refresh_dir in ${refresh_dirs} ; do

                for possible_target in ${possible_targets} ; do
                    uc_possible_target=$(echo "${possible_target}" | ${my_tr} '[a-z]' '[A-Z]')

                    if [ -d "${WB_AUTOMATE}/${refresh_dir}/${uc_possible_target}" ]; then
                        echo "    INFO:  Refreshing ${refresh_dir} directory \"${WB_AUTOMATE}/${refresh_dir}/${uc_possible_target}\""
                        ${my_rm} -rf "${WB_AUTOMATE}/${refresh_dir}/${uc_possible_target}"
                    fi

                done

            done

            # Flush the target directory outright
            if [ -d "${WB_AUTOMATE}/target" ]; then
                echo "    INFO:  Refreshing target directory \"${WB_AUTOMATE}/target\""
                ${my_rm} -rf "${WB_AUTOMATE}/target"/*
            fi

            # Flush the logs
            if [ -d "${WB_AUTOMATE}/Logs" ]; then
                echo "    INFO:  Refreshing Log directory \"${WB_AUTOMATE}/Logs\""
                ${my_rm} -rf "${WB_AUTOMATE}/Logs"/*
            fi

        fi

        this_makefile="${script_dir}/makefile.${processing_verb}"
        log_file="${LOGS}/makefile.${processing_verb}.log"

        # Explcitly export the uppercase filenames (with lowercase extensions) file lists
        # if defined
        if [ "${copy_list}" != "" ]; then
            export uc_copy_list=$(echo "${copy_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_copy_ext}/\.${copy_ext}/g")
            add_to_list real_targets "copy"
        fi

        if [ "${sysin_list}" != "" ]; then
            export uc_sysin_list=$(echo "${sysin_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_sysin_ext}/\.${sysin_ext}/g")
            add_to_list real_targets "sysin"
        fi

        if [ "${sibc_cardliba_list}" != "" ]; then
            export uc_sibc_cardliba_list=$(echo "${sibc_cardliba_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_sibc_cardliba_ext}/\.${sibc_cardliba_ext}/g")
            add_to_list real_targets "sibc.cardliba"
        fi

        if [ "${batch_list}" != "" ]; then
            export uc_batch_list=$(echo "${batch_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_batch_ext}/\.${batch_ext}/g")
            add_to_list real_targets "batch"
        fi

        if [ "${cics_list}" != "" ]; then
            export uc_cics_list=$(echo "${cics_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_cics_ext}/\.${cics_ext}/g")
            add_to_list real_targets "cics"
        fi

        if [ "${ddl_list}" != "" ]; then
            export uc_ddl_list=$(echo "${ddl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_ddl_ext}/\.${ddl_ext}/g")
            add_to_list real_targets "ddl"
        fi

        if [ "${map_list}" != "" ]; then
            export uc_map_list=$(echo "${map_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_map_ext}/\.${map_ext}/g")
            add_to_list real_targets "map"
        fi

        if [ "${jcl_list}" != "" ]; then
            export uc_jcl_list=$(echo "${jcl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_jcl_ext}/\.${jcl_ext}/g")
            add_to_list real_targets "jcl"
        fi

        if [ "${proc_list}" != "" ]; then
            export uc_proc_list=$(echo "${proc_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_proc_ext}/\.${proc_ext}/g")
            add_to_list real_targets "proc"
        fi

        # Make the target directories based on what is defined in ${real_targets}
        for refresh_dir in ${refresh_dirs} ; do

            for real_target in ${real_targets} ; do
                uc_real_target=$(echo "${real_target}" | ${my_tr} '[a-z]' '[A-Z]')

                if [ ! -d "${WB_AUTOMATE}/${refresh_dir}/${uc_real_target}" ]; then
                    echo "    INFO:  Creating directory \"${WB_AUTOMATE}/${refresh_dir}/${uc_real_target}\""
                    ${my_mkdir} -p "${WB_AUTOMATE}/${refresh_dir}/${uc_real_target}"
                fi

            done

        done

        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} all\" ... "
            cd "${script_dir}" && ${my_make} -f "${this_makefile}" all > "${log_file}" 2>&1
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "FAILED"
                err_msg="${processing_verb} processing of targets failed.  See \"${log_file}\" for details"
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

    for target_dir in ${TARGETS} ; do
        target_dir_var=$(echo "${target_dir}" | ${my_sed} -e 's/\./_/g')
        eval "file_ext=\$${target_dir_var}_ext"
        uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')

        # Here we slurp in each file in the target directory and plow through each
        # line skipping comment lines
        target_files=$(cd "${prepared_dir}/${uc_target_dir}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null)
        tmp_dir="/tmp/${USER}/$$"

        if [ ! -d "${tmp_dir}" ]; then
            ${my_mkdir} -p "${tmp_dir}"
        fi

        ########################################################################
        ####
        #### BEGIN Regular expression translation via PERL program processor.pl
        ####
        ########################################################################

        for target_file in ${target_files} ; do
            echo -ne "    INFO:  Pre-Processing \"${prepared_dir}/${uc_target_dir}/${target_file}\" for regular expression translation ... "
            tmp_file="${tmp_dir}/translate-pre-processing-${uc_target_dir}-${target_file}.$$"
            ${my_rm} -f "${tmp_file}"

            # Read in regex lines from "${preconvert_dir}/${uc_target_dir}"
            if [ -e "${preconvert_dir}/${uc_target_dir}" -a -s "${preconvert_dir}/${uc_target_dir}" ]; then
                "${SCRIPT_BASE}"/processor.pl --input_file "${prepared_dir}/${uc_target_dir}/${target_file}" --output_file "${tmp_file}" --data_type "${target_dir}" --regex_file "${preconvert_dir}/${uc_target_dir}" --mode "pre"
                        
                # Move ${tmp_file} to ${prepared_dir}/${uc_target_dir}/${target_file}
                if [ -e "${tmp_file}" -a -s "${tmp_file}" ]; then
                    ${my_diff} -q "${tmp_file}" "${prepared_dir}/${uc_target_dir}/${target_file}" > /dev/null 2>&1

                    if [ ${?} -ne ${SUCCESS} ]; then
                        ${my_rsync} "${tmp_file}" "${prepared_dir}/${uc_target_dir}/${target_file}" > /dev/null 2>&1
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

        ########################################################################
        ####
        #### END Regular expression translation via PERL program processor.pl
        ####
        ########################################################################

        ########################################################################
        ####
        #### BEGIN Extra pre-processing by type
        ####
        ########################################################################

        case ${target_dir} in 

            copy|batch|cics)
                comment_prefix='      *'
                echo "    INFO:  Extra Pre-Processing ${prepared_dir}/${uc_target_dir} files for ASIS translation:"
                target_files=$(cd "${prepared_dir}/${uc_target_dir}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null)
                
                for target_file in ${target_files} ; do

                    ################################################################
                    ####
                    #### BEGIN ASIS keyword translation
                    ####
                    ################################################################

                    echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for ASIS keyword translation ... "
                    # This command sequence should yield the proper triplet of info
                    valid_lines=($(${my_egrep} -n -a "EXEC CICS RECEIVE|ASIS|END-EXEC" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -A2 "EXEC CICS RECEIVE" | ${my_egrep} "^[0-9]*:" | ${my_sed} -e 's/\ /:ZZqC:/g'))
                    let line_counter=0
                
                    # Valid inputs occur in threes: a line with "EXEC CICS RECEIVE", followed by a line with "ASIS", followed by "END-EXEC"
                    while [ ${line_counter} -lt ${#valid_lines[@]} ]; do
                        exec_line_number=$(echo "${valid_lines[$line_counter]}" | ${my_awk} -F':' '{print $1}')
                        real_exec_line=$(echo "${valid_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${exec_line_number}://g")
                        let is_exec_line=$(echo "${real_exec_line}" | ${my_egrep} -c "EXEC CICS RECEIVE")
                
                        let line_counter=${line_counter}+1
                
                        asis_line_number=$(echo "${valid_lines[$line_counter]}" | ${my_awk} -F':' '{print $1}')
                        real_asis_line=$(echo "${valid_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${asis_line_number}://g")
                        let is_asis_line=$(echo "${real_asis_line}" | ${my_egrep} -c "ASIS")
                
                        let line_counter=${line_counter}+1
                
                        end_line_number=$(echo "${valid_lines[$line_counter]}" | ${my_awk} -F':' '{print $1}')
                        real_end_line=$(echo "${valid_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${end_line_number}://g")
                        let is_end_line=$(echo "${real_end_line}" | ${my_egrep} -c "END-EXEC")
                
                        # If all three tests are equal to 1, then we found the properly ordered triplet
                        # And we need to comment out the line number containing ASIS
                        if [ ${is_exec_line} -eq 1 -a ${is_asis_line} -eq 1 -a ${is_end_line} -eq 1 ]; then
                            line_prefix=$(echo "${real_asis_line}" | ${my_cut} -b 1-7)
                            line_remainder=$(echo "${real_asis_line}" | ${my_sed} -e "s/^${line_prefix}//g")
                            ${my_sed} -i -e "${asis_line_number}s/^${real_asis_line}/${comment_prefix}${line_remainder}/g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                        fi

                        let line_counter=${line_counter}+1
                    done

                    echo "DONE"

                    ################################################################
                    ####
                    #### END ASIS keyword translation
                    ####
                    ################################################################

                   # ################################################################
                   # ####
                   # #### BEGIN IBCABEND keyword translation
                   # ####
                   # ################################################################

                   # echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for IBCABEND filename translation ... "
                   # ibcabend_lines=$(${my_egrep} -n -a "\bCOPY\ *IBCABEND\." "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_sed} -e 's?\ ?:ZZqC:?g')

                   # for ibcabend_line in ${ibcabend_lines} ; do
                   #     ibcabend_line=$(echo "${ibcabend_line}" | ${my_sed} -e 's?:ZZqC:?\ ?g')

                   #     let has_cpy_suffix=$(echo "${ibcabend_line}" | ${my_egrep} -c "IBCABEND\.cpy\.")

                   #     if [ ${has_cpy_suffix} -eq 0 ]; then
                   #         this_line=$(echo "${ibcabend_line}" | ${my_awk} -F':' '{print $1}')
                   #         ${my_sed} -i -e "${this_line}s?^\(.*\)\(COPY\) *\(IBCABEND\)\.\(.*\)?\1\\${NL}${cbl_offset}${cbl_offset}\2\ \3.cpy\.\4?g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                   #     fi

                   # done

                   # echo "DONE"

                   # ################################################################
                   # ####
                   # #### END IBCABEND keyword translation
                   # ####
                   # ################################################################

                    ################################################################
                    ####
                    #### BEGIN C1MATCHI keyword translation
                    ####
                    ################################################################

                    # MULTILINE GREP!!!!
                    # egrep -n "^.*$" /tmp/junk | pcregrep -M -e "\bEXEC\b[[:space:]|\n\d+:]*\bCICS\b[[:space:]|\n\d+:]*\bLINK\b[[:space:]|\n\d+:]*\bPROGRAM\b[[:space:]|\n\d+:]*\(C1MATCHI\)[[:space:]|\n\d+:]*\bCOMMAREA\b[[:space:]|\n\d+:]*\(CODE1\-LINKAGE\-IO\)[[:space:]|\n\d+:]*LENGTH[[:space:]|\n\d+:]*\(LINKAGE\-LENGTH\)[[:space:]|\n\d+:]*\bEND\-EXEC\b\." | sort -rn
                    # Must process in reverse order to maintain line number

                    echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for C1MATCHI keyword translation ... "
                    c1matchi_lines=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_pcregrep} -M -e "\bEXEC\b[[:space:]|\n\d+:]*\bCICS\b[[:space:]|\n\d+:]*\bLINK\b[[:space:]|\n\d+:]*\bPROGRAM\b[[:space:]|\n\d+:]*\(C1MATCHI\)[[:space:]|\n\d+:]*\bCOMMAREA\b[[:space:]|\n\d+:]*\(CODE1\-LINKAGE\-IO\)[[:space:]|\n\d+:]*LENGTH[[:space:]|\n\d+:]*\(LINKAGE\-LENGTH\)[[:space:]|\n\d+:]*\bEND\-EXEC\b\." | ${my_sed} -e 's?\ ?:ZZqC:?g' | ${my_sort} -rn)

                    for c1matchi_line in ${c1matchi_lines} ; do
                        real_line=$(echo "${c1matchi_line}" | ${my_sed} -e 's/:ZZqC:/\ /g')
                        start_line=$(echo "${real_line}" | ${my_egrep} "\bEXEC\b" | ${my_egrep} -v "\bEND\-EXEC\b\.")
                        end_line=$(echo "${real_line}" | ${my_egrep} "\bEND\-EXEC\.\b")

                        if [ "${start_line}" != "" ]; then
                            first_line_number=$(echo "${start_line}" | ${my_awk} -F ':' '{print $1}')
                            first_line=$(echo "${start_line}" | ${my_sed} -e "s/^${first_line_number}://g")
                        fi

                        if [ "${end_line}" != "" ]; then
                            last_line_number=$(echo "${end_line}" | ${my_awk} -F ':' '{print $1}')
                            last_line=$(echo "${end_line}" | ${my_sed} -e "s/^${last_line_number}://g")
                        fi

                        if [ "${first_line}" != "" -a "${last_line}" != "" ]; then

                            # Change the last line to the proper sequence
                            let delta=${last_line_number}-${first_line_number}
                            ${my_sed} -i -e "${last_line_number}s?^${last_line}\$?${cbl_offset}CALL 'c1matchi' USING\\${NL}${cbl_offset}${cbl_offset}P9IN P9OUT P9AUDIT\.?g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                            let delta_counter=1

                            # Now delete the rest
                            while [ ${delta_counter} -le ${delta} ]; do
                                let this_line_number=${last_line_number}-${delta_counter}
                                ${my_sed} -i -e "${this_line_number}d" "${prepared_dir}/${uc_target_dir}/${target_file}"
                                let delta_counter=${delta_counter}+1
                            done

                            start_line=""
                            end_line=""
                            first_line_number=""
                            first_line=""
                            last_line_number=""
                            last_line=""
                        fi

                    done

                    echo "DONE"

                    ################################################################
                    ####
                    #### END C1MATCHI keyword translation
                    ####
                    ################################################################

                    ################################################################
                    ####
                    #### BEGIN DUMP TRANSACTION keyword translation
                    ####
                    ################################################################

                    echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for DUMP TRANSACTION keyword translation ... "
                    dump_lines=$(${my_egrep} -n -a "DUMP" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_sed} -e 's?\ ?:ZZqC:?g')
                    
                    for dump_line in ${dump_lines} ; do
                        let add_transaction=0
                        dump_line=$(echo "${dump_line}" | ${my_sed} -e 's?:ZZqC:?\ ?g')
                        let has_transaction_in_it=$(echo "${dump_line}" | ${my_egrep} -c "\bTRANSACTION\b")
                    
                        if [ ${has_transaction_in_it} -eq 0 ]; then
                            let this_line=$(echo "${dump_line}" | ${my_awk} -F':' '{print $1}')
                            let next_line=${this_line}+1
                            let has_transaction_after_it=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -c "^${next_line}:.*\bTRANSACTION\b")
                    
                            if [ ${has_transaction_after_it} -eq 0 ]; then
                                let has_cics_in_it=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -c "^${this_line}:.*\bCICS\b")
                    
                                if [ ${has_cics_in_it} -eq 0 ]; then
                                    let previous_line=${this_line}-1
                                    let has_cics_before_it=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -c "^${previous_line}:.*\bCICS\b")
                    
                                    if [ ${has_cics_before_it} -gt 0 ]; then
                                        let has_exec_in_it=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -c "^${previous_line}:.*\bEXEC\b")
                    
                                        if [ ${has_exec_in_it} -eq 0 ]; then
                                            let previous_previous_line=${previous_line}-1
                                            let has_exec_before_it=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -c "^${previous_previous_line}:.*\bEXEC\b")
                    
                                            if [ ${has_exec_before_it} -gt 0 ];then
                                                let add_transaction=${add_transaction}+1
                                            fi
                    
                                        else
                                            let add_transaction=${add_transaction}+1
                                        fi
                    
                                    fi
                    
                                else
                                    let has_exec_in_it=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -c "^${this_line}:.*\bEXEC\b")
                    
                                    if [ ${has_exec_in_it} -eq 0 ]; then
                                        let previous_line=${this_line}-1
                                        let has_exec_before_it=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -c "^${previous_line}:.*\bEXEC\b")
                    
                                        if [ ${has_exec_before_it} -gt 0 ];then
                                            let add_transaction=${add_transaction}+1
                                        fi
                    
                                    else
                                        let add_transaction=${add_transaction}+1
                                    fi
                    
                                fi
                    
                            fi
                    
                        fi
                    
                        if [ ${add_transaction} -gt 0 ]; then
                            real_line=$(echo "${dump_line}" | ${my_sed} -e 's?^[0-9]*:??g')
                            ${my_sed} -i -e "${this_line}s?^\(${real_line}\)\$?\1\\${NL}${cbl_offset}TRANSACTION?g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                        fi
                    
                    done

                    echo "DONE"

                    ################################################################
                    ####
                    #### END DUMP TRANSACTION keyword translation
                    ####
                    ################################################################

                    ################################################################
                    ####
                    #### BEGIN P9COMM keyword translation
                    ####
                    ################################################################

                    echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for P9COMM keyword translation ... "
                    let has_p9comm=$(${my_egrep} -c -a "COPY\ *P9COMM\." "${prepared_dir}/${uc_target_dir}/${target_file}")

                    if [ ${has_p9comm} -gt 0 ]; then
                        ${my_sed} -i -e "s?COPY\ *P9COMM\.?COPY P9IN\.\\${NL}${cbl_offset}COPY P9OUT\.\\${NL}${cbl_offset}COPY P9AUDIT\.?g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                    fi

                    echo "DONE"

                    ################################################################
                    ####
                    #### BEGIN P9COMM keyword translation
                    ####
                    ################################################################

                done

                ####################################################################
                ####
                #### BEGIN PTFIX keyword translation
                ####
                ####################################################################

                if [ "${target_dir}" = "cics" ]; then
                    echo -ne "    INFO:  Extra Pre-Processing ${prepared_dir}/${uc_target_dir} files for PTFIX translation ... "

                    files_to_touch=$(${my_egrep} -H -a "15 CURSOR-ATTR-1     PIC S9\(9\) COMP VALUE \+16777152\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./ptfix *          15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 CURSOR-ATTR-1     PIC S9(9) COMP VALUE +16777152./ a\ptfix            15 CURSOR-ATTR-1     PIC X(4) VALUE X'\''00FFFF20'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "15 FILLER            PIC S9\(9\) COMP VALUE \+16777160\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 FILLER            PIC S9(9) COMP VALUE +16777160./ptfix *          15 FILLER            PIC S9(9) COMP VALUE +16777160./' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 FILLER            PIC S9(9) COMP VALUE +16777160./ a\ptfix            15 FILLER            PIC X(4) VALUE X'\''00FFFF48'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "OK-ATTR-1         PIC S9\(9\) COMP VALUE \+4210880\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. /ptfix *          15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. /' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 OK-ATTR-1         PIC S9(9) COMP VALUE +4210880. / a\ptfix            15 OK-ATTR-1         PIC X(4) VALUE X'\''00000020'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "15 PROT-ATTR-1       PIC S9\(9\) COMP VALUE \+4210928\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. /ptfix *          15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. /' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 PROT-ATTR-1       PIC S9(9) COMP VALUE +4210928. / a\ptfix            15 PROT-ATTR-1       PIC X(4) VALUE X'\''00000030'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "15 BLANK-ATTR-1      PIC S9\(9\) COMP VALUE \+4210940\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. /ptfix *          15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. /' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 BLANK-ATTR-1      PIC S9(9) COMP VALUE +4210940. / a\ptfix            15 BLANK-ATTR-1      PIC X(4) VALUE X'\''00000025'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "15 CBLANK-ATTR-1     PIC S9\(9\) COMP VALUE \+16777164\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./ptfix *          15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 CBLANK-ATTR-1     PIC S9(9) COMP VALUE +16777164./ a\ptfix            15 CBLANK-ATTR-1     PIC X(4) VALUE X'\''00FFFF3C'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "15 IBLANK-ATTR-1     PIC S9\(9\) COMP VALUE \+4210892\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. /ptfix *          15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. /' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 IBLANK-ATTR-1     PIC S9(9) COMP VALUE +4210892. / a\ptfix            15 IBLANK-ATTR-1     PIC X(4) VALUE X'\''0000003C'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "15 OHIGH-ATTR-1      PIC S9\(9\) COMP VALUE \+4210936\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. /ptfix *          15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. /' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 OHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210936. / a\ptfix            15 OHIGH-ATTR-1      PIC X(4) VALUE X'\''00000038'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    files_to_touch=$(${my_egrep} -H -a "15 IHIGH-ATTR-1      PIC S9\(9\) COMP VALUE \+4210888\." "${prepared_dir}/${uc_target_dir}"/*.${file_ext} 2> /dev/null | ${my_strings} | ${my_awk} -F':' '{print $1}' | ${my_sort} -u)

                    for file_to_touch in ${files_to_touch} ; do
                        ${my_sed} -i 's/                 15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. /ptfix *          15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. /' "${prepared_dir}/${uc_target_dir}"/*.${file_ext} &&
                        ${my_sed} -i '/ptfix \*          15 IHIGH-ATTR-1      PIC S9(9) COMP VALUE +4210888. / a\ptfix            15 IHIGH-ATTR-1      PIC X(4) VALUE X'\''00000048'\''.' "${prepared_dir}/${uc_target_dir}"/*.${file_ext}
                    done

                    if [ ${?} -eq ${SUCCESS} ]; then
                        echo "SUCCESS"
                    else
                        echo "FAILED"
                        err_msg="Extra PracTrans sed translation failed on ${prepared_dir}/${uc_target_dir} Cobol files"
                        exit_code=${ERROR}
                    fi

                fi

                ####################################################################
                ####
                #### END PTFIX keyword translation
                ####
                ####################################################################

            ;;

            jcl|proc|sysin)

                ####################################################################
                ####
                #### BEGIN SUBSYS and INCLUDE keyword translation
                ####
                ####################################################################

                comment_prefix='//*'
                echo "    INFO:  Extra Pre-Processing ${prepared_dir}/${uc_target_dir} files for %%, SUBSYS and INCLUDE translation:"
                target_files=$(cd "${prepared_dir}/${uc_target_dir}" 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null)

                for target_file in ${target_files} ; do
                    let has_percent_percent=$(${my_egrep} -c "%%" "${prepared_dir}/${uc_target_dir}/${target_file}")

                    if [ ${has_percent_percent} -gt 0 ]; then
                        echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for %% keyword translation ... "
                        ${my_sed} -i 's/%%\([A-Z]*[0-9]*\)/${\1}/g' "${prepared_dir}/${uc_target_dir}/${target_file}"
                        echo "DONE"
                    fi

                    echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for SUBSYS keyword translation ... "
                    this_line_count=$(${my_wc} -l "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_awk} '{print $1}')
                    subsys_lines=($(${my_egrep} -n -a "SUBSYS=" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_awk} -F':' '{print $1}'))

                    for subsys_line in ${subsys_lines[*]} ; do
                        alt_label=""
                    
                        # Get the original label
                        let orig_label_line=${subsys_line}
                        orig_label=$(${my_egrep} -n -a "SUBSYS=" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^${subsys_line}:" | ${my_awk} '{print $1}' | ${my_awk} -F'/' '{print $NF}')
                        alt_label=$(${my_egrep} -A${this_line_count} -a "//${orig_label}.*SUBSYS=" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} "DDNAME=" | head -1 | ${my_awk} -F',' '{print $(NF-1)}' | ${my_awk} -F'=' '{print $NF}')
                    
                        # Munge the labels if we have enough pieces
                        if [ "${orig_label}" != "" -a "${alt_label}" != "" ]; then
                            let alt_label_line=$(${my_egrep} -n -a "^//${alt_label}" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_awk} -F':' '{print $1}')
                            let label_line_counter=${orig_label_line}
                    
                            # Comment out all the lines between SUBSYS start and the line above "^//${alt_label}"
                            while [ ${label_line_counter} -lt ${alt_label_line} ]; do
                                this_line=$(${my_egrep} -n -a "^.*$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^${label_line_counter}:" | ${my_sed} -e "s/^${label_line_counter}://g")
                                first3_chars=$(echo "${this_line}" | ${my_cut} -b 1-3 | ${my_sed} -e 's/\*/\\\*/g')
                                line_remainder=$(echo "${this_line}" | ${my_sed} -e "s?^${first3_chars}??g" -e 's/\*/\\\*/g')
                                ${my_sed} -i -e "${label_line_counter}s?^${this_line}\$?${comment_prefix}${line_remainder}?g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                                let label_line_counter=${label_line_counter}+1
                            done
                    
                            # Fix the alt label line
                            ${my_sed} -i -e "s?^//${alt_label}?//${orig_label}?g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                        else
                            echo -ne " Missing label(s): ORIG LABEL=${orig_label}, ALT_LABEL=${alt_label} ... "
                        fi
                    
                    done

                    echo "DONE"

                    echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for INCLUDE keyword translation ... "
                    target_lines=($(${my_egrep} -n -a "INCLUDE.*MEMBER=|//[^\*|\ ]" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_egrep} -A1 "INCLUDE" | ${my_egrep} "^[0-9]*:" | ${my_sed} -e 's/\ /:ZZqC:/g'))
                    element_count=${#target_lines[@]}

                    # There should always be pairs of lines found, a start line and an end line
                    let line_counter=0
                
                    while [ ${line_counter} -lt ${element_count} ]; do
                
                        # Get the start line number
                        let start_line=$(echo "${target_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' | ${my_awk} -F':' '{print $1}')
                
                        # Get the real line
                        real_start_line=$(echo "${target_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' -e "s/^${start_line}://g")
                
                        # Increment the counter to find the end_line
                        let line_counter=${line_counter}+1
                
                        # Get the end line number
                        let end_line=$(echo "${target_lines[$line_counter]}" | ${my_sed} -e 's/:ZZqC:/\ /g' | ${my_awk} -F':' '{print $1}')
                
                        # Subtract 1 from ${end_line} to avoid inclusivity
                        let end_line=${end_line}-1
                
                        # Figure out the integer for -A argument in egrep
                        let lines_after=${end_line}-${start_line}
                
                        # Get the actual lines to be munged
                        real_target_lines=($(${my_egrep} -A${lines_after} -a "^${real_start_line}$" "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_sed} -e 's/\ /:ZZqC:/g'))
                
                        # Munge the lines in question
                        for real_target_line in ${real_target_lines[*]} ; do
                            real_target_line=$(echo "${real_target_line}" | ${my_sed} -e 's/:ZZqC:/\ /g')
                            first3_chars=$(echo "${real_target_line}" | ${my_cut} -b 1-3 | ${my_sed} -e 's/\*/\\\*/g')
                            line_remainder=$(echo "${real_target_line}" | ${my_sed} -e "s?^${first3_chars}??g")

                            if [ "${real_target_line}" != "${comment_prefix}${line_remainder}" ]; then
                                eval "${my_sed} -i -e 's?^${real_target_line}\$?${comment_prefix}${line_remainder}?g' \"${prepared_dir}/${uc_target_dir}/${target_file}\""
                            fi

                        done
                
                        # Munge the start line
                        first3_chars=$(echo "${real_start_line}" | ${my_cut} -b 1-3 | ${my_sed} -e 's/\*/\\\*/g')
                        line_remainder=$(echo "${real_start_line}" | ${my_sed} -e "s?^${first3_chars}??g")
                        eval "${my_sed} -i -e 's?^${real_start_line}\$?${comment_prefix}${line_remainder}?g' \"${prepared_dir}/${uc_target_dir}/${target_file}\""
                
                        # Increment line_counter to start next pair
                        let line_counter=${line_counter}+1
                    done
                
                    echo "DONE"
                done

                ####################################################################
                ####
                #### END SUBSYS and INCLUDE keyword translation
                ####
                ####################################################################

            ;;

        esac

        ########################################################################
        ####
        #### END Extra pre-processing by type
        ####
        ########################################################################
 
    done
    
fi

# WHAT: Perform analysis operations
# WHY:  Needed for proper compilation
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    echo "ANALYZE"
    processing_verb="analyze"
    export source_dir

    if [ -d "${script_dir}" -a -d "${prepared_dir}" -a -d "${source_dir}" ]; then

        # Setup item lists for source
        for target_dir in ${TARGETS} ; do
            uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')
            target_dir_var=$(echo "${target_dir}" | ${my_sed} -e 's/\./_/g')
            eval "file_ext=\$${target_dir_var}_ext"

            if [ -d "${prepared_dir}/${uc_target_dir}" ]; then
                eval "raw_${target_dir_var}_list=\"\$(cd ${prepared_dir}/${uc_target_dir} 2> /dev/null && ${my_ls} *.${file_ext} 2> /dev/null)\""
            else
                echo "    WARNING:  Directory \"${prepared_dir}/${uc_target_dir}\" does not exist"
            fi
    
        done

        # Try analyzing
        for list_name in ${TARGETS} ; do
            list_name_var=$(echo "${list_name}" | ${my_sed} -e 's/\./_/g')
            clean_list="${list_name_var}_list"
            eval "${clean_list}=\"\""
            raw_list="raw_${list_name_var}_list"
        
            for list_item in $(eval "echo \$${raw_list}") ; do
                eval "add_to_list ${clean_list} \"${list_item}\""
            done

            eval "export ${clean_list}"
        done

        if [ "${CLEAN_DIRS}" = "yes" ]; then

            for target_dir in ${TARGETS} ; do
                uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')
                echo "    INFO:  Refreshing ${processing_verb} directory \"${source_dir}/${uc_target_dir}\""
                ${my_rm} -rf "${source_dir}/${uc_target_dir}"
                ${my_mkdir} -p "${source_dir}/${uc_target_dir}"
            done

        fi

        this_makefile="${script_dir}/makefile.${processing_verb}"
        log_file="${LOGS}/makefile.${processing_verb}.log"

        export uc_copy_list=$(echo "${copy_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_copy_ext}/\.${copy_ext}/g")
        export uc_sysin_list=$(echo "${sysin_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_sysin_ext}/\.${sysin_ext}/g")
        export uc_batch_list=$(echo "${batch_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_batch_ext}/\.${batch_ext}/g")
        export uc_cics_list=$(echo "${cics_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_cics_ext}/\.${cics_ext}/g")
        export uc_ddl_list=$(echo "${ddl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_ddl_ext}/\.${ddl_ext}/g")
        export uc_map_list=$(echo "${map_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_map_ext}/\.${map_ext}/g")
        export uc_jcl_list=$(echo "${jcl_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_jcl_ext}/\.${jcl_ext}/g")
        export uc_proc_list=$(echo "${proc_list}" | ${my_tr} '[a-z]' '[A-Z]' | ${my_sed} -e "s/\.${uc_proc_ext}/\.${proc_ext}/g")

        if [ -e "${this_makefile}" ]; then
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} all\" ... "
            cd "${script_dir}" && ${my_make} -f "${this_makefile}" all > "${log_file}" 2>&1
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "FAILED"
                err_msg="${processing_verb} processing of targets failed.  See \"${log_file}\" for details"
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

        ${my_mkdir} -p "${report_dir}"

        this_makefile="${source_dir}/makefile.${processing_verb}"
        log_file="${LOGS}/makefile.${processing_verb}.log"

        if [ -e "${this_makefile}" ]; then

            # Clean out pob subfolders
            if [ "${CLEAN_DIRS}" = "yes" ]; then

                for target_dir in ${TARGETS} ; do
                    uc_target_dir=$(echo "${target_dir}" | ${my_tr} '[a-z]' '[A-Z]')

                    if [ -d "${source_dir}/${uc_target_dir}/pob" ]; then
                        ${my_rm} -rf "${source_dir}/${uc_target_dir}/pob"
                    fi

                done

            fi

            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} cleanpob\" ... "
            #cd "${source_dir}" && ${my_make} -f "${this_makefile}" cleanpob 
            cd "${source_dir}" && ${my_find} . -name "*.pob" -o -name "*.depends" -o -name "*.cdm" -o -name "*.shrec" -exec ${my_rm} -f {} \;
            echo "DONE"
            echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
            cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > "${log_file}" 2>&1
            exit_code=${?}

            if [ ${exit_code} -ne ${SUCCESS} ]; then
                echo "FAILED"
                err_msg="${processing_verb} processing of targets failed.  See \"${log_file}\" for details"
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

            # Set fatal check
            if [ "${fatal_check}" = "no" -o "${fatal_check}" = "off" ]; then
                let fatal_count=0
            else
                let fatal_count=$(${my_egrep} -c -a ";FATAL;" "${anomaly_report}")
            fi

            # Set error check
            if [ "${error_check}" = "no" -o "${error_check}" = "off" ]; then
                let error_count=0
            else
                let error_count=$(${my_egrep} -c -a ";ERROR;" "${anomaly_report}")
            fi

            # Set warning check
            if [ "${warning_check}" = "no" -o "${warning_check}" = "off" ]; then
                let warning_count=0
            else
                let warning_count=$(${my_egrep} -c -a ";WARNING;" "${anomaly_report}")
            fi

            # Set missing check
            if [ "${missing_check}" = "no" -o "${missing_check}" = "off" ]; then
                let missing_count=0
            else
                let missing_count=$(${my_egrep} -c -a ";MISSING;" "${anomaly_report}")
            fi

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
            err_msg="Anomaly report file \"${anomaly_report}\" is either missing or contains no data"
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
        let cobol_copy_missing_count=$(${my_egrep} -c -a ";MISSING;" "${cobol_copy_report}")

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
    log_file="${LOGS}/makefile.${processing_verb}.log"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > "${log_file}" 2>&1
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed.  See \"${log_file}\" for details"
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
    log_file="${LOGS}/makefile.${processing_verb}.log"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > "${log_file}" 2>&1
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed.  See \"${log_file}\" for details"
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
    log_file="${LOGS}/makefile.${processing_verb}.log"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > "${log_file}" 2>&1
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed.  See \"${log_file}\" for details"
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
    log_file="${LOGS}/makefile.${processing_verb}.log"

    if [ -e "${this_makefile}" ]; then
        echo -ne "    INFO:  Running \"${my_make} -f ${this_makefile} ${processing_verb}\" ... "
        cd "${source_dir}" && ${my_make} -f "${this_makefile}" ${processing_verb} > "${log_file}" 2>&1
        exit_code=${?}

        if [ ${exit_code} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="${processing_verb} processing of targets failed.  See \"${log_file}\" for details"
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
    echo "POST-PROCESSING - Regular Expression Pattern Matching"

    # Look for any regex files located in <folder>
    # where filename indicates the prepared folder in which to operate
    pcTarget_dir="${WB_AUTOMATE}/target"
    postconvert_dir="${WB_AUTOMATE}/param/regex/post_conversion"
    export postconvert_dir
    post_dirs_to_ignore="config data SQL DSNUTILS reload unload"
    find_exclude=$(echo "${post_dirs_to_ignore}" | ${my_sed} 's?\([a-zA-Z0-9]*\)?! -path "*/\1/*"?g')

    for file_extension in ${file_extensions} ; do
        file_ext="${file_extension}"
        target_files=$(cd "${pcTarget_dir}" && ${my_find} . -depth -type f ${find_exclude} | ${my_egrep} "\.${file_ext}$")
        comment_prefix="      *"

#=====
        # Redefine target_files?
        case ${file_ext} in 

            cbl)
                target_files=$(cd "${pcTarget_dir}" && ${my_find} . -depth -type f | ${my_egrep} "\.${file_ext}$|\.pco$")
            ;;

            jcl)
                comment_prefix="#"
                file_ext="ksh"
                target_files=$(cd "${pcTarget_dir}" && ${my_find} . -depth -type f | ${my_egrep} "\.${file_ext}$")
            ;;

        esac
         
#=====

        tmp_dir="/tmp/${USER}/$$"

        if [ ! -d "${tmp_dir}" ]; then
            ${my_mkdir} -p "${tmp_dir}"
        fi

        ########################################################################
        ####
        #### BEGIN Regular expression translation via PERL program processor.pl
        ####
        ########################################################################

        for this_target_file in ${target_files} ; do
            #This target file is: ./Master-copy/COPY/IBBIWCPD.cpy
            echo "This target file is: ${this_target_file}"

            # Break out the file name
            target_file=$(${my_basename} "${this_target_file}")

            # Break out the relative parent directory
            source_code_dir=$(${my_dirname} "${this_target_file}")

            # Get rid of the leading ./
            source_code_dir=$(echo "${source_code_dir} | ${my_sed} -e 's?^\./??g'")

            # Add in the full directory path
            source_code_dir="${pcTarget_dir}/${source_code_dir}"

            # Figure out the uc_target
            uc_target_dir=$(echo "${source_code_dir}" | ${my_awk} -F'/' '{print $NF}')

            # Define the target from uc_target
            target_dir=$(echo "${uc_target_dir}" | ${my_tr} '[A-Z]' '[a-z]')

            # Read in regex lines from "${postconvert_dir}/${uc_target_dir}"
            if [ -e "${postconvert_dir}/${uc_target_dir}" -a -s "${postconvert_dir}/${uc_target_dir}" ]; then
                echo -ne "    INFO:  Post-Processing \"${source_code_dir}/${target_file}\" for regular expression translation ... "
                tmp_file="${tmp_dir}/translate-post-processing-${uc_target_dir}-${target_file}.$$"
                ${my_rm} -f "${tmp_file}"

                ${SCRIPT_BASE}/processor.pl --input_file "${source_code_dir}/${target_file}" --output_file "${tmp_file}" --data_type "${target_dir}" --regex_file "${postconvert_dir}/${uc_target_dir}" --mode "post"

                # Move ${tmp_file} to ${source_code_dir}/${target_file}
                if [ -e "${tmp_file}" -a -s "${tmp_file}" ]; then
                    ${my_diff} -q "${tmp_file}" "${source_code_dir}/${target_file}" > /dev/null 2>&1

                    if [ ${?} -ne ${SUCCESS} ]; then
                        ${my_rsync} "${tmp_file}" "${source_code_dir}/${target_file}" > /dev/null 2>&1
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

            fi

        done

        ########################################################################
        ####
        #### END Regular expression translation via PERL program processor.pl
        ####
        ########################################################################

        #echo "POST-PROCESSING - SQL timestamp conversion"

        if [ "${file_extension}" = "cbl" -o "${file_extension}" = "cpy" ]; then

            ################################################################
            ####
            #### BEGIN IBCABEND keyword translation
            ####
            ################################################################

            echo -ne "            Processing file ${prepared_dir}/${uc_target_dir}/${target_file} for IBCABEND filename translation ... "
            ibcabend_lines=$(${my_egrep} -n -a "\bCOPY\ *IBCABEND\." "${prepared_dir}/${uc_target_dir}/${target_file}" | ${my_strings} | ${my_sed} -e 's?\ ?:ZZqC:?g')

            for ibcabend_line in ${ibcabend_lines} ; do
                ibcabend_line=$(echo "${ibcabend_line}" | ${my_sed} -e 's?:ZZqC:?\ ?g')

                let has_cpy_suffix=$(echo "${ibcabend_line}" | ${my_egrep} -c "IBCABEND\.cpy\.")

                if [ ${has_cpy_suffix} -eq 0 ]; then
                    this_line=$(echo "${ibcabend_line}" | ${my_awk} -F':' '{print $1}')
                    ${my_sed} -i -e "${this_line}s?^\(.*\)\(COPY\) *\(IBCABEND\)\.\(.*\)?\1\\${NL}${cbl_offset}${cbl_offset}\2\ \3.cpy\.\4?g" "${prepared_dir}/${uc_target_dir}/${target_file}"
                fi

            done

            echo "DONE"

            ################################################################
            ####
            #### END IBCABEND keyword translation
            ####
            ################################################################

            ########################################################################
            ####
            #### BEGIN SQL timestamp conversion
            ####
            ########################################################################

           # for this_target_file in ${target_files} ; do
           #     echo -ne "    INFO:  Extra Post-Processing of \"${source_code_dir}/${target_file}\" SQL timestamp conversion ... "

           #     source_code_dir=$(${my_dirname} "${this_target_file}")
           #     target_file=$(${my_basename} "${this_target_file}")
           #     uc_target_dir=$(echo "${source_code_dir} | ${my_awk} -F'/' '{print $1}'")
           #     target_dir=$(echo "${uc_target_dir}" | ${my_tr} '[A-Z]' '[a-z]')

           #     # egrep -n -a "^.*$" /tmp/junk | strings | pcregrep -M -e "\bEXEC\b[[:space:]|\n\d+:]*\bCICS\b[[:space:]|\n\d+:]*\bLINK\b[[:space:]|\n\d+:]*\bPROGRAM\b[[:space:]|\n\d+:]*\(C1MATCHI\)[[:space:]|\n\d+:]*\bCOMMAREA\b[[:space:]|\n\d+:]*\(CODE1\-LINKAGE\-IO\)[[:space:]|\n\d+:]*LENGTH[[:space:]|\n\d+:]*\(LINKAGE\-LENGTH\)[[:space:]|\n\d+:]*\bEND\-EXEC\b\." | sort -rn

           #     for pattern in "MWDB2ORA\.MAKE_TIME" "MWDB2ORA\.TIME2HOST" "MWDB2ORA\.STR2TIME" "MWDB2ORA\.STR2TMS" "MWDB2ORA\.STR2DATE" ; do
           #         echo -ne "            Processing file ${source_code_dir}/${target_file} for ${pattern} keyword translation ... "
           #         matched_lines=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_pcregrep} -M -e "\b${pattern}\b\([[:space:]|\n\d+:]*.*[[:space:]|\n\d+:]*\)" | ${my_sed} -e 's?\ ?:ZZqC:?g' | ${my_sort} -rn)
           #         let matched_line_count=$(echo -ne "${matched_lines}\n" | ${my_wc} -l | ${my_awk} '{print $1}')

           #         start_line_keyword="${pattern}\("
           #         start_line_keyword_ignore=""
           #         end_line_keyword="\)"
           #         end_line_keyword_ignore=""

#====
           #         case ${pattern} in

           #             # Rule #1: MWDB2ORA.MAKE_TIME change to use SYSTIMESTAMP
           #             #${my_sed} -i -e "s?MWDB2ORA.MAKE_TIME(.*)?MNTC_LST_TM = SYSTIMESTAMP?g" "${source_code_dir}/${target_file}"
           #             "MWDB2ORA\.MAKE_TIME")

           #                 if [ ${matched_line_count} -gt 1 ]; then

           #                     for matched_line in ${matched_lines} ; do
           #                         this_line_number=$(echo "${matched_line}" | ${my_awk} -F':' '{print $1}')
           #                         this_real_line=$(echo "${matched_line}" | ${my_sed} -e "s/^${this_line_number}://g")
           #                         let has_keyword=$(echo "${this_real_line}" | ${my_egrep} -c ":\b.*\b\)")
           #                         let has_pattern=$(echo "${this_real_line}" | ${my_egrep} -c ":\b${pattern}\b\)")

           #                         if [ ${has_pattern} -gt 0 ]; then
           #                             ${my_sed} -i -e "${this_line_number}s?^\(.*\)${pattern}(\$?\1MNTC_LST_TM = ?g" "${source_code_dir}/${target_file}"
           #                         fi

           #                         if [ ${has_keyword} -gt 0 ]; then
           #                             ${my_sed} -i -e "${this_line_number}s?^\(.*\):.*)\(.*\)\$?$\1SYSTIMESTAMP\2?g" "${source_code_dir}/${target_file}"
           #                         else
           #                             first_seven_chars=$(echo "${this_real_line} | ${my_cut} -b 1-7")
           #                             ${my_sed} -i -e "${this_line_number}s?^${first_seven_chars}\(.*\)\$?${comment_prefix}\1?g" "${source_code_dir}/${target_file}"
           #                         fi
           #                                             
           #                     done

           #                 elif [ ${is_one_line} -eq 1 ]; then
           #                     ${my_sed} -i -e "s?${pattern}(.*)?MNTC_LST_TM = SYSTIMESTAMP?g" "${source_code_dir}/${target_file}"
           #                 fi

           #             ;;    

           #             # Rule #2: MWDB2ORA.TIME2HOST change to TO_CHAR
           #             #${my_sed} -i -e "s?MWDB2ORA.TIME2HOST(\(.*\))?TO_CHAR(\1,\\${NL}${cbl_offset}'HH24.MI.SS')?g" "${source_code_dir}/${target_file}"
           #             "MWDB2ORA\.TIME2HOST")

           #                 if [ ${matched_line_count} -gt 1 ]; then

           #                     for matched_line in ${matched_lines} ; do
           #                         this_line_number=$(echo "${matched_line}" | ${my_awk} -F':' '{print $1}')
           #                         this_real_line=$(echo "${matched_line}" | ${my_sed} -e "s/^${this_line_number}://g")
           #                         let has_keyword=$(echo "${this_real_line}" | ${my_egrep} -c ":\b.*\b\)")
           #                         let has_pattern=$(echo "${this_real_line}" | ${my_egrep} -c ":\b${pattern}\b\)")

           #                         if [ ${has_pattern} -gt 0 ]; then
           #                             ${my_sed} -i -e "${this_line_number}s?^\(.*\)${pattern}\(.*)\)\$?\1TO_CHAR\2?g" "${source_code_dir}/${target_file}"
           #                         fi

           #                         if [ ${has_keyword} -gt 0 ]; then
           #                             ${my_sed} -i -e "${this_line_number}s?^.*\(:.*)\)\$?${cbl_offset}\1,\\${NL}${cbl_offset}'HH24.MI.SS'?g" "${source_code_dir}/${target_file}"
           #                         else
           #                             first_seven_chars=$(echo "${this_real_line} | ${my_cut} -b 1-7")
           #                             ${my_sed} -i -e "${this_line_number}s?^${first_seven_chars}\(.*\)\$?${comment_prefix}\1?g" "${source_code_dir}/${target_file}"
           #                         fi
           #                                             
           #                     done

           #                 elif [ ${is_one_line} -eq 1 ]; then
           #                     ${my_sed} -i -e "s?${pattern}(\(.*\))?TO_CHAR(\1,\\${NL}${cbl_offset}'HH24.MI.SS')?g" "${source_code_dir}/${target_file}"
           #                 fi

           #             ;;

           #             # Rule #3: MWDB2ORA.STR2TIME change to use TO_TIMESTAMP
           #             #${my_sed} -i -e "s?MWDB2ORA.STR2TIME(\(.*\))?TO_TIMESTAMP(\1,\\${NL}${cbl_offset}'HH24.MI.SS')?g" "${source_code_dir}/${target_file}"
           #             "MWDB2ORA\.STR2TIME")

           #                 if [ ${matched_line_count} -gt 1 ]; then

           #                     for matched_line in ${matched_lines} ; do
           #                         this_line_number=$(echo "${matched_line}" | ${my_awk} -F':' '{print $1}')
           #                         this_real_line=$(echo "${matched_line}" | ${my_sed} -e "s/^${this_line_number}://g")
           #                         let has_keyword=$(echo "${this_real_line}" | ${my_egrep} -c ":\b.*\b\)")
           #                         let has_pattern=$(echo "${this_real_line}" | ${my_egrep} -c ":\b${pattern}\b\)")

           #                         if [ ${has_pattern} -gt 0 ]; then
           #                             ${my_sed} -i -e "${this_line_number}s?^\(.*\)${pattern}\(.*)\)\$?\1TO_TIMESTAMP\2?g" "${source_code_dir}/${target_file}"
           #                         fi

           #                         if [ ${has_keyword} -gt 0 ]; then
           #                             ${my_sed} -i -e "${this_line_number}s?^.*\(:.*)\)\$?${cbl_offset}\1,\\${NL}${cbl_offset}'HH24.MI.SS'?g" "${source_code_dir}/${target_file}"
           #                         else
           #                             first_seven_chars=$(echo "${this_real_line} | ${my_cut} -b 1-7")
           #                             ${my_sed} -i -e "${this_line_number}s?^${first_seven_chars}\(.*\)\$?${comment_prefix}\1?g" "${source_code_dir}/${target_file}"
           #                         fi
           #                                             
           #                     done

           #                 elif [ ${is_one_line} -eq 1 ]; then
           #                     ${my_sed} -i -e "s?${pattern}(\(.*\))?TO_TIMESTAMP(\1,\\${NL}${cbl_offset}'HH24.MI.SS')?g" "${source_code_dir}/${target_file}"
           #                 fi

           #             ;;

           #             # Rule #4: MWDB2ORA.STR2TMS remove
           #             #${my_sed} -i -e "s?MWDB2ORA.STR2TMS(\(.*\))?\1?g" "${source_code_dir}/${target_file}"
           #             "MWDB2ORA\.STR2TMS")
           #                 let matched_line_count=$(echo -ne "${matched_lines}\n" | ${my_wc} -l | ${my_awk} '{print $1}')

           #                 if [ ${matched_line_count} -gt 1 ]; then

           #                     for matched_line in ${matched_lines} ; do
           #                         this_line_number=$(echo "${matched_line}" | ${my_awk} -F':' '{print $1}')
           #                         this_real_line=$(echo "${matched_line}" | ${my_sed} -e "s/^${this_line_number}://g")
           #                         let has_keyword=$(echo "${this_real_line}" | ${my_egrep} -c ":\b.*\b\)")

           #                         if [ ${has_keyword} -gt 0 ]; then
           #                             ${my_sed} -i -e "${this_line_number}s?^.*\(:.*\))\$?${cbl_offset}\1?g" "${source_code_dir}/${target_file}"
           #                         else
           #                             first_seven_chars=$(echo "${this_real_line} | ${my_cut} -b 1-7")
           #                             ${my_sed} -i -e "${this_line_number}s?^${first_seven_chars}\(.*\)\$?${comment_prefix}\1?g" "${source_code_dir}/${target_file}"
           #                         fi
           #                                             
           #                     done

           #                 elif [ ${is_one_line} -eq 1 ]; then
           #                     ${my_sed} -i -e "s?${pattern}(\(.*\))?\1?g" "${source_code_dir}/${target_file}"
           #                 fi

           #             ;;

           #             # Rule #5: MWDB2ORA.STR2DATE change to use TO_CHAR
           #             #${my_sed} -i -e "s?BETWEEN MWDB2ORA.STR2DATE(\(.*\) AND \(.*\))?BETWEEN TO_DATE(\1, 'MM/DD/YYYY')\\${NL}${cbl_offset} AND TO_DATE(\2, 'MM/DD/YYYY')?g" "${source_code_dir}/${target_file}"
           #             #${my_sed} -i -e "s?EXEC SQL SELECT MWDB2ORA.STR2DATE(\(.*\), \(.*\), .*)?EXEC SQL SELECT TO_CHAR(\1, \2,\\${NL}${cbl_offset} 'MM-DD-YYYY')?g" "${source_code_dir}/${target_file}"
           #             "MWDB2ORA\.STR2DATE")
           #                 single_param_matched_lines=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_pcregrep} -M -e "\b${pattern}\b\([^,\)]+\)" | ${my_sed} -e 's?\ ?:ZZqC:?g')
           #                 multi_param_matched_lines=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_pcregrep} -M -e "\b${pattern}\b\([^,\)]+,[^,\)]+,[^,\)]+\)" | ${my_sed} -e 's?\ ?:ZZqC:?g')

           #             ;;

           #         esac
#====

           #     done

           #     echo "DONE"
           # done

            ########################################################################
            ####
            #### END SQL timestamp conversion
            ####
            ########################################################################

        fi

        echo "POST-PROCESSING - IEFBR14 file deletion optimization"

        if [ "${file_extension}" = "jcl" ]; then

            for this_target_file in ${target_files} ; do
                echo -ne "    INFO:  Extra Post-Processing of \"${source_code_dir}/${target_file}\" for MOD,DELETE,DELETE optimization ... "

                source_code_dir=$(${my_dirname} "${this_target_file}")
                target_file=$(${my_basename} "${this_target_file}")
                uc_target_dir=$(echo "${source_code_dir} | ${my_awk} -F'/' '{print $1}'")
                target_dir=$(echo "${uc_target_dir}" | ${my_tr} '[A-Z]' '[a-z]')

                iefbr14_lines=($(${my_egrep} -n -a "^\(|IEFBR14" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B1 "IEFBR14" | ${my_egrep} "^[0-9]*:" | ${my_awk} -F':' '{print $1}'))

                # Blocks of code can be detected as line number pairs
                let line_counter=${#iefbr14_lines[@]}-1

                # Valid inputs occur in pairs: a line beginning with "(", and a line ending with "IEFBR14"
                while [ ${line_counter} -gt 0 ]; do
                    let ending_line_number=${iefbr14_lines[$line_counter]}
                    let line_counter=${line_counter}-1
                    let starting_line_number=${iefbr14_lines[$line_counter]}
                    let line_counter=${line_counter}-1

                    # Comment out ending_line_number
                    ending_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^${ending_line_number}:" | ${my_sed} -e "s/^${ending_line_number}://g")
                    ${my_sed} -i -e "${ending_line_number}s?^${ending_line}\$?#${ending_line}?g" "${source_code_dir}/${target_file}"

                    # Move up one element in the array
                    let next_line_up=${ending_line_number}-1

                    # Munge all lines that match between ${starting_line_number} and ${ending_line_number}
                    while [ ${next_line_up} -gt ${starting_line_number} ]; do

                        # Capture this line
                        line_to_munge=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^${next_line_up}:" | ${my_sed} -e "s/^${next_line_up}://g")
                        let mod_del_del_check=$(echo "${line_to_munge}" | ${my_egrep} -c "m_FileAssign.*MOD,DELETE,DELETE")

                        if [ ${mod_del_del_check} -gt 0 ]; then
                            data_file=$(echo "${line_to_munge}" | ${my_awk} -F'/' '{print "${DATA}/" $NF}')

                            if [ "${data_file}" != "" ]; then
                                ${my_sed} -i -e "${next_line_up}s?^${line_to_munge}\$?${ksh_offset}m_FileExist -r DELSW1 ${data_file}\\${NL}${ksh_offset}if [[ \${DELSW1} = true ]]; then\\${NL}${ksh_offset}   m_FileDelete ${data_file}\\${NL}${ksh_offset}fi?g" "${source_code_dir}/${target_file}"
                            fi

                        fi

                        let next_line_up=${next_line_up}-1
                    done

                done

                echo "DONE"
            done

        fi

        echo "POST-PROCESSING - FTPBATCH conversion"

        if [ "${file_extension}" = "jcl" ]; then

            for this_target_file in ${target_files} ; do
                source_code_dir=$(${my_dirname} "${this_target_file}")
                target_file=$(${my_basename} "${this_target_file}")
                uc_target_dir=$(echo "${source_code_dir} | ${my_awk} -F'/' '{print $1}'")
                target_dir=$(echo "${uc_target_dir}" | ${my_tr} '[A-Z]' '[a-z]')

                let has_ftpbatch=$(${my_egrep} -c -n "m_ProcInclude.*FTPBATCH" "${source_code_dir}/${target_file}")

                # Make sure we have an FTPBATCH section upon which to operate
                if [ ${has_ftpbatch} -gt 0 ]; then
                    echo -ne "    INFO:  Extra Post-Processing of \"${source_code_dir}/${target_file}\" for FTPBATCH conversion ... "
                    # FTPBATCH put or get?
                    let put_block=$(${my_egrep} -c -a "^put " "${source_code_dir}/${target_file}")
                    let get_block=$(${my_egrep} -c -a "^get " "${source_code_dir}/${target_file}")

                    if [ ${put_block} -gt 0 ]; then
                        echo -ne "Found put block ... "
                        line_count=$(${my_wc} -l "${source_code_dir}/${target_file}" | ${my_awk} '{print $1}')
                        let has_cnvtls=$(${my_egrep} -c -a "^\(CNVTLS[0-9]*\)" "${source_code_dir}/${target_file}")

                        # Make sure the file hasn't already been converted for FTPBATCH put operations
                        if [ ${has_cnvtls} -eq 0 ]; then
                            let cnvtls_data_counter=0
                            put_batch_lines=($(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B${line_count} "^[0-9]*:put " | ${my_egrep} "^[0-9]*:.*m_ProcInclude.*\ FTPBATCH" | tail -1 | ${my_awk} -F':' '{print $1}'))
                            let put_batch_line_count=${#put_batch_lines[@]}-1

                            # Start operating on the last FTPBATCH line, so as to not disturb line positions after processing
                            while [ ${put_batch_line_count} -ge 0 ]; do
                                function_start_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B${line_count} "^${put_batch_lines[$put_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:\(" | tail -1)
                                function_start_line_number=$(echo "${function_start_line}" | ${my_awk} -F':' '{print $1}')
                                function_start_line=$(echo "${function_start_line}" | ${my_sed} -e "s/^${function_start_line_number}://g")

                                function_end_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -A${line_count} "^${put_batch_lines[$put_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:_end" | head -1)
                                function_end_line_number=$(echo "${function_end_line}" | ${my_awk} -F':' '{print $1}')
                                function_end_line=$(echo "${function_end_line}" | ${my_sed} -e "s/^${function_end_line_number}://g")

                                jump_label=$(echo "${function_start_line}" | ${my_sed} -e 's/[\(|\)]//g')
                                jump_label_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B${line_count} "^[0-9]*:\(${jump_label}\)$" | ${my_egrep} "^[0-9]*:.*JUMP_LABEL=${jump_label}$" | tail -1)
                                jump_label_line_number=$(echo "${jump_label_line}" | ${my_awk} -F':' '{print $1}')
                                jump_label_line=$(echo "${jump_label_line}" | ${my_sed} -e "s/^${jump_label_line_number}://g")

                                let function_line_counter=${function_start_line_number}+1
                                let ftp_put_counter=0

                                # Figure out all the lines containing DATA file names
                                while [ ${function_line_counter} -lt ${function_end_line_number} ]; do
                                    next_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^${function_line_counter}:" | ${my_sed} -e "s/^${function_line_counter}://g")
                                    let is_data_line=$(echo "${next_line}" | ${my_egrep} -c "m_FileOverride.*\ FTP\ .*{DATA}/")

                                    if [ ${is_data_line} -gt 0  ]; then
                                        data_file[${ftp_put_counter}]=$(echo "${next_line}" | ${my_awk} '{print $NF}')

                                        # Rename the ${}data_file[]} to ${data_file[]}.ftp
                                        ${my_sed} -i -e "${function_line_counter}s?${data_file[$ftp_put_counter]}\$?${data_file[$ftp_put_counter]}.ftp?g" "${source_code_dir}/${target_file}"

                                        let ftp_put_counter=${ftp_put_counter}+1
                                    fi

                                    let function_line_counter=${function_line_counter}+1
                                done

                                cnvtls_line=""
                                let data_line_counter=0

                                while [ ${data_line_counter} -lt ${ftp_put_counter} ]; do

                                    if [ "${cnvtls_line}" = "" ]; then
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

                    if [ ${get_block} -gt 0 ]; then
                        line_count=$(${my_wc} -l "${source_code_dir}/${target_file}" | ${my_awk} '{print $1}')
                        echo -ne "Found get block ... "
                        let has_cnvtrs=$(${my_egrep} -c -a "^\(CNVTRS\)" "${source_code_dir}/${target_file}")

                        # Make sure the file hasn't already been converted for FTPBATCH get operations
                        if [ ${has_cnvtrs} -eq 0 ]; then
                            let cnvtrs_data_counter=0
                            get_batch_lines=($(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B${line_count} "^[0-9]*:get " | ${my_egrep} "^[0-9]*:.*m_ProcInclude.*\ FTPBATCH" | tail -1 | ${my_awk} -F':' '{print $1}'))
                            let get_batch_line_count=${#get_batch_lines[@]}-1

                            # Start operating on the last FTPBATCH line, so as to not disturb line positions after processing
                            while [ ${get_batch_line_count} -ge 0 ]; do
                                function_start_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B${line_count} "^${get_batch_lines[$get_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:\(" | tail -1)
                                function_start_line_number=$(echo "${function_start_line}" | ${my_awk} -F':' '{print $1}')
                                function_start_line=$(echo "${function_start_line}" | ${my_sed} -e "s/^${function_start_line_number}://g")

                                function_end_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -A${line_count} "^${get_batch_lines[$get_batch_line_count]}:.*m_ProcInclude.*\ FTPBATCH" | ${my_egrep} "^[0-9]*:_end" | head -1)
                                function_end_line_number=$(echo "${function_end_line}" | ${my_awk} -F':' '{print $1}')
                                function_end_line=$(echo "${function_end_line}" | ${my_sed} -e "s/^${function_end_line_number}://g")

                                let function_line_counter=${function_start_line_number}+1
                                let ftp_get_counter=0

                                # Figure out all the lines containing DATA file names
                                while [ ${function_line_counter} -lt ${function_end_line_number} ]; do
                                    next_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^${function_line_counter}:" | ${my_sed} -e "s/^${function_line_counter}://g")
                                    let is_data_line=$(echo "${next_line}" | ${my_egrep} -c "m_FileOverride.*\ FTP\ .*{DATA}/")
                                    let is_get_line=$(echo "${next_line}" | ${my_egrep} -c "^get")

                                    if [ ${is_data_line} -gt 0  ]; then
                                        data_file[${ftp_get_counter}]=$(echo "${next_line}" | ${my_awk} '{print $NF}')

                                        # Rename the ${}data_file[]} to ${data_file[]}.ftp
                                        ${my_sed} -i -e "${function_line_counter}s?${data_file[$ftp_get_counter]}\$?${data_file[$ftp_get_counter]}.ftp?g" "${source_code_dir}/${target_file}"

                                        let ftp_get_counter=${ftp_get_counter}+1
                                    fi

                                    if [ ${is_get_line} -gt 0  ]; then
                                        ${my_sed} -i -e "${function_line_counter}s?^get.*\$?${next_line} \[REPLACE\]?g" "${source_code_dir}/${target_file}"
                                    fi

                                    let function_line_counter=${function_line_counter}+1
                                done

                                cnvtrs_line=""
                                let data_line_counter=0

                                while [ ${data_line_counter} -lt ${ftp_get_counter} ]; do

                                    if [ "${cnvtrs_line}" = "" ]; then
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

                    # Comment out locsite lines
                    locsite_lines=$(${my_egrep} -n -a "^.*locsite" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -v "#.*locsite")

                    for locsite_line in ${locsite_lines} ; do
                        this_line_number=$(echo "${locsite_line}" | ${my_awk} -F':' '{print $1}')
                        echo -ne "    INFO:  Extra Post-Processing of \"${source_code_dir}/${target_file}\" for LOCSITE removal ... "
                        ${my_sed} -i -e "${this_line_number}s?^\(.*locsite.*\)\$?#\1?g" "${source_code_dir}/${target_file}"
                    done

                    echo "DONE"
                fi

            done

        fi

        echo "POST-PROCESSING - DFDSS conversion"

        if [ "${file_extension}" = "jcl" ]; then

            for this_target_file in ${target_files} ; do
                source_code_dir=$(${my_dirname} "${this_target_file}")
                target_file=$(${my_basename} "${this_target_file}")
                uc_target_dir=$(echo "${source_code_dir} | ${my_awk} -F'/' '{print $1}'")
                target_dir=$(echo "${uc_target_dir}" | ${my_tr} '[A-Z]' '[a-z]')

                let has_dfdss=$(${my_egrep} -c -a "m_ProcInclude.*DFDSS\ " "${source_code_dir}/${target_file}")

                # Make sure we have an DFDSS section upon which to operate
                if [ ${has_dfdss} -gt 0  ]; then
                    echo "TAR Conversion ... coming soon"
                fi

            done

        fi

        echo "POST-PROCESSING - SMTP conversion"

        if [ "${file_extension}" = "jcl" ]; then

            for this_target_file in ${target_files} ; do
                source_code_dir=$(${my_dirname} "${this_target_file}")
                target_file=$(${my_basename} "${this_target_file}")
                uc_target_dir=$(echo "${source_code_dir} | ${my_awk} -F'/' '{print $1}'")
                target_dir=$(echo "${uc_target_dir}" | ${my_tr} '[A-Z]' '[a-z]')

                let has_smtp=$(${my_egrep} -c -a "m_OutputAssign.*\ SMTP2\ " "${source_code_dir}/${target_file}")

                # Make sure we have an SMTP section upon which to operate
                if [ ${has_smtp} -gt 0  ]; then
                    echo -ne "    INFO:  Extra Post-Processing of \"${source_code_dir}/${target_file}\" for SMTP conversion ... "
                    line_count=$(${my_wc} -l "${source_code_dir}/${target_file}" | ${my_awk} '{print $1}')
                    let has_cnvsmtp=$(${my_egrep} -c -a "^\(CNVSMTP[0-9]*\)" "${source_code_dir}/${target_file}")

                    # Make sure the file hasn't already been converted for SMTP operations
                    if [ ${has_cnvsmtp} -eq 0 ]; then
                        let cnvsmtp_data_counter=0
                        smtp2_lines=($(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^[0-9]*:.*m_OutputAssign.*\ SMTP2\ " | ${my_awk} -F':' '{print $1}'))
                        let smtp2_line_count=${#smtp2_lines[@]}-1

                        # Start operating on the last SMTP2 line, so as to not disturb line positions during processing
                        while [ ${smtp2_line_count} -ge 0 ]; do
                            function_start_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B${line_count} "^${smtp2_lines[$smtp2_line_count]}:.*m_OutputAssign.*\ SMTP2\ " | ${my_egrep} "^[0-9]*:\(" | tail -1)
                            function_start_line_number=$(echo "${function_start_line}" | ${my_awk} -F':' '{print $1}')
                            function_start_line=$(echo "${function_start_line}" | ${my_sed} -e "s/^${function_start_line_number}://g")

                            function_end_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -A${line_count} "^${smtp2_lines[$smtp2_line_count]}:.*m_OutputAssign.*\ SMTP2\ " | ${my_egrep} "^[0-9]*:\ *;;$" | head -1)
                            function_end_line_number=$(echo "${function_end_line}" | ${my_awk} -F':' '{print $1}')
                            function_end_line=$(echo "${function_end_line}" | ${my_sed} -e "s/^${function_end_line_number}://g")

                            jump_label=$(echo "${function_start_line}" | ${my_sed} -e 's/[\(|\)]//g')
                            jump_label_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -B${line_count} "^[0-9]*:\(${jump_label}\)$" | ${my_egrep} "^[0-9]*:.*JUMP_LABEL=${jump_label}$" | tail -1)
                            jump_label_line_number=$(echo "${jump_label_line}" | ${my_awk} -F':' '{print $1}')
                            jump_label_line=$(echo "${jump_label_line}" | ${my_sed} -e "s/^${jump_label_line_number}://g")

                            filerepro_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} -A${line_count} "^${smtp2_lines[$smtp2_line_count]}:.*m_OutputAssign.*\ SMTP2\ " | ${my_egrep} "^[0-9]*:.*m_FileRepro" | head -1)
                            filerepro_line_number=$(echo "${filerepro_line}" | ${my_awk} -F':' '{print $1}')
                            filerepro_line=$(echo "${filerepro_line}" | ${my_sed} -e "s/^${filerepro_line_number}://g")

                            # Now we munge the filepro line
                            ${my_sed} -i -e "${filerepro_line_number}s?^${filerepro_line}\$?${ksh_offset}m_Smtp -i SYSUT1?g" "${source_code_dir}/${target_file}"

                            # Now we comment out the SMTP2 line
                            ${my_sed} -i -e "${smtp2_lines[$smtp2_line_count]}s?\(^.*$\)?${comment_prefix}\1?g" "${source_code_dir}/${target_file}"

                            let function_line_counter=${function_start_line_number}+1
                            let smtp_counter=0

                            # Figure out all the lines containing DATA file names
                            while [ ${function_line_counter} -lt ${function_end_line_number} ]; do
                                next_line=$(${my_egrep} -n -a "^.*$" "${source_code_dir}/${target_file}" | ${my_strings} | ${my_egrep} "^${function_line_counter}:" | ${my_sed} -e "s/^${function_line_counter}://g")
                                let is_data_line=$(echo "${next_line}" | ${my_egrep} -c "m_FileAssign.*\ *[{DATA}{TMP}]/")

                                if [ ${is_data_line} -gt 0  ]; then
                                    data_file[${smtp_counter}]=$(echo "${next_line}" | ${my_awk} '{print $NF}')

                                    # Rename the ${}data_file[]} to ${data_file[]}.ls
                                    ${my_sed} -i -e "${function_line_counter}s?${data_file[$smtp_counter]}\$?${data_file[$smtp_counter]}.ls?g" "${source_code_dir}/${target_file}"

                                    let smtp_counter=${smtp_counter}+1
                                fi

                                let function_line_counter=${function_line_counter}+1
                            done

                            cnvsmtp_line=""
                            let data_line_counter=0

                            while [ ${data_line_counter} -lt ${smtp_counter} ]; do

                                if [ "${cnvsmtp_line}" = "" ]; then
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

                    echo "DONE"
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
