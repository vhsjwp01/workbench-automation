#!/bin/bash
#set -x

################################################################################
#                      S C R I P T    D E F I N I T I O N
################################################################################
#

#-------------------------------------------------------------------------------
# Revision History
#-------------------------------------------------------------------------------
# 20150708     Jason W. Plummer          Original: A generic script to run a 
#                                        rehost conversion process in as 
#                                        automated a way as possible

################################################################################
# DESCRIPTION
################################################################################
#

# NAME: convert.sh
# 
# This script performs a choreographed series of make targets from:
#
#     migrate_entry.pl from ART workbench
#
# OPTIONS:
#
# --project_dir        - The fully qualified path to a convert project
# --workbench_dir      - The fully qualified path to an ART workbench 
#                        installation
#

################################################################################
# TODO
################################################################################
#
# 1) de-mystify ${PROJECT_DIR}/scripts/migrate.pl
# 2) everything else remaining

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
USAGE="${USAGE}[ --project_dir <fully qualified path to a directory containing a makefile *REQUIRED*> ]${USAGE_ENDLINE}"
USAGE="${USAGE}[ --workbench_dir <fully qualified path to a directory containing a makefile *REQUIRED*> ]"

MAKE_TARGETS="FileConvert RdbmsConvert cobolConvert trad_jcl"

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
    my_command=`echo "${my_command}" | sed -e 's/\-/_/g'`

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

################################################################################
# MAIN
################################################################################
#


# WHAT: Make sure we have some useful commands
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    for command in awk cd egrep make ls sed uname wc ; do
        unalias ${command} > /dev/null 2>&1
        f__check_command "${command}"

        if [ ${?} -ne ${SUCCESS} ]; then
            let exit_code=${exit_code}+1
        fi

    done

fi

# WHAT: Make sure we are on the right platform
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    this_os=`${my_uname} -s 2> /dev/null`
    this_arch=`${my_uname} -m 2> /dev/null | ${my_sed} -e 's/i[3456]86/i386/g'`
    this_os_arch="${this_os}_${this_arch}"

    case ${this_os_arch} in

        Linux_x86_64)
            export REFINEDISTRIB="Linux64"
        ;;

        Linux_i386)
            export REFINEDISTRIB="Linux32"
        ;;

        *)
            export REFINEDISTRIB="unknown"
        ;;

    esac

    if [ "${REFINEDISTRIB}" = "unknown" ]; then
        err_msg="Unsupported OS platform \"${this_os_arch}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Make sure the ${my_make} we found is GNU make
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    let is_gnu_make=`${my_make} -version 2>&1 | ${my_egrep} -c "GNU Make"`

    if [ ${is_gnu_make} -ne 1 ]; then
        err_msg="Please install GNU make"
        exit_code=${ERROR}
    fi

fi

# WHAT: Make sure we have necessary arguments
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    while (( "${#}" )); do
        key=`echo "${1}" | ${my_sed} -e 's?\`??g'`
        value=`echo "${2}" | ${my_sed} -e 's?\`??g'`

        case "${key}" in

            --project_dir|--workbench_dir)
                key=`echo "${key}" | ${my_sed} -e 's?^--??g'`

                if [ "${value}" != "" ]; then
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

    if [ "${project_dir}" = "" ]; then
        err_msg="No source directory argument was passed"
        exit_code=${ERROR}
    fi

    if [ "${workbench_dir}" = "" ]; then
        err_msg="No ART workbench directory argument was passed"
        exit_code=${ERROR}
    fi

fi

# WHAT: Make sure ART workbench directory exists
# WHY:  Cannot continue otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    if [ ! -d "${workbench_dir}" ]; then
        err_msg="ART workbench directory \"${workbench_dir}\" does not exist"
        exit_code=${ERROR}
    else
        export WBDIR="${workbench_dir}"
        export REFINEDIR="${workbench_dir}/refine"
        export PHOENIX="${workbench_dir}/refine"
        export CLASSPATH="${workbench_dir}/refine/scripts/mbcs/MBCSConvert.jar"
    fi

fi

# WHAT: Make sure needed directories exist
# WHY:  Cannot continue otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    if [ ! -d "${project_dir}" ]; then
        err_msg="Source directory \"${project_dir}\" does not exist"
        exit_code=${ERROR}
    else
        export PROJECT="${project_dir}"

        # WHAT: Make sure the appropriate sub directories exist
        # WHY:  Needed for processing later
        #
        sub_dirs="logs source param tmp"

        for sub_dir in ${sub_dirs} ; do

            if [ -d "${PROJECT}/${sub_dir}" ]; then case ${sub_dir} in 
                    logs)
                        export TRAVAIL="${PROJECT}/${sub_dir}"
                        export LOGS="${PROJECT}/${sub_dir}"
                    ;;

                    source)
                        export SOURCE="${PROJECT}/${sub_dir}"
                    ;;

                    param)
                        export PARAM="${PROJECT}/${sub_dir}"
                    ;;

                    tmp)
                        export TMPPROJECT="${PROJECT}/${sub_dir}"
                    ;; 

                esac

            fi

        done

        ${my_cd} "${project_dir}"
    fi

fi

# WHAT: Make sure we have a makefile in the ${project_dir}/${source} directory
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then
    let makefile_check=`${my_ls} [Mm]akefile 2> /dev/null | ${my_wc} -l | ${my_awk} '{print $1}'`

    if [ ${makefile_check} -ne 1 ]; then
        err_msg="Could not locate a makefile in source directory \"${project_dir}\""
        exit_code=${ERROR}
    else
        this_makefile=`${my_ls} [Mm]akefile`
    fi

fi

# WHAT: Make sure our ${MAKE_TARGETS} exist in ${this_makefile}
# WHY:  Cannot proceed otherwise
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    for make_target in ${MAKE_TARGETS} ; do
        let target_check=`${my_egrep} -c "^${make_target}:" "${this_makefile}" `

        if [ ${target_check} -ne 1 ]; then
            echo "    WARNING:  Make directive \"${make_target}\" not present in make file \"${project_dir}/${this_makefile}\""
            let exit_code=${exit_code}+1
        fi

    done

    if [ ${exit_code} -ne ${SUCCESS} ]; then
        err_msg="Failed to locate all needed make targets in \"${project_dir}/${this_makefile}\""
        exit_code=${ERROR}
    fi

fi

# WHAT: Just do it
# WHY:  If we get here, then we have all the needed pieces
#
if [ ${exit_code} -eq ${SUCCESS} ]; then

    for make_target in ${MAKE_TARGETS} ; do
        echo -ne "Make target \"${make_target}\" ... "
        ${my_cd} "${project_dir}" && ${my_make} ${make_target}

        if [ ${?} -ne ${SUCCESS} ]; then
            echo "FAILED"
            err_msg="Make directive \"${make_target}\" FAILED"
            let exit_code=${exit_code}+${?}
            break
        else
            echo "SUCCESS"
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
