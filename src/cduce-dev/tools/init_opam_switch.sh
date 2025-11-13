#!/bin/sh

#This script is used to initialize the current opam switch with
#the build dependecies

PACKAGES="cduce-types cduce cduce-js cduce-tools"

case "$(uname -s)" in
    Darwin*)
        SED_ERE_FLAG="-E"
        SED_PIPE="|"
        ;;
    *)
        SED_ERE_FLAG=""
        SED_PIPE="\|"
        ;;
esac

case "$(uname -s)" in
    CYGWIN*)
        CYGPATH=`which cygpath.exe`
        if test -z "$CYGPATH"; then
            echo "Error: running under Cygwin but cannot find cygpath.exe "
            exit 2
        fi
        FIXPATH="${CYGPATH} -w "
        ;;
    *)
        FIXPATH="echo"
        ;;
esac

DO_INSTALL=""
DO_PRINT=""
DO_MIN=""

for i in "$@"
do
    if test "$i" = "--install"; then
        DO_INSTALL=1
    elif test "$i" = "--print-deps"; then
        DO_PRINT=1
    elif test "$i" = "--min-version"; then
        DO_MIN=1
    elif test "$i" = "--help"; then
        DO_INSTALL=""
        DO_PRINT=""
        break
    fi
done

if test -z "$DO_INSTALL" -a -z "$DO_PRINT"; then
    echo "Usage: $0 [options]"
    echo
    echo "options:"
    echo "  --install     install the developpment dependencies via opam"
    echo "  --print-deps  print the development dependencies"
    echo "  --min-version select the minimal version of dependencies"
    echo "  --help        display this message"
    exit 0
fi

##

OPAM=`which opam`
SCRIPT_PATH=`dirname $0`
BASE_PATH="$(cd ${SCRIPT_PATH}/..; pwd)"
if test -z "$OPAM" ;
then
    echo "Error: cannot find opam in PATH, is it installed ?"
    exit 1
fi
OPAM_VERSION=`$OPAM --version`
case "$OPAM_VERSION" in
2.0*)
    FILE_OPT="--file="
    ;;
*)
    FILE_OPT="--just-file "
    ;;
esac

DEPS=""
EXCLUDE_RE=""
for p in $PACKAGES
do
    EXCLUDE_RE="${p}"' *{[^}]*}'"${SED_PIPE}${EXCLUDE_RE}"
done 

if test "$DO_MIN"; then
    CLEAN_RE='s/ *{[^}]*= *\([^ =}]\+\) *} */.\1 /g'
else
    CLEAN_RE='s/ *{[^}]*} */ /g'
fi
for p in $PACKAGES
do
    OFILE="${BASE_PATH}/${p}.opam"
    if test -f "$OFILE";
    then
        OFILE="$(${FIXPATH} "${OFILE}")"
        RAW_DEPS="$(opam show -f depopts,depends ${FILE_OPT}${OFILE} | sed -e 's/"//g')"
        RAW_DEPS_NO_CDUCE="$(echo ${RAW_DEPS} | sed ${SED_ERE_FLAG} -e 's/'"${EXCLUDE_RE}"'^ *'"${SED_PIPE}"'depopts: '"${SED_PIPE}"'depends: //g' )"
        CLEANED_DEPS="$(echo ${RAW_DEPS_NO_CDUCE} | sed -e "${CLEAN_RE}" )"
        DEPS="${DEPS} ${CLEANED_DEPS}"
    else
        echo "Error: cannot find opam file ${p}.opam in $(cd ${SCRIPT_PATH}/..; pwd)"
        exit 2
    fi
done
DEPS="$(printf "%s\n" ${DEPS} | sort -u | xargs echo)"
if test "$DO_PRINT"; then
    echo "$DEPS"
fi
if test "$DO_INSTALL"; then
"$OPAM" install -y $DEPS
fi
