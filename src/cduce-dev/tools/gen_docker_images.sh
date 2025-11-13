#!/bin/sh
BASE_IMAGE="ocaml-cduce"

if [ "x$1" = "x--rebuild" ];
then
    REBUILD="rebuild"
else
    REBUILD=""
fi


SCRIPT_PATH=$(dirname "$0")
PACKAGES="$("$SCRIPT_PATH"/init_opam_switch.sh --print-deps) zarith zarith_stubs_js"
USER_ID="$(id -u)"
NOCACHE="--no-cache"
for f in `cat ${SCRIPT_PATH}/Dockerfile | grep COPY | cut -f2 -d ' '`
do
    if test -f "${SCRIPT_PATH}/$f"
    then
	continue
    else
	echo "Cannot find local file $f (are you running this script on the CI machine ?)"
	exit 2
    fi
done

"$SCRIPT_PATH"/gen_gitlab_ci.sh  | grep ocaml-cduce | cut -f 3 -d : | sort -u | while read OCAML_VERSION;
do
    echo "BUILDING IMAGE FOR ${OCAML_VERSION}"
    IMAGE="${BASE_IMAGE}:${OCAML_VERSION}"
    IMAGE_ID="$(docker images -q "${IMAGE}")"
    if [ "x${IMAGE_ID}" = "x" -o "x$REBUILD" = "xrebuild" ];
    then
	case "$OCAML_VERSION" in
	    5*)
		P=`echo $PACKAGES | sed -e 's/ocamlnet\|pxp//g'`
		;;
	    *)
		P="${PACKAGES}"
		;;
	esac
	echo $P
	docker build $NOCACHE --rm=false -t "${IMAGE}" \
	    --build-arg="user_id=${USER_ID}" \
            --build-arg="ocaml_version=${OCAML_VERSION}" \
            --build-arg="packages=${P}" "$SCRIPT_PATH"
	NOCACHE=""
    else
	echo "SKIPING IMAGE ${OCAML_VERSION}"
    fi
done
