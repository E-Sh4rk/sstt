#!/bin/sh

ACTION="$1"
REMOTE_USER="$2"
REMOTE_HOST="$3"
REMOTE_DIR="$4"
COMMIT="$5"

REMOTE="${REMOTE_USER}@${REMOTE_HOST}"
FULL_DIR="${REMOTE_DIR}/${COMMIT}"

SSH_OPTS="-o LogLevel=ERROR -o StrictHostKeyChecking=no"
usage() {
   echo "Usage: $0 <push|pull|delete> <remote> <version> <tag>"
}

if test -z "$ACTION"
then
    usage
    exit 1
fi

case "$ACTION" in
    push)
        if test -d _build/
        then
            echo -n "Compressing build directory ... "
            tar czf _build.tar.gz _build/
            echo ok
            echo -n "Uploading cache ... "
            ssh ${SSH_OPTS} "${REMOTE}" mkdir -p "${FULL_DIR}"
            scp ${SSH_OPTS} _build.tar.gz "${REMOTE}:${FULL_DIR}"
            rm _build.tar.gz
            echo ok
        else
            echo "_build directory does not exist"
            exit 3
        fi
         ;;
    pull)
        echo -n "Fetching cache ... "
        scp ${SSH_OPTS} "${REMOTE}:${FULL_DIR}/_build.tar.gz" . >/dev/null 2>&1 && echo ok || echo missing
        if test -f _build.tar.gz
        then
            echo -n "Decompressing build directory ... "
            tar xf _build.tar.gz
            rm -f _build.tar.gz
            echo ok
        fi
         ;;
    delete)
        ssh ${SSH_OPTS} "${REMOTE}" rm -rf "${FULL_DIR}"
         ;;
    *)
        usage
        exit 2
        ;;
esac
