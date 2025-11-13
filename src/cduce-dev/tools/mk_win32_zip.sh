#!/bin/sh

DLLS="libcurl-4.dll libeay32.dll libexpat-1.dll libnghttp2-14.dll libssh2-1.dll ssleay32.dll zlib1.dll libzstd-1.dll"

if ! test -f dune-project; then
    echo "This script must be launched from the top-level directory"
    exit 1
fi

VERSION="$(sed -n 's:.*(version \(.*\)).*:\1:p' dune-project)"
ARCH="$(uname -m)"
DEST=cduce-"$VERSION"-"$ARCH"
rm -rf "$DEST" "$DEST".zip

clean_up () {    
    if test "${DO_PRINT}";
    then
        echo "${DO_PRINT}"
    fi
    rm -rf "$DEST" "$DEST".zip
    exit 1
}

case "$(uname -s)" in
    CYGWIN*)
        ;;
    *)
        echo "This script must run under Cygwin"
        exit 1
        ;;
esac

ZIP="$(which zip 2>/dev/null)"
if test "$ZIP"; then
    ZIP_PRE="$ZIP -r "
    ZIP_POST=""
else
    ZIP="$(which powershell.exe 2>/dev/null)"
    if test "$ZIP"; then
        ZIP_PRE="$ZIP Compress-Archive -DestinationPath"
        ZIP_POST="-Path"
    else
        echo "Please install zip or powershell.exe to create the archive"
        exit 1
    fi
fi
echo -n "Building CDuce ... "
DO_PRINT="failed"
dune build --display=quiet
DO_PRINT=""
echo "ok"

echo -n "Building archive ${DEST}.zip ... "
DO_PRINT="failed"
mkdir "$DEST"
cp -a win32/oss-licenses.txt "$DEST" && \
cp -a win32/LICENSES "$DEST" && \
cp -a LICENSE "$DEST" && \
cp -a _build/install/default/bin/*.exe "$DEST" && \
for lib in $DLLS; do
    dll_file="$(ldd _build/install/default/bin/cduce.exe | sed -n 's:^.*'"$lib"' => \(.*\) (.*$:\1:p')"
    if test -f "${dll_file}"; then
        cp -a "$dll_file" "$DEST"
    fi
done && \
$ZIP_PRE "$DEST".zip $ZIP_POST "$DEST" && \
rm -rf "$DEST" && \
echo "ok"
DO_PRINT=""