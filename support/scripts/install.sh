########################################################-*-Shell-script-*-####
# Install Function
##############################################################################
# $Id$

source ${0%/*}/stdlib.sh;

##############################################################################
# install

function install-args() {
    case "$1" in
        --version   ) echo "install (bash script) $version"; exit 0;;
        -d          ) directory="true"; return 1;;
        --directory ) directory="true"; return 1;;
        -D          ) leading="true"; return 1;;
        -o          ) args-option-value owner "$@";;
        --owner=*   ) args-inline-value owner "$@";;
        -g          ) args-option-value group "$@";;
        --group=*   ) args-inline-value group "$@";;
        -m          ) args-option-value mode  "$@";;
        --mode=*    ) args-inline-value mode  "$@";;
        -*          ) args-option-unknown     "$@";;
        *           ) args-append-array files "$@";;
    esac;
}

function install-mkdir() {
    local dstpath="$1"; shift 1;
    if [ "$dstpath" != "." ]; then
        run mkdir -p "$dstpath";
    fi;
}

function install-copy() {
    local srcfile="$1"; shift 1;
    local dstfile="$1"; shift 1;
    run cp "$srcfile" "$dstfile";
}

function install-attr() {
    local file="$1";
    if [ -n "$mode" ]; then
        run chmod "$mode" "$file";
    fi;
    if [ -n "$owner" -a -n "$group" ]; then
        run chmod "$owner:$group" "$file";
    elif [ -n "$owner" ]; then
        run chmod "$owner" "$file";
    elif [ -n "$group" ]; then
        run chmod "$USER:$group" "$file";
    fi;
}

function install-dirs() {
    while [ $# -gt 0 ]; do
        local dstpath="$1"; shift 1;
        install-mkdir "$dstpath";
        install-attr "$dstpath";
    done;
}

function install-file() {
    local srcfile="$1"; shift 1;
    local dstfile="$1"; shift 1;
    local dstpath="`dirname "$dstfile"`";
    if [ "$leading" == "true" ]; then
        install-mkdir "$dstpath";
    fi;
    install-copy "$srcfile" "$dstfile";
    install-attr "$dstfile";
}

function install-files() {
    local dstpath="$1"; shift 1;
    while [ $# -gt 0 ]; do
        local srcfile="$1"; shift 1;
        local dstfile="$dstpath/`basename "$srcfile"`";
        install-copy "$srcfile" "$dstfile";
        install-attr "$dstfile";
    done;
}

function install() {
    local version="1.00";
    local directory="false";
    local leading="false";
    local owner="";
    local group="";
    local mode="";
    local -a files;

    args-loop "$FUNCNAME" "$@";

    local count="${#files[@]}";
    if [ $count -lt 1 ]; then
        if [ "$directory" == "true" ]; then
            abort "$FUNCNAME: missing target directory";
        else
            abort "$FUNCNAME: missing source file";
        fi;
    fi;

    if [ "$directory" == "true" ]; then
        install-dirs "${files[@]}";
    else
        if [ $count -lt 2 ]; then
            abort "$FUNCNAME: missing destination";
        fi;

        local last="${files[$(($count-1))]}";
        if [ -d "$last" ]; then
            unset files[$(($count-1))];
            install-files "$last" "${files[@]}";
        elif [ $count -eq 2 ]; then
            install-file "${files[@]}";
        else
            local text1="$FUNCNAME: installing multiple files, but last";
            local text2="argument, \`$last' is not a directory";
            abort "$text1 $text2";
        fi;
    fi;
}

##############################################################################
