########################################################-*-Shell-script-*-####
# Mirror Function
##############################################################################
# $Id$

source ${0%/*}/install.sh;

##############################################################################
# mirror

# usage: mirror [-C prefix] [install-options] [files] dstdir
# first cds into <prefix>, then mirrors <files> into <dstdir>

function mirror-args() {
    case "$1" in
        --version             ) echo "install (bash script) $version"; exit 0;;
        -C                    ) args-option-value prefix "$@";;
        -o                    ) args-option-value owner  "$@";;
        --owner=*             ) args-inline-value owner  "$@";;
        -g                    ) args-option-value group  "$@";;
        --group=*             ) args-inline-value group  "$@";;
        -m                    ) args-option-value mode   "$@";;
        --mode=*              ) args-inline-value mode   "$@";;
        -p                    ) preserve="true"; return 1;;
        --preserve-timestamps ) preserve="true"; return 1;;
        -*                    ) args-option-unknown      "$@";;
        *                     ) args-append-array files  "$@";;
    esac;
}

function mirror-files() {
    local prefix="$1"; shift 1;
    local dstpath="$1"; shift 1;
    while [ $# -gt 0 ]; do
        local srcfile="$1"; shift 1;
        local dstfile="$dstpath/$srcfile";
        install "${instargs[@]}" "$prefix/$srcfile" "$dstfile";
    done;
}

function mirror() {
    local version='$Revision$';
    local prefix=".";
    local owner="";
    local group="";
    local mode="";
    local preserve="false";
    local -a files;

    args-loop "$FUNCNAME" "$@";

    if [ ! -d "$prefix" ]; then
        abort "$FUNCNAME: could not find directory \`$prefix'";
    fi;

    local count="${#files[@]}";
    if [ $count -lt 1 ]; then
        abort "$FUNCNAME: missing target directory";
    fi;

    local -a instargs;
    instargs[${#instargs[@]}]="-D";
    if [ -n "$owner" ]; then
        instargs[${#instargs[@]}]="--owner=$owner";
    fi;
    if [ -n "$group" ]; then
        instargs[${#instargs[@]}]="--group=$group";
    fi;
    if [ -n "$mode" ]; then
        instargs[${#instargs[@]}]="--mode=$mode";
    fi;
    if [ "$preserve" == "true" ]; then
        instargs[${#instargs[@]}]="--preserve-timestamps";
    fi;

    local dstpath="${files[$(($count-1))]}";
    unset files[$(($count-1))];
    mirror-files "$prefix" "$dstpath" "${files[@]}";
}

##############################################################################
