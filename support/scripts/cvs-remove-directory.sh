########################################################-*-Shell-script-*-####
# CVS-Remove-Directory Function
##############################################################################
# $Id$

source ${0%/*}/stdlib.sh;

##############################################################################
# cvs-move-directory

function cvs-remove-directory-usage() {
    echo "Usage: $program <source-directory>";
}

function cvs-remove-directory-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --verbose             ) verbose=true; return 1;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function cvs-remove-directory() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local -a args;
    args-loop "$@";

    # get source and destination directories
    [ ${#args[@]} == 1 ] || { $program-usage 1>&2; exit 1; };
    local src_dir="${args[0]}";

    if [ ! -d "$src_dir" ]; then
        abort "could not find source directory '$src_dir'";
    fi;
    if [ ! -d "$src_dir/CVS" ]; then
        abort "source directory '$src_dir' is not cvs-controlled";
    fi;

    local src_name=`basename "$src_dir"`;
    local src_parent=`dirname "$src_dir"`;
    local src_entries="$src_parent/CVS/Entries";

    if [ ! -f "$src_entries" ]; then
        abort "could not find source entry file '$src_entries'";
    fi;

    runO "$src_entries~" grep -v "D/$src_name////" "$src_entries";
    run mv "$src_entries~" "$src_entries";
}

##############################################################################
