########################################################-*-Shell-script-*-####
# CVS-Move-Directory Function
##############################################################################
# $Id$

source ${0%/*}/stdlib.sh;

##############################################################################
# cvs-move-directory

function cvs-move-directory-usage() {
    echo "Usage: $program <source-directory> <destination-directory>";
}

function cvs-move-directory-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --verbose             ) verbose=true; return 1;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function cvs-move-directory-update-repository() {
    [ $# = 2 -a -d "$1" -a -d "$1/CVS" ] || abort "internal error";
    local directory="$1"; shift 1;
    local repository="$1"; shift 1;

    runO "$directory/CVS/Repository" echo "$repository";

    local name;
    for name in `ls -A "$directory"`; do
        local file="$directory/$name";
        if [ -d "$file" -a -d "$file/CVS" ]; then
            cvs-move-directory-update-repository "$file" "$repository/$name";
        fi;
    done;
}

function cvs-move-directory() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local -a args;
    args-loop "$@";

    # get source and destination directories
    [ ${#args[@]} == 2 ] || { $program-usage 1>&2; exit 1; };
    local src_dir="${args[0]}";
    local dst_dir="${args[1]}";

    if [ ! -d "$src_dir" ]; then
        abort "could not find source directory '$src_dir'";
    fi;
    if [ ! -d "$src_dir/CVS" ]; then
        abort "source directory '$src_dir' is not cvs-controlled";
    fi;
    if [ -d "$dst_dir" ]; then
        abort "destination directory '$dst_dir' exists already";
    fi;

    local src_name=`basename "$src_dir"`;
    local dst_name=`basename "$dst_dir"`;
    local src_parent=`dirname "$src_dir"`;
    local dst_parent=`dirname "$dst_dir"`;
    local src_entries="$src_parent/CVS/Entries";
    local dst_entries="$dst_parent/CVS/Entries";
    local dst_repository="$dst_parent/CVS/Repository";

    if [ ! -f "$src_entries" ]; then
        abort "could not find source entry file '$src_entries'";
    fi;
    if [ ! -f "$dst_entries" ]; then
        abort "could not find destination entry file '$dst_entries'";
    fi;
    if [ ! -f "$dst_repository" ]; then
        abort "could not find destination repository file '$dst_repository'";
    fi;

    run mv "$src_dir" "$dst_dir";
    runO "$src_entries~" grep -v "D/$src_name////" "$src_entries";
    run mv "$src_entries~" "$src_entries";
    runOO "$dst_entries" echo "D/$dst_name////";

    cvs-move-directory-update-repository "$dst_dir" \
        `head -1 $dst_repository`/"$dst_name";
}

##############################################################################
