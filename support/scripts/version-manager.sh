########################################################-*-Shell-script-*-####
# Version-Manager Function
##############################################################################
# $Id$

source ${0%/*}/stdlib.sh;

##############################################################################
# version-manager

function version-manager-usage() {
    echo "Usage: $program <version-file> update";
    echo "       $program <version-file> increment";
    echo "       $program <version-file> set <version-value>";
}

function version-manager-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --verbose             ) verbose=true; return 1;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function version-manager-compose() {
    [ $# = 5 ] || abort "internal error";
    local variable="$1"; shift 1;
    local value="$1.$2.$3.$4"; shift 4;
    eval "$variable=\"\$value\"";
}

function version-manager-decompose() {
    [ $# = 2 ] || abort "internal error";
    local array="$1"; shift 1;
    local value="$1"; shift 1;
    local v0=`expr "$value" : '\([0-9]*\)\.[0-9]*\.[0-9]*\.[0-9]*$'`;
    local v1=`expr "$value" : '[0-9]*\.\([0-9]*\)\.[0-9]*\.[0-9]*$'`;
    local v2=`expr "$value" : '[0-9]*\.[0-9]*\.\([0-9]*\)\.[0-9]*$'`;
    local v3=`expr "$value" : '[0-9]*\.[0-9]*\.[0-9]*\.\([0-9]*\)$'`;
    eval "$array[0]=\"\$v0\"";
    eval "$array[1]=\"\$v1\"";
    eval "$array[2]=\"\$v2\"";
    eval "$array[3]=\"\$v3\"";
}

function version-manager-check-syntax() {
    [ $# = 1 ] || abort "internal error";
    local value="$1"; shift 1;
    expr "$value" : '[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*$' 1> /dev/null 2>&1;
}

function version-manager-check-order() {
    [ $# = 8 ] || abort "internal error";
    local l0="$1"; local l1="$2"; local l2="$3"; local l3="$4"; shift 4;
    local r0="$1"; local r1="$2"; local r2="$3"; local r3="$4"; shift 4;
    [ $l0 -lt $r0 ] && return 0; [ $l0 -gt $r0 ] && return 1;
    [ $l1 -lt $r1 ] && return 0; [ $l1 -gt $r1 ] && return 1;
    [ $l2 -lt $r2 ] && return 0; [ $l2 -gt $r2 ] && return 1;
    [ $l3 -lt $r3 ] && return 0; [ $l3 -gt $r3 ] && return 1;
    return 1;
}

function version-manager() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local -a args;
    args-loop "$@";

    # get file name and command name
    [ ${#args[@]} -ge 2 ] || { $program-usage 1>&2; exit 1; }
    local file="${args[0]}";
    local command="${args[1]}";

    # check command name and argument count
    case "$command" in
        update    ) local nargs=0;;
        increment ) local nargs=0;;
        set       ) local nargs=1;;
        *         ) $program-usage 1>&2; exit 1;;
    esac;
    [ ${#args[@]} = $[$nargs+2] ] || { $program-usage 1>&2; exit 1; }

    # check new value syntax, if command is "set"
    if [ "$command" = "set" ]; then
        local new_value="${args[2]}";
        if ! $program-check-syntax "$new_value-b0"; then
            local -a error;
            error[0]="version value '$new_value' does not conform";
            error[1]="to version syntax <int>.<int>.<int>";
            abort "${error[*]}";
        fi;
        new_value="$new_value-b0";
    fi;

    # check file existence
    [ -f "$file" ] || abort "could not find file '$file'";

    # update version file
    run rm -f "$file";
    run cvs -Q update "$file";

    # get old value
    local old_value=`tail -1 "$file"`;

    # check old value syntax
    if ! $program-check-syntax "$old_value"; then
        local -a error;
        error[0]="version value '$old_value' in file '$file' does not conform";
        error[1]="to version syntax <int>.<int>.<int>.<int>";
        abort "${error[*]}";
    fi;

    # terminate if command is "update"
    [ "$command" = "update" ] && exit 0;

    # compute old and new arrays
    local -a old_array;
    local -a new_array;
    $program-decompose old_array "$old_value";
    if [ "$command" = "increment" ]; then
        $program-decompose new_array "$old_value";
        new_array[3]=$[${new_array[3]}+1];
    else
        $program-decompose new_array "$new_value";
    fi;

    # check order of old and new values
    if ! $program-check-order "${old_array[@]}" "${new_array[@]}"; then
        abort "new version must be greater than '$old_value'";
    fi;

    # compute new value
    local new_value;
    $program-compose new_value "${new_array[@]}";

    # rewrite version file
    runO "$file~" sed -es@"$old_value"@"$new_value"@ "$file";
    run mv "$file~" "$file";

    # commit version file
    runO /dev/null cvs -Q commit -m "Set version to $new_value" "$file";

    if [ "$command" = "set" ]; then
        echo "Successfully changed version to $new_value";
    fi;
}

##############################################################################
