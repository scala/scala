########################################################-*-Shell-script-*-####
# Bash Standard Library
##############################################################################
# $Id$

function print() {
    while [ $# -gt 0 ]; do
        echo "$1";
        shift 1;
    done
}

function abort() {
    print "$@" 1>&2;
    exit 1;
}

function run() {
    [ "$verbose" = "true" ] && echo "$@";
    "$@" || exit $?;
}

##############################################################################

# usage: args-abort <error>
# abort argument processing with message <error>
function args-abort() {
    local error="$1";
    shift 1;
    abort "$args_script: $error" "$@";
}

# usage: args-loop <script> "$@"
# process all arguments
function args-loop() {
    local args_script="$1"; shift 1;
    while [ $# -gt 0 ]; do
        $args_script-args "$@";
        shift $?;
    done;
}

# usage: args-option-unknown "$@"
# <option> ... => abort "unknown <option>";
function args-option-unknown() {
    args-abort "unknown option $1";
}

# usage: args-append-array <array> "$@"
# <argument> ... => <array>[${#<array>[@]}]=<argument>; shift 1;
function args-append-array() {
    local array="$1"; shift 1;
    eval "$array[\${#$array[@]}]=\"\$1\"";
    return 1;
}

# usage: args-option-value <value> "$@"
# <option> <argument> ... => <value>=<argument>; shift 2;
function args-option-value() {
    local value="$1"; shift 1;
    if [ $# -lt 2 ]; then
        args-abort "missing argument for option $1";
    fi;
    eval "$value=\"\$2\"";
    return 2;
}

# usage: args-inline-value <value> "$@"
# <value-name>=<argument> ... => <value>=<argument>; shift 1;
function args-inline-value() {
    local value="$1"; shift 1;
    local prefix="$value";
    eval "$value=\"\${1#--$prefix=}\"";
    return 1;
}

##############################################################################
