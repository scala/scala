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
    print "${program:-error}: ""$@" 1>&2;
    exit 1;
}

function run_() {
    "$@" || exit $?;
}

function run() {
    [ "$verbose" = "true" ] && echo "$@";
    run_ "$@";
}

function runO() {
    local stdout="$1"; shift 1;
    [ "$verbose" = "true" ] && echo "$@" "1>" "$stdout";
    run_ "$@" 1> "$stdout";
}

##############################################################################

# usage: args-loop <script> "$@"
# process all arguments
function args-loop() {
    while [ $# -gt 0 ]; do
        $program-args "$@";
        shift $?;
    done;
}

# usage: args-option-unknown "$@"
# <option> ... => abort "unknown option <option>";
function args-option-unknown() {
    abort "unknown option $1";
}

# usage: args-unknown "$@"
# <argument> ... => abort "don't know what to do with argument <argument>";
function args-unknown() {
    abort "don't know what to do with argument '$1'";
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
        abort "missing argument for option $1";
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
