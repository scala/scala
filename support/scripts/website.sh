########################################################-*-Shell-script-*-####
# Website Functions
##############################################################################
# $Id$

source ${0%/*}/stdlib.sh;

##############################################################################
# website-print-xml-distributions

function website-print-xml-distributions-usage() {
    echo "Usage: $program <archive-directory>";
}

function website-print-xml-distributions-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        --verbose             ) verbose="true"; return 1;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function website-print-xml-distributions-get-archive() {
    local archive="$1"; shift 1;

    if [ -f "$archive" ]; then
        local name=`basename $archive`;
        local size=`stat -l -c%s $archive`;
        echo "<archive name=\"$name\" size=\"$size\"/>";
    fi;
}

function website-print-xml-distributions() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local verbose="false";
    local -a args;
    args-loop "$@";

    [ ${#args[@]} == 1 ] || { $program-usage 1>&2; exit 1; };
    local archivedir="${args[0]}";

    if [ ! -d "$archivedir" ]; then
        abort "could not find directory '$archivedir'";
    fi;

    echo "<distributions>";
    echo "";

    local file;
    local archives=`ls -1t $archivedir/*.tar.gz`;
    for file in $archives; do
        local basename=`basename $file .tar.gz`;
        local tgz_file="$archivedir/$basename.tar.gz";
        local bz2_file="$archivedir/$basename.tar.bz2";
        local zip_file="$archivedir/$basename.zip";

        local version_regex="s/[^-]*-\([0-9]*\(\.[0-9]*\.\|-\)[0-9]*\)/\1/";
        local version=`echo $basename | sed "$version_regex"`;
        local date=`date -r "$file" +%d-%b-%Y`;
        local tgz_line=`$program-get-archive $tgz_file`;
        local bz2_line=`$program-get-archive $bz2_file`;
        local zip_line=`$program-get-archive $zip_file`

        echo "  <distribution>";
        echo "    <version>$version</version>";
        echo "    <date>$date</date>";
        echo "    $tgz_line";
        echo "    $bz2_line";
        echo "    $zip_line";
        echo "  </distribution>";
        echo "";
    done;

    echo "</distributions>";
}

##############################################################################
# website-print-xml-installers

function website-print-xml-installers-usage() {
    echo "Usage: $program <archive-basepath>";
}

function website-print-xml-installers-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        --verbose             ) verbose="true"; return 1;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function website-print-xml-installers-add-entry() {
    [ $# = 5 ] || abort "internal error";
    local archive="$1"; shift 1;
    local platform="$1"; shift 1;
    local description="$1"; shift 1;
    local path="$1"; shift 1;
    local anchor="$1"; shift 1;

    if [ ! -f "$archive" ]; then
        warning "could not find file '$archive'";
        return 1;
    fi;

    local size=`stat -l -c%s "$archive"`;
    local size=`echo "scale=1; $size/1024/1024" | bc`;
    case "$size" in .* ) size="0$size";; esac;

    echo "  <installer>";
    [ -n $anchor ] &&
    echo "    <platform>$platform</platform>";
    echo "    <description>$description</description>";
    echo "    <file path=\"$path\" size=\"$size\"/>";
    echo "    <anchor>$anchor</anchor>";
    echo "  </installer>";
    echo "";
}

function website-print-xml-installers-add-installer() {
    [ $# = 4 ] || abort "internal error";
    local installerdir="$1"; shift 1;
    local path="$1"; shift 1;
    local platform="$1"; shift 1;
    local description="$1"; shift 1;

    local path="Web_Installers/InstData/$path"
    local archive="$installerdir/$path"; shift 1;
    $program-add-entry \
        "$archive" "$platform" "$description" "$path" "$platform";
}

function website-print-xml-installers-add-installers() {
    [ $# = 1 ] || abort "internal error";
    local installerdir="$1"; shift 1;

    if [ ! -d "$installerdir" ]; then
        warning "could not find directory '$installerdir'";
        return 1;
    fi;

    local -a add=($program-add-installer "$installerdir");
    "${add[@]}" "Windows/NoVM/install.exe" "win"     "Windows Installer";
    "${add[@]}" "MacOSX/install.zip"       "macosx"  "MacOSX Installer";
    "${add[@]}" "Linux/NoVM/install.bin"   "linux"   "Linux Installer";
    "${add[@]}" "Solaris/NoVM/install.bin" "solaris" "Solaris Installer";
    "${add[@]}" "AIX/NoVM/install.bin"     "aix"     "AIX Installer";
    "${add[@]}" "HPUX/NoVM/install.bin"    "hp"      "HPUX Installer";
    "${add[@]}" "GenericUnix/install.bin"  "unix"    "GenericUnix Installer";
    "${add[@]}" "Java/install.jar"         "other"   "Java Installer";
}

function website-print-xml-installers-add-archive() {
    [ $# = 3 ] || abort "internal error";
    local archive="$1"."$2"; shift 1;
    local suffix="$1"; shift 1;
    local description="$1"; shift 1;

    local path="./distrib/"`basename $archive`;
    $program-add-entry "$archive" "" "$description" "$path" "$suffix";
}

function website-print-xml-installers-add-archives() {
    [ $# = 1 ] || abort "internal error";
    local basepath="$1"; shift 1;

    local -a add=($program-add-archive "$basepath");
    "${add[@]}" "tar.gz"  "Gzip Unix tarball (Unix/Cygwin)";
    "${add[@]}" "tar.bz2" "Bz2 Unix tarball (Unix/Cygwin)";
    "${add[@]}" "zip"     "Zip Archive (Windows)";
}

function website-print-xml-installers() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local verbose="false";
    local -a args;
    args-loop "$@";

    [ ${#args[@]} == 1 ] || { $program-usage 1>&2; exit 1; };
    local basepath="${args[0]}";

    echo "<installers>";
    echo "";
    $program-add-installers "$basepath.ia";
    $program-add-archives   "$basepath";
    echo "</installers>";

    local installhtm="$basepath.ia/Web_Installers/install.htm";
    if [ ! -f "$installhtm" ]; then
        warning "could not find file '$installhtm'";
        return 1;
    fi;

    local start=`grep -n "^setArchiveFile()\$" "$installhtm"`;
    local end=`grep -n "^platformButtons()\$" "$installhtm"`;
    start=$[${start%%:*} + 3];
    end=$[${end%%:*} - 2];

    echo "";
    echo "<webinstaller>";
    echo "";
    echo "<params>";
    head -$end "$installhtm" | tail -$[$end - $start] \
        | sed '-es/[	 ]*//' '-es!>$!/>!' '-e/^$/d';
    echo "</params>";
    echo "";
    echo "</webinstaller>";
}

##############################################################################
# website-print-xml

function website-print-xml-usage() {
    echo "Usage: $program <archive-directory> <current-version>";
}

function website-print-xml-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        --verbose             ) verbose="true"; return 1;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function website-print-xml() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local verbose="false";
    local -a args;
    args-loop "$@";

    [ ${#args[@]} == 2 ] || { $program-usage 1>&2; exit 1; };
    local archivedir="${args[0]}";
    local current="${args[1]}";

    if [ ! -d "$archivedir" ]; then
        abort "could not find directory '$archivedir'";
    fi;

    echo "<website>";
    echo "";
    echo "<version>$current</version>";
    echo "";
    $program-distributions "$archivedir";
    echo "";
    $program-installers "$archivedir/scala-$current";
    echo "";
    echo "</website>";
}

##############################################################################
# website-build

function website-build-usage() {
    echo "Usage: $program <install-directory> <archive-directory>";
}

function website-build-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        --verbose             ) verbose="true"; return 1;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function website-build-link() {
    [ $# = 2 ] || abort "internal error";
    local srcfile="$1"; shift 1;
    local dstdir="$1"; shift 1;

    if [ ! -e "$srcfile" ]; then
        abort "could not find directory or file '$srcfile'";
    fi;

    if [ ! -d "$dstdir" ]; then
        abort "could not find directory '$dstdir'";
    fi;

    run ln -s "$srcfile" "$dstdir";
}

function website-build-pdf-link() {
    [ $# = 3 ] || abort "internal error";
    local srcdir="$1"; shift 1;
    local basename="$1"; shift 1;
    local dstdir="$1"; shift 1;

    $program-link "$srcdir/$basename.pdf" "$dstdir";
    run acroread -toPostScript "$dstdir/$basename.pdf";
    run gzip -9 "$dstdir/$basename.ps";
}

function website-build() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local verbose="false";
    local -a args;
    args-loop "$@";

    [ ${#args[@]} == 2 ] || { $program-usage 1>&2; exit 1; };
    local installdir="${args[0]}";
    local archivedir="${args[1]}";

    if [ ! -d "$installdir" ]; then
        abort "could not find directory '$installdir'";
    fi;

    if [ ! -d "$archivedir" ]; then
        abort "could not find directory '$archivedir'";
    fi;

    # determine version of installed scala
    if [ ! -x "$installdir/bin/scala-info" ]; then
        abort "could not find script '$installdir/bin/scala-info'";
    fi;
# !!!    local current=`$installdir/bin/scala-info --version`;
    local current=`cat $installdir/VERSION`;
    if [ $? != 0 -o -z "$current" ]; then
        abort "could not determine version of installed scala";
    fi;

    # remove old repository and create new one
    local websitedir="$archivedir/website";
    if [ -d "$websitedir" ]; then
        [ -d "$websitedir.old" ] && run rm -rf "$websitedir.old";
        run mv "$websitedir" "$websitedir.old";
    fi;
    run mkdir "$websitedir";
    run mkdir "$websitedir/doc";

    # create website.xml
    runO "$websitedir/website.xml" website-print-xml "$archivedir" "$current";

    # link web installers
    $program-link "$archivedir/scala-$current.ia/Web_Installers" "$websitedir";

    # link api documentation
    $program-link  "$installdir/doc/api" "$websitedir/doc";

    # link PDF documents and create PostScript versions
    $program-pdf-link "$installdir/doc" "ScalaReference" "$websitedir/doc";
    $program-pdf-link "$installdir/doc" "ScalaByExample" "$websitedir/doc";
    $program-pdf-link "$installdir/doc" "ScalaTutorial" "$websitedir/doc";

    # copy old PDF and PostScript version of overview
    run cp -a "$websitedir.old/doc/ScalaOverview.pdf" "$websitedir/doc/";
    run cp -a "$websitedir.old/doc/ScalaOverview.ps.gz" "$websitedir/doc/";
}

##############################################################################
