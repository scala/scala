#!/bin/sh
# 
# Checks if UTF-8 output makes it through unmangled.

cygwin=false;
case "`uname`" in
    CYGWIN*) cygwin=true ;;
esac

SOURCE="$0";
if $cygwin; then
    if [ "$OS" = "Windows_NT" ] && cygpath -m .>/dev/null 2>/dev/null ; 
then
        format=mixed
    else
        format=windows
    fi
    SOURCE=`cygpath --$format "$SOURCE"`;
fi

exec scala -Dfile.encoding="UTF-8" -nocompdaemon "$SOURCE" "$@"
!#

/*Comment Комментарий*/
Console.println("QWERTY");
Console.println("ЙЦУКЕН");
