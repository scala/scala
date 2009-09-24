#!/bin/sh
# fact - A simple Scala script that prints out the factorial of
#        the argument specified on the command line.

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

export LC_CTYPE=en_US.UTF-8
exec scala -nocompdaemon "$SOURCE" "$@"
!#

/*Comment Комментарий*/
Console.println("QWERTY");
Console.println("ЙЦУКЕН");
