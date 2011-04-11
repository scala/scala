#!/bin/sh
# fact - A simple Scala script that prints out the factorial of
#        the argument specified on the command line.

cygwin=false;
case "`uname`" in
    CYGWIN*) cygwin=true ;;
esac

SOURCE="$0";
if $cygwin; then
    if [ "$OS" = "Windows_NT" ] && cygpath -m .>/dev/null 2>/dev/null ; then
        format=mixed
    else
        format=windows
    fi
    SOURCE=`cygpath --$format "$SOURCE"`;
fi

exec scala -nocompdaemon "$SOURCE" "$@"
!#

case class Test(one : Int, two : Int)
object Test{
 def apply(one : Int): Test = Test(one, 2);
}
