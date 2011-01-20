/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class VolatileCharRef implements java.io.Serializable {
    private static final long serialVersionUID = 6537214938268005702L;

    volatile public char elem;
    public VolatileCharRef(char elem) { this.elem = elem; }
    public String toString() { return java.lang.Character.toString(elem); }
}
