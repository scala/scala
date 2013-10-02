/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class LongRef implements java.io.Serializable {
    private static final long serialVersionUID = -3567869820105829499L;

    public long elem;
    public LongRef(long elem) { this.elem = elem; }
    public String toString() { return java.lang.Long.toString(elem); }

    public static LongRef create(long e) { return new LongRef(e); }
    public static LongRef zero() { return new LongRef(0); }
}
