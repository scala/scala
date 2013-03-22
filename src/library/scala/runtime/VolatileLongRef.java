/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class VolatileLongRef implements java.io.Serializable {
    private static final long serialVersionUID = -3567869820105829499L;

    volatile public long elem;
    public VolatileLongRef(long elem) { this.elem = elem; }
    public String toString() { return java.lang.Long.toString(elem); }

    public static VolatileLongRef create(long e) { return new VolatileLongRef(e); }
    public static VolatileLongRef zero() { return new VolatileLongRef(0); }
}
