/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class VolatileShortRef implements java.io.Serializable {
    private static final long serialVersionUID = 4218441291229072313L;

    volatile public short elem;
    public VolatileShortRef(short elem) { this.elem = elem; }
    public String toString() { return java.lang.Short.toString(elem); }

    public static VolatileShortRef create(short e) { return new VolatileShortRef(e); }
    public static VolatileShortRef zero() { return new VolatileShortRef((short)0); }
}
