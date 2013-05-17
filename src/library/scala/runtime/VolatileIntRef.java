/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;

public class VolatileIntRef implements java.io.Serializable {
    private static final long serialVersionUID = 1488197132022872888L;

    volatile public int elem;
    public VolatileIntRef(int elem) { this.elem = elem; }
    public String toString() { return java.lang.Integer.toString(elem); }

    public static VolatileIntRef create(int e) { return new VolatileIntRef(e); }
    public static VolatileIntRef zero() { return new VolatileIntRef(0); }
}
