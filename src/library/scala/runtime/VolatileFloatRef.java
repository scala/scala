/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class VolatileFloatRef implements java.io.Serializable {
    private static final long serialVersionUID = -5793980990371366933L;

    volatile public float elem;
    public VolatileFloatRef(float elem) { this.elem = elem; }
    public String toString() { return java.lang.Float.toString(elem); }

    public static VolatileFloatRef create(float e) { return new VolatileFloatRef(e); }
    public static VolatileFloatRef zero() { return new VolatileFloatRef(0); }
}
