/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;

public class VolatileDoubleRef implements java.io.Serializable {
    private static final long serialVersionUID = 8304402127373655534L;

    volatile public double elem;
    public VolatileDoubleRef(double elem) { this.elem = elem; }
    public String toString() { return java.lang.Double.toString(elem); }
}
