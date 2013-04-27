/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class IntRef implements java.io.Serializable {
    private static final long serialVersionUID = 1488197132022872888L;

    public int elem;
    public IntRef(int elem) { this.elem = elem; }
    public String toString() { return java.lang.Integer.toString(elem); }

    public static IntRef create(int e) { return new IntRef(e); }
    public static IntRef zero() { return new IntRef(0); }
}
