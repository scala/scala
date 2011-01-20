/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org                **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class VolatileByteRef implements java.io.Serializable {
    private static final long serialVersionUID = -100666928446877072L;

    volatile public byte elem;
    public VolatileByteRef(byte elem) { this.elem = elem; }
    public String toString() { return java.lang.Byte.toString(elem); }
}
