/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class VolatileBooleanRef implements java.io.Serializable {
    private static final long serialVersionUID = -5730524563015615974L;

    volatile public boolean elem;
    public VolatileBooleanRef(boolean elem) { this.elem = elem; }
    public String toString() { return String.valueOf(elem); }

    public static VolatileBooleanRef create(boolean e) { return new VolatileBooleanRef(e); }
    public static VolatileBooleanRef zero() { return new VolatileBooleanRef(false); }
}
