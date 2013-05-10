/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class CharRef implements java.io.Serializable {
    private static final long serialVersionUID = 6537214938268005702L;

    public char elem;
    public CharRef(char elem) { this.elem = elem; }
    public String toString() { return java.lang.Character.toString(elem); }

    public static CharRef create(char e) { return new CharRef(e); }
    public static CharRef zero() { return new CharRef((char)0); }
}
