/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.util;



import java.lang.reflect.Field;



public final class Unsafe {
    public final static sun.misc.Unsafe instance;
    static {
        try {
            sun.misc.Unsafe found = null;
            for(Field field : sun.misc.Unsafe.class.getDeclaredFields()) {
		if (field.getType() == sun.misc.Unsafe.class) {
		    field.setAccessible(true);
		    found = (sun.misc.Unsafe) field.get(null);
		    break;
		}
            }
            if (found == null) throw new IllegalStateException("Can't find instance of sun.misc.Unsafe");
            else instance = found;
        } catch(Throwable t) {
          throw new ExceptionInInitializerError(t);
        }
    }
}
