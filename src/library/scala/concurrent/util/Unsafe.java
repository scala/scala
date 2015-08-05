/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.util;
import java.lang.reflect.Field;

// TODO: remove once akka no longer needs it, hopefully by 2.12.0-M3!
@Deprecated
public final class Unsafe {
  @Deprecated
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

// Scala version:
// classOf[sun.misc.Unsafe].getDeclaredFields.filter(_.getType == classOf[sun.misc.Unsafe]).headOption.map { field =>
//   field.setAccessible(true); field.get(null).asInstanceOf[sun.misc.Unsafe]
// } getOrElse (throw new IllegalStateException("Can't find instance of sun.misc.Unsafe"))
