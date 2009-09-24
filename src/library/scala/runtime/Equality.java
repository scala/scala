/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;

import java.io.*;

/** An object (static class) encapsulating the variations on equality
 *  presently under consideration.  It's written in java so it can easily
 *  be made available to BoxesRunTime as well as scala code.
 */
public class Equality
{
  public static boolean disallowNullEquals = true;
  public static boolean disallowPrimitiveEquals = true;
  public static boolean warnOnEqualsAny = true;
  public static boolean callCanEqualIfAvailable = true;
  public static boolean logEverything = false;

  public static String obToString(Object o) {
    String s = o.toString() + " (" + o.getClass().getSimpleName() + ")";
    return s.replaceAll("\\n", " ");
  }

  public static void logInternal(String msg, Object a, Object b, String where) {
    log(String.format("%s %s == %s at %s", msg, obToString(a), obToString(b), where));
  }

  public static String whereAreWe() {
    StackTraceElement[] es = Thread.currentThread().getStackTrace();
    if (es.length < 4)
      return "<unknown>";

    StackTraceElement e = es[3];
    String clazz = e.getClassName().replaceAll("\\$.*$", "\\$...");
    return String.format("%s.%s(%s.%d)", clazz, e.getMethodName(), e.getFileName(), e.getLineNumber());
  }

  public static void log(String msg) {
    if (logEverything)
      System.out.println(msg);
  }
}
