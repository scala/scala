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
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.Handler;
import java.util.logging.StreamHandler;
import java.util.logging.SimpleFormatter;

/** An object (static class) encapsulating the variations on equality
 *  presently under consideration.  It's written in java so it can easily
 *  be made available to BoxesRunTime as well as scala code.
 */
public class Equality
{
  public static boolean logEverything = false;
  public static boolean use28Semantics = false;

  public static boolean warnOnBoxedCompare = false;
  public static boolean warnOnBoxedCompareIfValuesAreEqual = false;
  public static boolean dieOnBoxedCompare = false;
  public static boolean dieOnBoxedCompareIfValuesAreEqual = false;

  private static Handler handler;
  public static Logger logger;
  public static final Logger defaultLogger;

  static {
    class EqualityLogger extends Logger {
      EqualityLogger() {
        super("EqualityLogger", null);
      }
    }
    defaultLogger = new EqualityLogger();

    handler = new StreamHandler(System.out, new SimpleFormatter());
    handler.setLevel(Level.INFO);
    defaultLogger.addHandler(handler);

    logger = defaultLogger;
  }

  public static void warnOrDie(String msg, boolean isFatal) {
    if (isFatal) throw new RuntimeException(msg);
    else log(msg);
    // else System.out.println(msg);
  }

  public static String obToString(Object o) {
    String s = o.toString() + " (" + o.getClass().getSimpleName() + ")";
    return s.replaceAll("\\n", " ");
  }

  public static void logComparison(String msg, String cmp, String where) {
    log(String.format("%s (%s) at %s", msg, cmp, where));
  }

  public static String whereAreWe() { return whereAreWe(4); }
  public static String whereAreWe(int depth) {
    StackTraceElement[] es = Thread.currentThread().getStackTrace();
    if (es.length < depth)
      return "<unknown>";

    StackTraceElement e = es[depth - 1];
    String clazz = e.getClassName().replaceAll("\\$.*$", "\\$...");
    return String.format("%s.%s(%s.%d)", clazz, e.getMethodName(), e.getFileName(), e.getLineNumber());
  }

  public static void log(String msg) {
    if (logger != null) {
      logger.warning(msg);
      handler.flush();
    }
    else System.out.println(msg);
  }
}
