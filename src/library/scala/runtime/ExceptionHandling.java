/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime;

public abstract class ExceptionHandling {

  public static Throwable tryCatch(Runnable runnable) {
    try {
      runnable.run();
      return null;
    } catch (Throwable exception) {
      return exception;
    }
  }

}
