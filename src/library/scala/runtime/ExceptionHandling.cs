/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

using System;
using scala;

namespace scala.runtime {

	public abstract class RunTime {
	
    public interface Runnable {
      void run();
    }

    public static Exception tryCatch(Runnable runnable) {
      try {
        runnable.run();
        return null;
      } catch (Exception exception) {
        return exception;
			}
    }
  
  }

}
