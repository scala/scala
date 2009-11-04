/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import compat.Platform.EOL

final class RichException(exc: Throwable) {

  def getStackTraceString: String = {
    val s = new StringBuilder()
    for (trElem <- exc.getStackTrace()) {
      s.append(trElem.toString())
      s.append(EOL)
    }
    s.toString()
  }

}
