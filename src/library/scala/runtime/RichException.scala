/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RichString.scala 9112 2006-11-01 16:58:23Z michelou $


package scala.runtime

import compat.Platform.EOL

final class RichException(exc: Throwable) {

  def getStackTraceString: String = {
    val s = new StringBuilder()
    for (val trElem <- exc.getStackTrace()) {
      s.append(trElem.toString())
      s.append(EOL)
    }
    s.toString()
  }

}
