/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package parsing

import java.net.URL
import java.io.File.separator

import scala.io.Source

/**
 *  @author  Burak Emir
 *  @version 1.0
 */
trait ExternalSources {
  self: ExternalSources with MarkupParser with MarkupHandler =>

  def externalSource(systemId: String): Source = {
    if (systemId startsWith "http:")
      return Source fromURL new URL(systemId)

    val fileStr: String = input.descr match {
      case x if x startsWith "file:"  => x drop 5
      case x                          => x take ((x lastIndexOf separator) + 1)
    }

    Source.fromFile(fileStr + systemId)
  }
}
