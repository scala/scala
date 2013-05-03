/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package tools.ant.sabbus

import org.apache.tools.ant.Task

class Break extends Task {

  def setId(input: String) {
    id = Some(input)
  }

  private var id: Option[String] = None

  override def execute() {
    if (id.isEmpty) sys.error("Attribute 'id' is not set")
    Compilers.break(id.get)
  }

}
