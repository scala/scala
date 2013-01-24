/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml.dtd.impl

/** Pointed regular hedge expressions, a useful subclass of regular hedge expressions.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
@deprecated("This class will be removed", "2.10.0")
private[dtd] abstract class PointedHedgeExp extends Base {

  type _regexpT <: RegExp
  type _labelT

  case class  Node(label: _labelT, r: _regexpT) extends RegExp {
    final val isNullable = false
  }

  case class  TopIter(r1: _regexpT, r2: _regexpT) extends RegExp {
    final val isNullable = r1.isNullable && r2.isNullable //?
  }

  case object Point extends RegExp {
    final val isNullable = false
  }

}
