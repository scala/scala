/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.regexp

/** Basic regular expressions.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class Base
{
  type _regexpT <: RegExp

  abstract class RegExp {
    val isNullable: Boolean
  }

  /** Alt( R,R,R* ) */
  case class Alt(rs: _regexpT*) extends RegExp {
    // check rs \in R,R,R*
    // @todo: flattening
    if (rs.size < 2)
      throw new SyntaxError("need at least 2 branches in Alt")

    final val isNullable = rs forall (_.isNullable)
  }

  case class Sequ(rs: _regexpT*) extends RegExp {
    // @todo: flattening
    // check rs \in R,R*
    if (rs.isEmpty)
      throw new SyntaxError("need at least 1 item in Sequ")

    final val isNullable = rs forall (_.isNullable)
  }

  case class Star(r: _regexpT) extends RegExp {
    final lazy val isNullable = true
  }

  case object Eps extends RegExp {
    final lazy val isNullable = true
    override def toString() = "Eps"
  }

  /** this class can be used to add meta information to regexps */
  class Meta(r1: _regexpT) extends RegExp {
    final val isNullable = r1.isNullable
    def r = r1
  }

  final def mkSequ(rs: _regexpT *): RegExp =
    if (rs.isEmpty) Eps else Sequ(rs : _*)
}
