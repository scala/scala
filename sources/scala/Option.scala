/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** This class represents optional values. Instances of <code>Option</code>
 *  are either instances of case class <code>Some</code> or it is case
 *  object <code>None</code>.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait Option[+a] {

    def get: a = this match {
      case None => error("None.get")
      case Some(x) => x
    }

    def map[b](f: a => b): Option[b] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[b](f: a => Option[b]): Option[b] = this match {
      case None => None
      case Some(x) => f(x)
    }

    def filter(p: a => boolean): Option[a] = this match {
      case None => None
      case Some(x) => if (p(x)) Some(x) else None
    }

    def foreach(f: a => Unit): Unit = this match {
      case None => ()
      case Some(x) => f(x)
    }

    def toList: List[a] = this match {
      case None => Predef.List()
      case Some(x) => Predef.List(x)
    }
}
