/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
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
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Option[+A] extends Iterable[A] with java.io.Serializable {

  def isEmpty: Boolean = this match {
    case None => true
    case _ => false
  }

  def get: A = match {
    case None => error("None.get")
    case Some(x) => x
  }

  def get[B >: A](default: B): B = match {
    case None => default
    case Some(x) => x
  }

  def map[B](f: A => B): Option[B] = match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = match {
    case None => None
    case Some(x) => f(x)
  }

  def filter(p: A => Boolean): Option[A] = match {
    case None => None
    case Some(x) => if (p(x)) Some(x) else None
  }

  override def foreach(f: A => Unit): Unit = match {
    case None => ()
    case Some(x) => f(x)
  }

  def elements: Iterator[A] = match {
    case None => Iterator.empty
    case Some(x) => Iterator.fromValues(x)
  }

  def toList: List[A] = match {
    case None => List()
    case Some(x) => List(x)
  }

}
