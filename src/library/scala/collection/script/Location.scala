/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.script


/** Class <code>Location</code> describes locations in messages implemented
 *  by class <code>Message</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/05/2004
 */

sealed abstract class Location
case object Start extends Location
case object End extends Location
case object NoLo extends Location
case class Index(n: Int) extends Location
