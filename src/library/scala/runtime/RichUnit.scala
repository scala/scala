/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

/** This class exists only as a dummy subclass so that there are two ambiguous
 *  implicit conversions from Unit to some subclass to Object.
 *  It's important that this class should NOT inherit from Ordered.
 *
 *  Note - in reality the ambiguity is successfully introduced by any2stringadd
 *  and orderingToOrdered, and adding an implicit from () => RichUnit actually
 *  resolves the ambiguity by being more specific, and succeeds! So this class
 *  is probably useless, and unitWrapper has been removed from Predef.
 */
final class RichUnit {}
