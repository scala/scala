/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: MatchError.scala 10366 2007-03-16 13:34:42 +0000 (Fri, 16 Mar 2007) michelou $


package scala

import Predef._

/** This class represents uninitialized variable/value errors.
 *  @author  Martin Odersky
 */
final class UninitializedError extends RuntimeException("uninitialized value")

