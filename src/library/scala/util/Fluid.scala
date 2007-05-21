/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util


import Predef._
import java.lang.InheritableThreadLocal

/** <p>This is the same as DynamicVariable.  The name "fluid"
 *  is confusing, and perhaps not even accurate.</p>
 *
 *  @author  Lex Spoon
 *  @version 1.1, 2007-5-21
 */
@deprecated
class Fluid[T](init: T) extends DynamicVariable[T](init)
