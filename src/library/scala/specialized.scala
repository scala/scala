/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** Annotate type parameters on which code should be automatically
 *  specialized. For example:
 *  <code>
 *    class MyList[@specialized T] ...
 *  </code>
 *
 *  Type T can be specialized on a subset of the primitive types by
 *  specifying a comma-separated string argument:
 *
 *  <code>
 *   class MyList[@specialized("Int, Double, Boolean") T] ..
 *  </code>
 *  Only primitive types are supported and no name resolution is currently
 *  done on the string arguments (meaning imports and type aliases are
 *  not resolved).
 *
 *  @since 2.8
 */
class specialized(types: String) extends StaticAnnotation {
  def this() {
    this("Boolean, Byte, Short, Char, Int, Long, Float, Double")
  }
}

