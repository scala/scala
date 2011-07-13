/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** Annotate type parameters on which code should be automatically
 *  specialized. For example:
 *  {{{
 *    class MyList[@specialized T] ...
 *  }}}
 *
 *  Type T can be specialized on a subset of the primitive types by
 *  specifying a list of primitive types to specialize at:
 *  {{{
 *    class MyList[@specialized(Int, Double, Boolean) T] ..
 *  }}}
 *
 *  @since 2.8
 */
class specialized(types: SpecializableCompanion*) extends annotation.StaticAnnotation {
  def this() {
    this(Unit, Boolean, Byte, Short, Char, Int, Long, Float, Double)
  }
}
