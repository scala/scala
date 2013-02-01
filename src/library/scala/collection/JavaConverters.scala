/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import convert._

// TODO: I cleaned all this documentation up in JavaConversions, but the
// documentation in here is basically the pre-cleaned-up version with minor
// additions.  Would be nice to have in one place.

/** A collection of decorators that allow converting between
 *  Scala and Java collections using `asScala` and `asJava` methods.
 *
 *  The following conversions are supported via `asJava`, `asScala`
 *
 *  - `scala.collection.Iterable` <=> `java.lang.Iterable`
 *  - `scala.collection.Iterator` <=> `java.util.Iterator`
 *  - `scala.collection.mutable.Buffer` <=> `java.util.List`
 *  - `scala.collection.mutable.Set` <=> `java.util.Set`
 *  - `scala.collection.mutable.Map` <=> `java.util.Map`
 *  - `scala.collection.mutable.concurrent.Map` <=> `java.util.concurrent.ConcurrentMap`
 *
 *  In all cases, converting from a source type to a target type and back
 *  again will return the original source object, e.g.
 *  {{{
 *    import scala.collection.JavaConverters._
 *
 *    val sl = new scala.collection.mutable.ListBuffer[Int]
 *    val jl : java.util.List[Int] = sl.asJava
 *    val sl2 : scala.collection.mutable.Buffer[Int] = jl.asScala
 *    assert(sl eq sl2)
 *  }}}
 *  The following conversions also are supported, but the
 *  direction Scala to Java is done my a more specifically named method:
 *  `asJavaCollection`, `asJavaEnumeration`, `asJavaDictionary`.
 *
 *  - `scala.collection.Iterable` <=> `java.util.Collection`
 *  - `scala.collection.Iterator` <=> `java.util.Enumeration`
 *  - `scala.collection.mutable.Map` <=> `java.util.Dictionary`
 *
 *  In addition, the following one way conversions are provided via `asJava`:
 *
 *  - `scala.collection.Seq` => `java.util.List`
 *  - `scala.collection.mutable.Seq` => `java.util.List`
 *  - `scala.collection.Set` => `java.util.Set`
 *  - `scala.collection.Map` => `java.util.Map`
 *
 *  @author Martin Odersky
 *  @since  2.8.1
 */
object JavaConverters extends DecorateAsJava with DecorateAsScala {
  @deprecated("Don't access these decorators directly.", "2.10.0")
  type AsJava[A]            = Decorators.AsJava[A]
  @deprecated("Don't access these decorators directly.", "2.10.0")
  type AsScala[A]           = Decorators.AsScala[A]
  @deprecated("Don't access these decorators directly.", "2.10.0")
  type AsJavaCollection[A]  = Decorators.AsJavaCollection[A]
  @deprecated("Don't access these decorators directly.", "2.10.0")
  type AsJavaEnumeration[A] = Decorators.AsJavaEnumeration[A]
  @deprecated("Don't access these decorators directly.", "2.10.0")
  type AsJavaDictionary[A, B]  = Decorators.AsJavaDictionary[A, B]
}
