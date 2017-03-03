/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package convert

import java.{ util => ju }

private[collection] object Decorators {
  /** Generic class containing the `asJava` converter method */
  class AsJava[A](op: => A) {
    /** Converts a Scala collection to the corresponding Java collection */
    def asJava: A = op
  }

  /** Generic class containing the `asScala` converter method */
  class AsScala[A](op: => A) {
    /** Converts a Java collection to the corresponding Scala collection */
    def asScala: A = op
  }

  /** Generic class containing the `asJavaCollection` converter method */
  class AsJavaCollection[A](i: Iterable[A]) {
    /** Converts a Scala `Iterable` to a Java `Collection` */
    def asJavaCollection: ju.Collection[A] = JavaConverters.asJavaCollection(i)
  }

  /** Generic class containing the `asJavaEnumeration` converter method */
  class AsJavaEnumeration[A](i: Iterator[A]) {
    /** Converts a Scala `Iterator` to a Java `Enumeration` */
    def asJavaEnumeration: ju.Enumeration[A] = JavaConverters.asJavaEnumeration(i)
  }

  /** Generic class containing the `asJavaDictionary` converter method */
  class AsJavaDictionary[A, B](m : mutable.Map[A, B]) {
    /** Converts a Scala `Map` to a Java `Dictionary` */
    def asJavaDictionary: ju.Dictionary[A, B] = JavaConverters.asJavaDictionary(m)
  }
}
