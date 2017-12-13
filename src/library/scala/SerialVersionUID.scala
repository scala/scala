/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2017, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

package scala

/**
  * Annotation for specifying the `serialVersionUID` field of a (serializable) class.
  *
  * On the JVM, a class with this annotation will receive a `private`, `static`,
  * and `final` field called `serialVersionUID` with the provided [[value]],
  * which the JVM's serialization mechanism uses to determine serialization
  * compatibility between different versions of a class.
  *
  * @see [[http://docs.oracle.com/javase/8/docs/api/java/io/Serializable.html `java.io.Serializable`]]
  * @see [[Serializable]]
  */
class SerialVersionUID(value: Long) extends scala.annotation.ConstantAnnotation
