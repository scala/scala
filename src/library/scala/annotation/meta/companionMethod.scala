/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation.meta

/**
 * When defining an implicit class, the Scala compiler creates an implicit
 * conversion method for it. Annotations `@companionClass` and `@companionMethod`
 * control where an annotation on the implicit class will go. By default, annotations
 * on an implicit class end up only on the class.
 *
 */
final class companionMethod extends scala.annotation.StaticAnnotation
