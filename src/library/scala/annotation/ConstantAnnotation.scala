/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2017, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

package scala.annotation

/**
 * Annotation classes extending this trait only accept constant values as arguments.
 *
 * Note that this trait extends [[StaticAnnotation]], so constant annotations are
 * persisted in the classfile.
 */
trait ConstantAnnotation extends StaticAnnotation
