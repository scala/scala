/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.reflect

object Manifest {
  def singleType[T](value: Any): Manifest[T] = null
  def classType[T](clazz: Predef.Class[_]): Manifest[T] = null
  def classType[T](clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] = null
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_]): Manifest[T] = null
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] = null
  def abstractType[T](prefix: Manifest[_], name: String): Manifest[T] = null
  def abstractType[T](prefix: Manifest[_], name: String, args: Manifest[_]*): Manifest[T] = null
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] = null
}

abstract class Manifest[T]

