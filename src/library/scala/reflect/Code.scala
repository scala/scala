/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.reflect

/** This type is required by the compiler and <b>should not be used in client code</b>. */
@deprecated("Replaced with scala.reflect.macro.Context#reify, will be completely removed soon", "2.10")
class Code[T: Manifest](val tree: scala.reflect.mirror.Tree) {
  val manifest = implicitly[Manifest[T]]
  override def toString = "Code(tree = "+tree+", manifest = "+manifest+")"
}

/** This type is required by the compiler and <b>should not be used in client code</b>. */
@deprecated("Replaced with scala.reflect.macro.Context#reify, will be completely removed soon", "2.10")
object Code {
  def lift[A](tree: A): Code[A] =
    throw new Error("Code was not lifted by compiler")
}
