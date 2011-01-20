/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.reflect

/** This type is required by the compiler and <b>should not be used in client code</b>. */
class Code[T](val tree: Tree)

/** This type is required by the compiler and <b>should not be used in client code</b>. */
object Code {
  def lift[A](tree: A): Code[A] =
    throw new Error("Code was not lifted by compiler")
}
