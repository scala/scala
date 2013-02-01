/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** This class implements errors which are thrown whenever a
 *  field is used before it has been initialized.
 *
 *  Such runtime checks are not emitted by default.
 *  They can be enabled by the `-Xcheckinit` compiler option.
 *
 *  @since 2.7
 */
final case class UninitializedFieldError(msg: String) extends RuntimeException(msg) {
  def this(obj: Any) = this("" + obj)
}
