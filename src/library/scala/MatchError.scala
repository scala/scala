/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** This class implements errors which are thrown whenever an
 *  object doesn't match any pattern of a pattern matching
 *  expression.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 1.1, 05/03/2004
 *  @since   2.0
 */
final class MatchError(@transient obj: Any) extends RuntimeException {
  /** There's no reason we need to call toString eagerly,
   *  so defer it until getMessage is called or object is serialized
   */
  private lazy val objString = {
    def ofClass = "of class " + obj.getClass.getName
    if (obj == null) "null"
    else try {
      obj.toString() + " (" + ofClass + ")"
    } catch {
      case _: Throwable => "an instance " + ofClass
    }
  }

  @throws[java.io.ObjectStreamException]
  private def writeReplace(): Object = {
    objString
    this
  }

  override def getMessage() = objString
}
