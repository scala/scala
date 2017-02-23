/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

/** A SourceLocation is a descriptor for the invocation of a method that takes
 *  an implicit parameter of type SourceLocation. It provides information about the
 *  location in the source where the method is invoked, such as the source file name
 *  and line number.
 */
trait SourceLocation extends Serializable {

  /** The name of the source file in which this SourceLocation has been generated.
   */
  def fileName: String = ""

  /** The line number at which this SourceLocation has been generated.
   */
  def line: Int

  /** The character offset at which this SourceLocation has been generated.
   */
  def charOffset: Int = 0

  override def toString: String =
    fileName + ":" + line + ":" + charOffset
}

/** The object SourceLocation defines factory methods for SourceLocations.
 *  It is intended for use by the compiler and should not be used
 *  in client code.
 */
object SourceLocation {
  def apply(line: Int, fileName: String): SourceLocation =
    new ConcreteSourceLocation(line, fileName)

  def apply(line: Int, offset: Int, fileName: String): SourceLocation =
    new ConcreteSourceLocation(line, offset, fileName)

  private class ConcreteSourceLocation(override val line: Int,
                                       override val charOffset: Int,
                                       override val fileName: String)
  extends SourceLocation {
    def this(line: Int, file: String) {
      this(line, 0, file)
    }
  }

}
