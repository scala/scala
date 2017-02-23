/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

/** A SourceContext is a descriptor for the invocation of a method that takes
 *  an implicit parameter of type SourceContext. It provides information about the
 *  location in the source where the method is invoked, such as the source file name
 *  and line number.
 */
trait SourceContext extends SourceLocation {

  var parent: Option[SourceContext] =
    None

  def update(context: SourceContext): SourceContext

  def bindings: List[(String, Int)]

  def assignedVariable: Option[String] =
    if (bindings(0)._1 == null) None
    else Some(bindings(0)._1)

  def methodName: String

  def receiver: Option[String]

  def allContexts: List[List[(String, Int)]]

  override def toString = if (SourceContext.debug) {
    "SourceContext at " + methodName + ":\n// " +
    "Assigning to: " + assignedVariable + "\n// " +
    "SourceLocation: " + super.toString + "\n// " +
    "Parent: " +
    (if (parent.isEmpty) "None" else "\n// " + parent.get.toString)
  } else
    super.toString

}

object SourceContext {

  var debug = false

  def apply(name: String, sourceInfo: List[(String, Int)]): SourceContext =
    apply("<unknown file>", name, sourceInfo)

  def apply(fileName: String, name: String, sourceInfo: List[(String, Int)]): SourceContext =
    new ConcreteSourceContext(fileName, name, sourceInfo)

  def apply(fileName: String, name: String, receiver: String, sourceInfo: List[(String, Int)]): SourceContext =
    new ConcreteSourceContext(fileName, name, Some(receiver), sourceInfo)

  private class ConcreteSourceContext(override val fileName: String,
                                      val methodName: String,
                                      val receiver: Option[String],
                                      val bindings: List[(String, Int)])
  extends SourceContext {

    def this(file: String, method: String, bs: List[(String, Int)]) =
      this(file, method, None, bs)

    def line = bindings(0)._2

    def update(context: SourceContext): SourceContext = {
      context.parent = Some(this)
      context
    }

    def allContexts = {
      var contexts: List[List[(String, Int)]] = List()
      var curr: SourceContext = this
      contexts = contexts ::: List(curr.bindings)
      while (!curr.parent.isEmpty) {
        curr = curr.parent.get
        contexts = contexts ::: List(curr.bindings)
      }
      contexts
    }
  }

}
