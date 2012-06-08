package scala.reflect
package api

import java.io.{ PrintWriter, StringWriter }

trait TreePrinters { self: Universe =>

  trait TreePrinter {
    def print(args: Any*)
    protected var typesPrinted = false
    protected var uniqueIds = false
    def withTypesPrinted: this.type = { typesPrinted = true; this }
    def withUniqueIds: this.type = { uniqueIds = true; this }
  }

  def show(tree: Tree): String = show(tree, newTreePrinter)

  def show(tree: Tree, mkPrinter: PrintWriter => TreePrinter): String = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)
    val printer = mkPrinter(writer)
    printer.print(tree)
    writer.flush()
    buffer.toString
  }

  def showRaw(tree: Tree): String = show(tree, new RawTreePrinter(_))

  /** Hook to define what `show(tree)` means.
   */
  def newTreePrinter(out: PrintWriter): TreePrinter

  // emits more or less verbatim representation of the provided tree
  // [Eugene] todo. needs to be refined
  //  http://groups.google.com/group/scala-user/browse_thread/thread/de5a5be2e083cf8e
  class RawTreePrinter(out: PrintWriter) extends TreePrinter {
    def print(args: Any*): Unit = args foreach {
      case EmptyTree =>
        print("EmptyTree")
      case tree @ TypeTree() =>
        print("TypeTree()")
        if (tree.tpe != null)
          print(".setType(", tree.tpe, ")")
        else if (tree.original != null)
          print(".setOriginal(", tree.original, ")")
      case Literal(Constant(s: String)) =>
        print("Literal(Constant(\"" + s + "\"))")
      case tree: Tree =>
        print(tree.productPrefix+"(")
        val it = tree.productIterator
        while (it.hasNext) {
          it.next() match {
            case name: Name if uniqueIds && tree.hasSymbol && tree.symbol != NoSymbol =>
              print(tree.symbol.name, "#", tree.symbol.id)
            case arg =>
              print(arg)
          }
          print(if (it.hasNext) ", " else "")
        }
        print(")")
        if (typesPrinted)
          print(".setType(", tree.tpe, ")")
      case list: List[_] =>
        print("List(")
        val it = list.iterator
        while (it.hasNext) {
          print(it.next())
          print(if (it.hasNext) ", " else "")
        }
        print(")")
      case mods: Modifiers =>
        val parts = collection.mutable.ListBuffer[String]()
        parts += mods.flagString
        if (mods.privateWithin.toString.nonEmpty)
          parts += "newTypeName(\"" + mods.privateWithin.toString + "\")"
        if (mods.annotations.nonEmpty)
          parts += mods.annotations map showRaw mkString ("List(", ", ", ")")
        print(parts mkString ("Modifiers(", ", ", ")"))
      case name: Name =>
        if (name.isTermName) print("newTermName(\"") else print("newTypeName(\"")
        print(name.toString)
        print("\")")
      case arg =>
        out.print(arg)
    }
  }
}
