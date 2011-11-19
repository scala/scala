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

  def show(tree: Tree, mkPrinter: PrintWriter => TreePrinter = newTreePrinter): String = {
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
          print(if (it.hasNext) ", " else ")")
        }
        if (typesPrinted)
          print(".setType(", tree.tpe, ")")
      case arg =>
        out.print(arg)
    }
  }
}
