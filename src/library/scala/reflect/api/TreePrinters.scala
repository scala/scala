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

  // emits more or less verbatim representation of the provided tree
  // todo. when LiftCode becomes a macro, throw this code away and use that macro
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
        parts += "Set(" + mods.modifiers.map(_.sourceString).mkString(", ") + ")"
        parts += "newTypeName(\"" + mods.privateWithin.toString + "\")"
        parts += "List(" + mods.annotations.map{showRaw}.mkString(", ") + ")"
        
        var keep = 3
        if (keep == 3 && mods.annotations.isEmpty) keep -= 1
        if (keep == 2 && mods.privateWithin == EmptyTypeName) keep -= 1
        if (keep == 1 && mods.modifiers.isEmpty) keep -= 1
        
        print("Modifiers(", parts.take(keep).mkString(", "), ")")
      case name: Name =>
        if (name.isTermName) print("newTermName(\"") else print("newTypeName(\"")
        print(name.toString)
        print("\")")
      case arg =>
        out.print(arg)
    }
  }
}
