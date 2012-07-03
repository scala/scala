package scala.reflect
package api

import java.io.{ PrintWriter, StringWriter }

trait Printers { self: Universe =>

  trait TreePrinter {
    def print(args: Any*)
    protected var printTypes = false
    protected var printIds = false
    protected var printKinds = false
    protected var printMirrors = false
    def withTypes: this.type = { printTypes = true; this }
    def withoutTypes: this.type = { printTypes = false; this }
    def withIds: this.type = { printIds = true; this }
    def withoutIds: this.type = { printIds = false; this }
    def withKinds: this.type = { printKinds = true; this }
    def withoutKinds: this.type = { printKinds = false; this }
    def withMirrors: this.type = { printMirrors = true; this }
    def withoutMirrors: this.type = { printMirrors = false; this }
  }

  case class BooleanFlag(val value: Option[Boolean])
  object BooleanFlag {
    import language.implicitConversions
    implicit def booleanToBooleanFlag(value: Boolean): BooleanFlag = BooleanFlag(Some(value))
    implicit def optionToBooleanFlag(value: Option[Boolean]): BooleanFlag = BooleanFlag(value)
  }

  protected def render(what: Any, mkPrinter: PrintWriter => TreePrinter, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)
    var printer = mkPrinter(writer)
    printTypes.value.map(printTypes => if (printTypes) printer.withTypes else printer.withoutTypes)
    printIds.value.map(printIds => if (printIds) printer.withIds else printer.withoutIds)
    printKinds.value.map(printKinds => if (printKinds) printer.withKinds else printer.withoutKinds)
    printMirrors.value.map(printMirrors => if (printMirrors) printer.withMirrors else printer.withoutMirrors)
    printer.print(what)
    writer.flush()
    buffer.toString
  }

  /** By default trees are printed with `show` */
  override protected def treeToString(tree: Tree) = show(tree)

  /** Renders a prettified representation of a reflection artifact.
   *  Typically it looks very close to the Scala code it represents.
   */
  def show(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String =
    render(any, newTreePrinter(_), printTypes, printIds, printKinds, printMirrors)

  /** Hook to define what `show(...)` means.
   */
  def newTreePrinter(out: PrintWriter): TreePrinter

  /** Renders internal structure of a reflection artifact.
   */
  def showRaw(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String =
    render(any, newRawTreePrinter(_), printTypes, printIds, printKinds, printMirrors)

  /** Hook to define what `showRaw(...)` means.
   */
  def newRawTreePrinter(out: PrintWriter): TreePrinter

  /** Renders a prettified representation of a name.
   */
  def show(name: Name): String

  /** Renders internal structure of a name.
   */
  def showRaw(name: Name): String = name.toString

  /** Renders a prettified representation of a flag set.
   */
  def show(flags: FlagSet): String

  /** Renders internal structure of a flag set.
   */
  def showRaw(flags: FlagSet): String = flags.toString
}
