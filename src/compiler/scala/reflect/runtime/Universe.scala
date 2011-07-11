package scala.reflect
package runtime

class Universe extends internal.SymbolTable {

  type AbstractFileType = AbstractFile

  def picklerPhase = internal.NoPhase

  //val treePrinter = null

  val gen = new internal.TreeGen { val global: Universe.this.type = Universe.this }

  def settings = null
  def forInteractive = false
  def forScaladoc = false

  val phaseWithId: Array[internal.Phase] = Array()
  val currentRunId = 0
  def log(msg: => AnyRef): Unit = println(" [] "+msg)
  def rootLoader = null // not needed because RootClass will get a PackageType in Definitions anyway.

  private def packageType(clazz: Symbol) = new ClassInfoType(List(), newScope, clazz)

  definitions.RootClass.setInfo(packageType(definitions.RootClass))

  type TreeCopier = TreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  def focusPos(pos: Position) = pos
  def isRangePos(pos: Position) = false
  def showPos(pos: Position) = "<unknown position>"

  type Position = String // source file?
  val NoPosition = ""
}
