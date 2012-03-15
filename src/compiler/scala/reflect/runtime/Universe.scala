package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}

/** The universe for standard runtime reflection from Java.
 *  This type implements all abstract term members in internal.SymbolTable.
 *  It also provides methods to go from Java members to Scala members,
 *  using the code in JavaConversions.
 */
class Universe extends SymbolTable {

  type AbstractFileType = AbstractFile

  def picklerPhase = SomePhase

  val gen = new TreeGen { val global: Universe.this.type = Universe.this }

  lazy val settings = new Settings
  def forInteractive = false
  def forScaladoc = false

  val phaseWithId: Array[Phase] = Array(NoPhase, SomePhase)
  val currentRunId = 1 // fake a run id so that it is different from NoRunId
  phase = SomePhase // set to a phase different from NoPhase

  def log(msg: => AnyRef): Unit = println(" [] "+msg)

  type TreeCopier = TreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  def focusPos(pos: Position) = pos
  def isRangePos(pos: Position) = false
  def showPos(pos: Position) = "<unknown position>"

  type Position = String // source file?
  val NoPosition = ""

  definitions.AnyValClass // force it.

  // establish root association to avoid cyclic dependency errors later
  classToScala(classOf[java.lang.Object]).initialize

//  println("initializing definitions")
  definitions.init()

}
