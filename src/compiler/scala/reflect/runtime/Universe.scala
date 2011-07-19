package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}

/** The universe for standard runtime reflection from Java.
 *  This type implements all abstract term members in internal.SymbolTable.
 *  It also provides methods to go from Java members to Scala members,
 *  using the code in JavaConversions.
 */
class Universe extends internal.SymbolTable with JavaToScala with ScalaToJava with Loaders {

  type AbstractFileType = AbstractFile

  def picklerPhase = SomePhase

  val gen = new TreeGen { val global: Universe.this.type = Universe.this }

  def settings = new Settings
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
}

object Universe extends Universe

/** test code; should go to tests once things settle down a bit
 */
object Test extends Universe with App {
  val sym = classToScala(classOf[scala.collection.Iterable[_]])
  println(sym)
  println("parents = "+sym.info.parents)
  println("decls = "+(sym.info.decls.toList map (_.defString)))
  val ms = sym.info.members.toList map (_.initialize)
  println("members = "+(ms map (_.defString) mkString ("\n  ")))
}