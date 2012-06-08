package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}

/** The universe for standard runtime reflection from Java.
 *  This type implements all abstract term members in internal.SymbolTable.
 */
class JavaUniverse extends internal.SymbolTable with ReflectSetup with runtime.SymbolTable { self =>

  type AbstractFileType = AbstractFile

  def picklerPhase = SomePhase

  type TreeGen = internal.TreeGen

  override type Position = scala.reflect.internal.util.Position

  override val gen = new TreeGen { val global: self.type = self }

  lazy val settings = new Settings
  def forInteractive = false
  def forScaladoc = false

  def log(msg: => AnyRef): Unit = println(" [] "+msg)

  type TreeCopier = TreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  init()
}

