package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}

/** An implementation of [[scala.reflect.api.Universe]] for runtime reflection using JVM classloaders.
 *
 *  Should not be instantiated directly, use [[scala.reflect.runtime.universe]] instead.
 *
 *  @contentDiagram hideNodes "*Api" "*Extractor"
 */
class JavaUniverse extends internal.SymbolTable with ReflectSetup with runtime.SymbolTable { self =>

  def picklerPhase = SomePhase

  lazy val settings = new Settings
  def forInteractive = false
  def forScaladoc = false

  def log(msg: => AnyRef): Unit = if (settings.debug.value) println(" [] "+msg)

  type TreeCopier = InternalTreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  init()
}

