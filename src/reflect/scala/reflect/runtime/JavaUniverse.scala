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

  def forInteractive = false
  def forScaladoc = false
  lazy val settings = new Settings
  private val isLogging = sys.props contains "scala.debug.reflect"

  def log(msg: => AnyRef): Unit = if (isLogging) Console.err.println("[reflect] " + msg)

  type TreeCopier = InternalTreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  init()
}

