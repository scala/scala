package scala.reflect.runtime.internal

import scala.reflect.internal.{SomePhase, NoPhase, Phase, TreeGen}

private[reflect] class JavaUniverse extends scala.reflect.internal.SymbolTable with ReflectSetup with scala.reflect.runtime.internal.SymbolTable { self =>

  def picklerPhase = SomePhase

  lazy val settings = new Settings
  def forInteractive = false
  def forScaladoc = false

  def log(msg: => AnyRef): Unit = println(" [] "+msg)

  type TreeCopier = InternalTreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  init()
}

