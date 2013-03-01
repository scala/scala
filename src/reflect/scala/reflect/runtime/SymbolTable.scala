package scala.reflect
package runtime

import scala.reflect.internal.Flags._

/**
 *  This symbol table trait fills in the definitions so that class information is obtained by refection.
 *  It can be used either from a reflexive universe (class scala.reflect.runtime.JavaUniverse), or else from
 *  a runtime compiler that uses reflection to get a class information (class scala.tools.reflect.ReflectGlobal)
 */
private[scala] trait SymbolTable extends internal.SymbolTable with JavaMirrors with SymbolLoaders with SynchronizedOps {

  def info(msg: => String) =
    if (settings.verbose.value) println("[reflect-compiler] "+msg)

  def debugInfo(msg: => String) =
    if (settings.debug.value) info(msg)

  /** Declares that this is a runtime reflection universe.
   *
   *  This means that we can make certain assumptions to optimize the universe.
   *  For example, we may auto-initialize symbols on flag and annotation requests
   *  (see `shouldTriggerCompleter` below for more details).
   *
   *  On the other hand, this also means that usage scenarios of the universe
   *  will differ from the conventional ones. For example, we have to do additional cleanup
   *  in order to prevent memory leaks: http://groups.google.com/group/scala-internals/browse_thread/thread/eabcf3d406dab8b2.
   */
  override def isCompilerUniverse = false

  /** Unlike compiler universes, reflective universes can auto-initialize symbols on flag requests.
   *
   *  scalac wasn't designed with such auto-initialization in mind, and quite often it makes assumptions
   *  that flag requests won't cause initialization. Therefore enabling auto-init leads to cyclic errors.
   *  We could probably fix those, but at the moment it's too risky.
   *
   *  Reflective universes share codebase with scalac, but their surface is much smaller, which means less assumptions.
   *  These assumptions are taken care of in this overriden `shouldTriggerCompleter` method.
   */
  override protected def shouldTriggerCompleter(symbol: Symbol, completer: Type, isFlagRelated: Boolean, mask: Long) =
    completer match {
      case _: TopClassCompleter | _: JavaClassCompleter => !isFlagRelated || (mask & TopLevelPickledFlags) != 0
      case _ => super.shouldTriggerCompleter(symbol, completer, isFlagRelated, mask)
    }
}
