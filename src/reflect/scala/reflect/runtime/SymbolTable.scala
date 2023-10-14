/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package runtime

/**
 *  This symbol table trait fills in the definitions so that class information is obtained by reflection.
 *  It can be used either from a reflexive universe (class scala.reflect.runtime.JavaUniverse), or else from
 *  a runtime compiler that uses reflection to get a class information (class scala.tools.reflect.ReflectGlobal)
 */
private[scala] trait SymbolTable extends internal.SymbolTable with JavaMirrors with SymbolLoaders with SynchronizedOps with Gil with ThreadLocalStorage {

  def info(msg: => String) =
    if (settings.verbose.value) println("[reflect-compiler] "+msg)

  def debugInfo(msg: => String) =
    if (settings.isDebug) info(msg)

  /** Declares that this is a runtime reflection universe.
   *
   *  This means that we can make certain assumptions to optimize the universe.
   *  For example, we may auto-initialize symbols on flag and annotation requests
   *  (see `shouldTriggerCompleter` below for more details).
   *
   *  On the other hand, this also means that usage scenarios of the universe
   *  will differ from the conventional ones. For example, we have to do additional cleanup
   *  in order to prevent memory leaks: https://groups.google.com/group/scala-internals/browse_thread/thread/eabcf3d406dab8b2.
   */
  override def isCompilerUniverse = false
}
