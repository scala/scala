/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.macros
package runtime

import scala.reflect.internal.Flags._
import scala.reflect.runtime.ReflectionUtils

trait MacroRuntimes extends JavaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  import global._
  import definitions._

  /** Produces a function that can be used to invoke macro implementation for a given macro definition:
   *    1) Looks up macro implementation symbol in this universe.
   *    2) Loads its enclosing class from the macro classloader.
   *    3) Loads the companion of that enclosing class from the macro classloader.
   *    4) Resolves macro implementation within the loaded companion.
   *
   *  @return Requested runtime if macro implementation can be loaded successfully from either of the mirrors,
   *          `null` otherwise.
   */
  def macroRuntime(expandee: Tree): MacroRuntime = pluginsMacroRuntime(expandee)

  /** Default implementation of `macroRuntime`.
   *  Can be overridden by analyzer plugins (see AnalyzerPlugins.pluginsMacroRuntime for more details)
   */
  private val macroRuntimesCache = perRunCaches.newWeakMap[Symbol, MacroRuntime]()
  def standardMacroRuntime(expandee: Tree): MacroRuntime = {
    val macroDef = expandee.symbol
    macroLogVerbose(s"looking for macro implementation: $macroDef")
    if (fastTrack contains macroDef) {
      macroLogVerbose("macro expansion is serviced by a fast track")
      fastTrack(macroDef)
    } else {
      macroRuntimesCache.getOrElseUpdate(macroDef, new MacroRuntimeResolver(macroDef).resolveRuntime())
    }
  }

  /** Macro classloader that is used to resolve and run macro implementations.
   *  Loads classes from from -cp (aka the library classpath).
   *  Is also capable of detecting REPL and reusing its classloader.
   */
  private lazy val defaultMacroClassloaderCache: () => ClassLoader = perRunCaches.newGeneric(findMacroClassLoader())
  def defaultMacroClassloader: ClassLoader = defaultMacroClassloaderCache()

  /** Abstracts away resolution of macro runtimes.
   */
  type MacroRuntime = MacroArgs => Any
  class MacroRuntimeResolver(val macroDef: Symbol) extends JavaReflectionResolvers {
    val binding = loadMacroImplBinding(macroDef).get
    val isBundle = binding.isBundle
    val className = binding.className
    val methName = binding.methName

    def resolveRuntime(): MacroRuntime = {
      if (className == Predef_???.owner.javaClassName && methName == Predef_???.name.encoded) {
        args => throw new AbortMacroException(args.c.enclosingPosition, "macro implementation is missing")
      } else {
        try {
          macroLogVerbose(s"resolving macro implementation as $className.$methName (isBundle = $isBundle)")
          macroLogVerbose(s"classloader is: ${ReflectionUtils.show(defaultMacroClassloader)}")
          resolveJavaReflectionRuntime(defaultMacroClassloader)
        } catch {
          case ex: Exception =>
            macroLogVerbose(s"macro runtime failed to load: ${ex.toString}")
            macroDef setFlag IS_ERROR
            null
        }
      }
    }
  }
}
