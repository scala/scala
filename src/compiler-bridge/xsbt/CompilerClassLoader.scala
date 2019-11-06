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

package xsbt

import java.net.{URL, URLClassLoader}
import java.util

/**
  * A classloader to run the compiler.
  *
  * A CompilerClassLoader is constructed from a list of `urls` that need to be on
  * the classpath to run the compiler and the classloader used by sbt.
  *
  * To understand why a custom classloader is needed for the compiler, let us
  * describe some alternatives that wouldn't work.
  *
  *   - `new URLClassLoader(urls)`:
  *      The compiler contains sbt phases that callback to sbt using the `xsbti.*`
  *      interfaces. If `urls` does not contain the sbt interfaces we'll get a
  *      `ClassNotFoundException` in the compiler when we try to use them, if
  *      `urls` does contain the interfaces we'll get a `ClassCastException` or a
  *      `LinkageError` because if the same class is loaded by two different
  *      classloaders, they are considered distinct by the JVM.
  *   - `new URLClassLoader(urls, sbtLoader)`:
  *      Because of the JVM delegation model, this means that we will only load
  *      a class from `urls` if it's not present in the parent `sbtLoader`, but
  *      sbt uses its own version of the scala compiler and scala library which
  *      is not the one we need to run the compiler.
  *
  * Our solution is to implement a subclass of URLClassLoader with no parent, instead
  * we override `loadClass` to load the `xsbti.*` interfaces from `sbtLoader`.
  */
object CompilerClassLoader {
  /**
    * Cache the result of `fixBridgeLoader`.
    *
    * Reusing ClassLoaders is important for warm performance since otherwise the
    * JIT code cache for the compiler will be discarded between every call to
    * the sbt `compile` task.
    */
    private val fixedLoaderCache = new util.WeakHashMap[ClassLoader, ClassLoader]

  /**
    * Fix the compiler bridge ClassLoader
    *
    * Soundtrack: https://www.youtube.com/watch?v=imamcajBEJs
    *
    * The classloader that we get from sbt looks like:
    *
    * URLClassLoader(bridgeURLs,
    * DualLoader(scalaLoader, notXsbtiFilter, sbtLoader, xsbtiFilter))
    *
    * DualLoader will load the `xsbti.*` interfaces using `sbtLoader` and
    * everything else with `scalaLoader`. Once we have loaded the dotty Main
    * class using `scalaLoader`, subsequent classes in the dotty compiler will
    * also be loaded by `scalaLoader` and _not_ by the DualLoader. But the sbt
    * compiler phases are part of dotty and still need access to the `xsbti.*`
    * interfaces in `sbtLoader`, therefore DualLoader does not work for us
    * (this issue is not present with scalac because the sbt phases are
    * currently defined in the compiler bridge itself, not in scalac).
    *
    * CompilerClassLoader is a replacement for DualLoader. Until we can fix
    * this in sbt proper, we need to use reflection to construct our own
    * fixed classloader:
    *
    * URLClassLoader(bridgeURLs,
    * CompilerClassLoader(scalaLoader.getURLs, sbtLoader))
    *
    * @param bridgeLoader The classloader that sbt uses to load the compiler bridge
    * @return A fixed classloader that works with dotty
    */
  def fixBridgeLoader(bridgeLoader: ClassLoader): ClassLoader = synchronized {
    fixedLoaderCache.computeIfAbsent(bridgeLoader, (k: ClassLoader) => computeFixedLoader(k))
  }

  private def computeFixedLoader(bridgeLoader: ClassLoader): ClassLoader = {
    val urlBridgeLoader = bridgeLoader.asInstanceOf[URLClassLoader]
    val dualLoader = urlBridgeLoader.getParent
    val dualLoaderClass = dualLoader.getClass
    // DualLoader.parentA and DualLoader.parentB are private
    val parentAField = dualLoaderClass.getDeclaredField("parentA")
    parentAField.setAccessible(true)
    val parentBField = dualLoaderClass.getDeclaredField("parentB")
    parentBField.setAccessible(true)
    val scalaLoader = parentAField.get(dualLoader).asInstanceOf[URLClassLoader]
    val sbtLoader = parentBField.get(dualLoader).asInstanceOf[URLClassLoader]
    val bridgeURLs = urlBridgeLoader.getURLs
    new URLClassLoader(bridgeURLs, new CompilerClassLoaderImpl(scalaLoader.getURLs, sbtLoader))
  }

  private class CompilerClassLoaderImpl(val urls: Array[URL], val sbtLoader: ClassLoader) extends URLClassLoader(urls, null) {
    override def loadClass(className: String, resolve: Boolean): Class[_] =
      if (className.startsWith("xsbti.")) {
        // We can't use the loadClass overload with two arguments because it's
        // protected, but we can do the same by hand (the classloader instance
        // from which we call resolveClass does not matter).
        val c = sbtLoader.loadClass(className)
        if (resolve) resolveClass(c)
        c
      } else
        super.loadClass(className, resolve)
  }
}

