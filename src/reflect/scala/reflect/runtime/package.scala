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

/** Entry points into runtime reflection.
 *  See [[scala.reflect.api.package the overview page]] for details on how to use them.
 */
package object runtime {

  /** The entry point into Scala runtime reflection.
   *
   * To use Scala runtime reflection, simply use or import `scala.reflect.runtime.universe._`
   *
   * See [[scala.reflect.api.Universe]] or the
   * [[http://docs.scala-lang.org/overviews/reflection/environment-universes-mirrors.html Reflection Guide: Universes]]
   * for more details.
   */
  lazy val universe: api.JavaUniverse = new runtime.JavaUniverse

  /** The runtime reflection mirror that corresponds to the current lexical context.
   *  It's typically equivalent to `universe.runtimeMirror(getClass.getClassLoader)` invoked at the call site.
   */
  // implementation hardwired to the `currentMirror` method below
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  def currentMirror: universe.Mirror = macro ???
}

package runtime {
  private[scala] object Macros {
    def currentMirror(c: scala.reflect.macros.blackbox.Context): c.Expr[universe.Mirror] = {
      import c.universe._
      val runtimeClass = c.reifyEnclosingRuntimeClass
      if (runtimeClass.isEmpty) c.abort(c.enclosingPosition, "call site does not have an enclosing class")
      val scalaPackage = Select(Ident(TermName("_root_")), TermName("scala"))
      val runtimeUniverse = Select(Select(Select(scalaPackage, TermName("reflect")), TermName("runtime")), TermName("universe"))
      val currentMirror = Apply(Select(runtimeUniverse, TermName("runtimeMirror")), List(Select(runtimeClass, TermName("getClassLoader"))))
      c.Expr[Nothing](currentMirror)(c.WeakTypeTag.Nothing)
    }
  }
}
