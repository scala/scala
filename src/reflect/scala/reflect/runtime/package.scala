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
      val scalaPackage = Select(Ident(newTermName("_root_")), newTermName("scala"))
      val runtimeUniverse = Select(Select(Select(scalaPackage, newTermName("reflect")), newTermName("runtime")), newTermName("universe"))
      val currentMirror = Apply(Select(runtimeUniverse, newTermName("runtimeMirror")), List(Select(runtimeClass, newTermName("getClassLoader"))))
      c.Expr[Nothing](currentMirror)(c.WeakTypeTag.Nothing)
    }
  }
}
