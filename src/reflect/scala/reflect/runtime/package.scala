package scala.reflect

/** Entry points into runtime reflection.
 *  See [[scala.reflect.api.package the overview page]] for details on how to use them.
 */
package object runtime {

  /** The entry point into runtime reflection.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use it.
   */
  lazy val universe: api.JavaUniverse = new runtime.JavaUniverse

  /** The runtime reflection mirror that corresponds to the current lexical context.
   *  Is typically equivalent to `universe.runtimeMirror(getClass.getClassLoader)` invoked at the call site.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use it.
   */
  // implementation hardwired to the `currentMirror` method below
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  def currentMirror: universe.Mirror = ??? // macro
}

package runtime {
  private[scala] object Macros {
    def currentMirror(c: scala.reflect.macros.Context): c.Expr[universe.Mirror] = {
      import c.universe._
      val runtimeClass = c.reifyEnclosingRuntimeClass
      if (runtimeClass.isEmpty) c.abort(c.enclosingPosition, "call site does not have an enclosing class")
      val runtimeUniverse = Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")), newTermName("runtime")), newTermName("universe"))
      val currentMirror = Apply(Select(runtimeUniverse, newTermName("runtimeMirror")), List(Select(runtimeClass, newTermName("getClassLoader"))))
      c.Expr[Nothing](currentMirror)(c.WeakTypeTag.Nothing)
    }
  }
}
