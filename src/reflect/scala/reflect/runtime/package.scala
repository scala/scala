package scala.reflect

package object runtime {

  // type is api.JavaUniverse because we only want to expose the `scala.reflect.api.*` subset of reflection
  lazy val universe: api.JavaUniverse = new runtime.JavaUniverse

  // implementation hardwired to the `currentMirror` method below
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  def currentMirror: universe.Mirror = ??? // macro
}

package runtime {
  object Macros {
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
