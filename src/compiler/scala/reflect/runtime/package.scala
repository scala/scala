package scala.reflect

import language.experimental.macros

package object runtime {

  // type is api.JavaUniverse because we only want to expose the `scala.reflect.api.*` subset of reflection
  lazy val universe: api.JavaUniverse = new runtime.JavaUniverse

  // [Eugene++ to Martin] removed `mirrorOfLoader`, because one can use `universe.runtimeMirror` instead

  def currentMirror: universe.Mirror = macro Macros.currentMirror
}

package runtime {
  object Macros {
    def currentMirror(c: scala.reflect.makro.Context): c.Expr[universe.Mirror] = {
      import c.universe._
      val runtimeClass = c.reifyEnclosingRuntimeClass
      if (runtimeClass.isEmpty) c.abort(c.enclosingPosition, "call site does not have an enclosing class")
      val runtimeUniverse = Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")), newTermName("runtime")), newTermName("universe"))
      val currentMirror = Apply(Select(runtimeUniverse, newTermName("runtimeMirror")), List(Select(runtimeClass, newTermName("getClassLoader"))))
      c.Expr[Nothing](currentMirror)(c.TypeTag.Nothing)
    }
  }
}
