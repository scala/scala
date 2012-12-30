import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag, V: c.WeakTypeTag](c: Ctx) = {
    import c.universe._
    val name = TypeName("C_" + c.weakTypeOf[T].toString + "_" + c.weakTypeOf[U].toString + "_" + c.weakTypeOf[V].toString)
    c.topLevelRef(name) orElse {
      val Block(List(dummy: ClassDef), _) = reify{ class DUMMY }.tree
      val synthetic = ClassDef(NoMods, name, Nil, dummy.impl)
      c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, synthetic)
    }
  }
}

class D[T] {
  class C[U] {
    type Foo[V] = macro Impls.foo[T, U, V]
  }
}
