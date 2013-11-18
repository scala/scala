class AnyVals {
  def f1 = (5: Any).getClass
  def f2 = (5: AnyVal).getClass
  def f3 = 5.getClass
  def f4 = (5: java.lang.Integer).getClass
  def f5 = (5.asInstanceOf[AnyRef]).getClass

  // scalap says:
  //
  // def f1 : java.lang.Class[?0] forSome {type ?0} = { /* compiled code */ }
  // def f2 : java.lang.Class[?0] forSome {type ?0} = { /* compiled code */ }
  // def f3 : java.lang.Class[scala.Int] = { /* compiled code */ }
  // def f4 : java.lang.Class[?0] forSome {type ?0 <: java.lang.Integer} = { /* compiled code */ }
  // def f5 : java.lang.Class[?0] forSome {type ?0 <: scala.AnyRef} = { /* compiled code */ }
  //
  // java generic signature says:
  //
  // f1: java.lang.Class<?>
  // f2: java.lang.Class<?>
  // f3: java.lang.Class<java.lang.Object>
  // f4: java.lang.Class<? extends java.lang.Integer>
  // f5: java.lang.Class<?>
}

class AnyRefs {
  class A
  class B extends A

  def f1 = (new B: Any).getClass().newInstance()
  def f2 = (new B: AnyRef).getClass().newInstance()
  def f3 = (new B: A).getClass().newInstance()
  def f4 = (new B: B).getClass().newInstance()

  def f0[T >: B] = (new B: T).getClass().newInstance()

  def f5 = f0[Any]
  def f6 = f0[AnyRef]
  def f7 = f0[A]
  def f8 = f0[B]
}

class MoreAnyRefs {
  trait A
  trait B

  // don't leak anon/refinements
  def f1 = (new A with B { }).getClass()
  def f2 = (new B with A { }).getClass()
  def f3 = (new { def bippy() = 5 }).getClass()
  def f4 = (new A { def bippy() = 5 }).getClass()
}

@deprecated("Suppress warnings", since="2.11")
object Test {
  def returnTypes[T: Manifest] = (
    manifest[T].runtimeClass.getMethods.toList
      filter (_.getName startsWith "f")
      sortBy (_.getName)
      map (m => m.getName + ": " + m.getGenericReturnType.toString)
  )

  def main(args: Array[String]): Unit = {
    returnTypes[AnyVals] foreach println
    returnTypes[AnyRefs] foreach println
    returnTypes[MoreAnyRefs] foreach println
  }
}
