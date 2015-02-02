object Test extends App {
  object stubUpdate {
    def update(as: Any*) = println(".update"+as.toList.mkString("(",", ", ")"))
  }

  object stub {
    def apply = {println(".apply"); stubUpdate}
    def apply(as: Any*) = println(".apply"+as.toList.mkString("(",", ", ")"))
    def update(as: Any*) = println(".update"+as.toList.mkString("(",", ", ")"))
  }
  class MyDynamic extends Dynamic {
    def applyDynamic[T](n: String)(as: Any*) = {println("qual.applyDynamic("+ n +")"+ as.toList.mkString("(",", ", ")")); stub}
    def applyDynamicNamed[T](n: String)(as: (String, Any)*) = {println("qual.applyDynamicNamed("+ n +")"+ as.toList.mkString("(",", ", ")")); stub}
    def selectDynamic[T](n: String) = {println("qual.selectDynamic("+ n +")"); stub}
    def updateDynamic(n: String)(x: Any): Unit = {println("qual.updateDynamic("+ n +")("+ x +")")}
  }
  val qual = new MyDynamic
  val expr = "expr"
  val a = "a"
  val a2 = "a2"
  type T = String

  // If qual.sel is followed by a potential type argument list [Ts] and an argument list (arg1, …, argn) where none of the arguments argi are named:
  //  qual.applyDynamic(“sel”)(arg1, …, argn)
  qual.sel()
  qual.sel(a)
  // qual.sel(a, a2: _*) -- should not accept varargs?
  qual.sel(a)(a2)
  qual.sel[T](a)
  qual.sel[T](a)(a2)

  // If qual.sel is followed by a potential type argument list [Ts]
  // and a non-empty named argument list (x1 = arg1, …, xn = argn) where some name prefixes xi = might be missing:
  //  qual.applyDynamicNamed(“sel”)(xs1 -> arg1, …, xsn -> argn)
  qual.sel(arg = a)
  qual.sel[T](arg = a)
  qual.sel(a, arg2 = "a2")
  // qual.sel(a)(a2, arg2 = "a2")
  // qual.sel[T](a)(a2, arg2 = "a2")
  // qual.sel(arg = a, a2: _*)
  // qual.sel(arg, arg2 = "a2", a2: _*)

  // If qual.sel appears immediately on the left-hand side of an assignment
  // qual.updateDynamic(“sel”)(expr)
  qual.sel = expr

  // If qual.sel, possibly applied to type arguments, but is
  // not applied to explicit value arguments,
  // nor immediately followed by an assignment operator:
  // qual.selectDynamic[Ts](“sel”)
  qual.sel
  qual.sel[T]

  qual.sel(1) = expr // parser turns this into qual.sel.update(1, expr)
  qual.sel() = expr  // parser turns this into qual.sel.update(expr)
  qual.sel.apply(1)
  qual.sel.apply(1) = 1

  qual.apply(a)
  qual.apply[String](a)
  qual(a)
  qual[String](a)
  qual[T](arg = a)
  qual(a, arg2 = "a2")
  qual(a) = a2
}
