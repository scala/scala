class C(a: Any)
object F {
  def byname(a: => Any) = println(a)
  def hof(a: () => Any) = println(a())
}

class COkay extends C(0) {
  def this(a: Any) {
    this()
    def x = "".toString
    F.byname(x)
  }
}

//
// The thunk's apply method accesses the MODULE$
// field before it is set.
//
//   0: getstatic #23; //Field O1$.MODULE$:LO1$;
//   3: invokevirtual #26; //Method O1$.O1$$x$1:()Ljava/lang/String;
object O1 extends C({
  def x = "".toString
  F.byname(x)
})

// java.lang.NullPointerException
//   at O2$$anonfun$$init$$1.apply(<console>:11)
object O2 extends C({
  lazy val x = "".toString
  F.byname(x)
})

// java.lang.NullPointerException
//   at O3$$anonfun$$init$$1.apply(<console>:11)
object O3 extends C({
  def x = "".toString
  F.hof(() => x)
})

// Okay, the nested classes don't get an outer pointer passed,
// just an extra param for `x: String`.
object O6 extends C({
  val x = "".toString
  F.byname(x); F.hof(() => x); (new { val xx = x }.xx)
})


class C1 extends C({
  def x = "".toString
  F.byname(x)
})
class C2 extends C({
  lazy val x = "".toString
  F.byname(x)
})
class C3 extends C({
  def x = "".toString
  F.hof(() => x)
})
class C4 extends C({
  def x = "".toString
  object Nested { def xx = x}
  Nested.xx
})

// okay, for same reason as O6
class C6 extends C({
  val x = "".toString
  F.byname(x); F.hof(() => x); (new { val xx = x }.xx)
})

class C11(a: Any) {
  def this() = {
    this({
     def x = "".toString
      F.byname(x)
    })
  }
}

// Crashes earlier in lazyVals.
// class C12(a: Any) {
//   def this() = {
//     this({
//       lazy val x = "".toString
//       F.byname(x)
//     })
//   }
// }

class C13(a: Any) {
  def this() = {
    this({
      def x = "".toString
      F.hof(() => x)
    })
  }
}

class C14(a: Any) {
  def this() = {
    this({
      def x = "".toString
      object Nested { def xx = x}
      Nested.xx
    })
  }
}

class COuter extends C({
  def foo = 0
  class CInner extends C({foo})
})


class CEarly(a: Any) extends {
  val early = {def x = "".toString
    object Nested { def xx = x}
    Nested.xx
  }
} with AnyRef