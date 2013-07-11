trait T {
  val traitVal = ""
}

object O extends App with T {
  val vall = ""
  lazy val lazyy = ""
  def deff = ""

  println(vall)       // no warn
  new {
    println(vall)     // no warn
  }
}

object Client {
  println(O.vall)     // warn
  import O.vall
  println(vall)       // warn

  println(O.lazyy)    // no warn
  println(O.deff)     // no warn
  println(O.traitVal) // no warn
}

// Delayed init usage pattern from Specs2
// See: https://groups.google.com/d/msg/scala-sips/wP6dL8nIAQs/ogjoPE-MSVAJ
trait Before extends DelayedInit {
  def before()
  override def delayedInit(x: => Unit): Unit = { before; x }
}
object Spec {
  trait UserContext extends Before {
    def before() = ()
    val foo = "foo"
  }
  new UserContext {
    println(foo)                      // no warn
    println(this.foo)                 // no warn
    println({locally(()); this}.foo)  // warn (spurious, but we can't discriminate)
  }
}
