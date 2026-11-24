//> using options -nowarn
object Test {
  val log = new collection.mutable.ArrayBuffer[String]
  class VU(val u: Unit) extends AnyVal
  object A { log += "A initialised"; val done: Unit = () }
  object B { log += "B initialised"; val done: Unit = () }
  object C { log += "C initialised"; val done: Unit = () }
  object D { log += "D initialised"; val done: Unit = () }
  object E { log += "E initialised"; val done: Int = 0 }
  object F { log += "F initialised"; val done: Unit = () }
  object G { log += "G initialised"; val done: Unit = () }
  object H { log += "H initialised"; val done: Unit = () }
  object I { log += "I initialised"; val done: Unit = () }
  object J { log += "J initialised"; val done: Unit = () }
  lazy val lu: Unit = log += "lu initialised"

  // a val implemented by a lazy val: nothing in the selection `t.v` says that it has an effect
  trait T { val v: Unit }
  class L extends T { lazy val v: Unit = log += "L.v initialised" }
  class K { val done: Unit = () }

  // a Unit that is boxed in the field, and unboxed where it is selected
  class Gen[X](val x: X)
  object M extends Gen[Unit](()) { log += "M initialised" }
  class N extends Gen[Unit](())
  def unboxM: Unit = M.x
  def unboxN(n: N): Unit = n.x

  def npe(name: String)(body: => Any): Unit =
    try { body; log += s"$name: no exception" }
    catch { case _: NullPointerException => log += s"$name: NullPointerException" }

  // Selecting a val evaluates its prefix and calls its getter, also when the Unit value is boxed or unboxed.
  def main(args: Array[String]): Unit = {
    A.done                        // statement
    val b: Any = B.done           // boxed
    identity(C.done)              // generic argument
    val d = List(D.done)
    val e: Any = E.done           // Int instead of Unit
    val f = new VU(F.done)
    G.done.toString               // boxed receiver
    H.done == ()
    val j = (I.done, 1)
    J.done match { case () => 1 }
    val l: Any = lu
    val t: T = new L
    val tv: Any = t.v
    val k: K = null
    npe("boxing null.done") { val x: Any = k.done; x }
    unboxM
    npe("unboxing null.x")(unboxN(null))
    log.foreach(println)
  }
}
