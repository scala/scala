class C(val x: AnyRef, val y: AnyRef)
class D(val x: AnyRef, val y: AnyRef) {
  val z: AnyRef = ""
}

trait U {
  val a = b
  def b: AnyRef
}

abstract class V {
  val a = b
  def b: AnyRef
}

object Test {
  def check(f: => Unit): Unit = try {
    f
    println("!!!")
  } catch {
    case e: UninitializedFieldError =>
      println(e.getMessage)
  }
  def main(args: Array[String]): Unit = {
    check {
      class U1 extends U { val b = "" }
      new U1
    }
    check {
      class U1 extends V { val b = "" }
      new U1
    }

    new C("", "")
    assert(classOf[C].getDeclaredFields.size == 2) // no bitmp field when we just have paramaccessors
    new D("", "")
  }
}
