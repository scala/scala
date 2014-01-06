class A {
  private lazy val a1 = "a1"
  object B
  private lazy val a2 = "a2"


  @transient lazy val a3 = "a3"
  @transient private lazy val a4 = "a4"
  @transient lazy val a5 = "a5"
  @transient private lazy val a6 = "a6"

  final val a7 = "a7"
  private final val a8 = "a8"
  @transient final val a9 = "a9"




  def run = {
    println(a1)
    B
    println(a2)
    println(a3)
    println(a4)
    println(a5)
    println(a6)
    println(a7)
    println(a8)
    println(a9)
  }
}

class C extends A {
  private lazy val c1 = "c1"
  lazy val c2 = "c2"

  private lazy val c3 = "c3"

  @transient lazy val c4 = "c4"
  @transient private lazy val c5 = "c5"
  @transient lazy val c6 = "c6"
  @transient private lazy val c7 = "c7"
  lazy val c8 = "c8"

  final val c9 = "c9"
  private final val c10 = "c10"



  override def run = {
    super.run
    println(c1)
    println(c2)
    println(c3)
    println(c4)
    println(c5)
    println(c6)
    println(c7)
    println(c8)
    println(c9)
    println(c10)
  }
}

object Test extends App {
  (new C).run
}

