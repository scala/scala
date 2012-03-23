trait A extends DelayedInit
{
  print("-A")

  def delayedInit(body: => Unit) = {
    body
    postConstructionCode
  }
  def postConstructionCode: Unit = {
    print("\n A+")
  }
}
trait B extends A {
  print(" -B")
  override def postConstructionCode: Unit = {
    super.postConstructionCode
    print(" B+")
  }
}

trait C extends B {
  print(" -C")
  override def postConstructionCode: Unit = {
    super.postConstructionCode
    print(" C+")
  }
}

class D() extends C  {
  print(" -D")
  override def postConstructionCode: Unit = {
    super.postConstructionCode
    print(" D+")
  }
}
class E() extends D() {
  print(" -E")
  override def postConstructionCode: Unit = {
    super.postConstructionCode
    print(" E+")
  }
}

object Test {
  def p(msg: String) = println("\n\n// " + msg)

  def main(args: Array[String]) {
    val f: A => Unit = _ => ()

    p("new C { }")
    f(new C { })
    p("new C { 5 }")
    f(new C { 5 })

    p("new D()")
    f(new D())
    p("new D() { }")
    f(new D() { })

    p("new D() { val x = 5 }")
    f(new D() { val x = 5 })
    p("new { val x = 5 } with D()")
    f(new { val x = 5 } with D())

    p("new E() { val x = 5 }")
    f(new E() { val x = 5 })
    p("new { val x = 5 } with E()")
    f(new { val x = 5 } with E())

    p("new { val x = 5 } with E() { }")
    f(new { val x = 5 } with E() { })
    p("new { val x = 5 } with E() { 5 }")
    f(new { val x = 5 } with E() { 5 })

    println("")
  }
}
