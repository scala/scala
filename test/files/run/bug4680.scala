trait A extends DelayedInit {
  print("-A ")

  def delayedInit(body: => Unit) = {
    body
    postConstructionCode
  }
  protected def postConstructionCode: Unit = {
    print("\nA+ ")
  }
}
trait B extends A {
  print("-B ")
  override protected def postConstructionCode: Unit = {
    super.postConstructionCode
    print("B+ ")
  }
}

trait C extends B {
  print("-C ")
  override protected def postConstructionCode: Unit = {
    super.postConstructionCode
    print("C+ ")
  }
}

class D() extends C  {
  print("-D ")
  override protected def postConstructionCode: Unit = {
    super.postConstructionCode
    print("D+ ")
  }
}
class E() extends D() {
  println("-E")
  override protected def postConstructionCode: Unit = {
    super.postConstructionCode
    println("E+")
  }
}


object Test {
  def p(msg: String) = println("\n\n// " + msg)

  def main(args: Array[String]) {
    p("new C { }")
    new C { }
    p("new C { 5 }")
    new C { 5 }

    p("new D()")
    new D()
    p("new D() { }")
    new D() { }
    p("new D() { val x = 5 }")
    new D() { val x = 5 }
    p("new { val x = 5 } with D()")
    new { val x = 5 } with D()

    p("new E() { val x = 5 }")
    new E() { val x = 5 }
    p("new { val x = 5 } with E()")
    new { val x = 5 } with E()
    p("new { val x = 5 } with E() { }")
    new { val x = 5 } with E() { }
    p("new { val x = 5 } with E() { 5 }")
    new { val x = 5 } with E() { 5 }
  }
}
