object Test {
  // this works.
  class B { final def f(): Unit = () }
  trait A extends B { def f (): Unit }
  class BB extends B { def f (): Unit }

  // this earns a VerifyError.
  trait C { def wait (): Unit }
  class D { }
  
  def main(args: Array[String]): Unit = {
    new B with A { }
    new BB
//    new D with C { }
  }
}
