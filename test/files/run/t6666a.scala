class A(a: Any)

object Test {
  def main(args: Array[String]): Unit = {
  }

  val x: Unit = {
    object InVal extends A({
      new {}           // okay
      val o = {new {}} // nesting triggers a VerifyError.
      null
    });
    InVal;
    ()
  };
}
