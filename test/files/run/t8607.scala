package p1 {
  private[p1] trait B extends Any {
    def a: Any = ""
  }
 
  class C(val value: Int) extends AnyVal with B {
    // def b = ""
  }
}

object Test {
  def main(args: Array[String]) {
    val c = new p1.C(42)
    c.a
    /*
    new p1.C.<init>(
      c.$asInstanceOf[scala.this.Int]()
    ).a();


    new p1.C.<init>(
      new p1.C.<init>(
        c.$asInstanceOf[scala.this.Int]()
      ).$asInstanceOf[ErasedValueType(class C, scala.this.Int)]()
       .$asInstanceOf[scala.this.Int]()
    ).a();

    new p1.C.<init>(
      new p1.C.<init>(c)
        .$asInstanceOf[scala.this.Int]()
        .$asInstanceOf[scala.this.Int]()
      ).a();

    */
  }
}
