class Outer {
  def apply( position : Inner ) {}
  class Inner

  this.apply(new Inner)
  this (new Inner) // error,
}


class Outer1 {

  self =>

  def apply( position : Inner ) : String = "outer"

  class Inner( ) {

    def apply(arg: Inner): String = "inner"

    def testMe = {
      List(
        self.apply( this ), // a) this works
        self( this ), // b) this does not work!
        this apply this,
        this(this)
      ) foreach println
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val o = new Outer1
    val i = new o.Inner
    i.testMe
  }
}
