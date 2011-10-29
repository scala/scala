trait Foo {
  type Arg
  type Prod
  def makeProd(a: Arg): Prod
}

object Test {
  def f1(x: Foo)(y: x.Arg) = x.makeProd(y)

  case class f2[T <: Foo](x: T) {
    def apply(y: x.Arg) = x.makeProd(y)
  }

  val myFoo = new Foo {
    type Arg = Int
    type Prod = (Int, Int)
    def makeProd(i: Int) = (i, i)
  }

  def main(args: Array[String]): Unit = {
    println(f1(myFoo)(5))
    println(f2(myFoo)(10))
  }
}
