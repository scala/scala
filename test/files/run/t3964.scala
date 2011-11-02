object Test {
  class Base 
  object Bob extends Base
  class Foo { def bippy = 42 }
  class Oof { def bippy = -21 }
  
  // I am more specific than you
  implicit def f1(x: Bob.type): Foo = new Foo
  implicit def f2(x: Base): Oof = new Oof
  
  def main(args: Array[String]): Unit = {
    // this would of course print an unambiguous 42
    println(Bob.bippy)
    println((new Base).bippy)
  }
}
