object Test {

  trait Foo

  class Bar { 
    object baz extends Foo
  }

  def frob[P1, P2<:Foo](f:P1 => P2) = () 

  def main(args:Array[String]) : Unit = {
  	frob((p:Bar) => p.baz) 
  }

}
