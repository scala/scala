object Test
{
  class Foo[T](x : Array[AnyRef]) { def bar = x.asInstanceOf[Array[T]] }
  class Bar[T](x : Array[T]) { def bar = x.asInstanceOf[Array[AnyRef]] }

  object FromMono{
     def main(args : Array[String]) = (new Foo[AnyRef](Array[AnyRef]("Halp!"))).bar
  }

  object FromPoly{
    def main(args : Array[String]) = (new Bar[AnyRef](Array[AnyRef]("Halp!"))).bar
  }
  
  def main(args: Array[String]): Unit = {
    println(FromMono main null mkString)
    println(FromPoly main null mkString)
  }
}

