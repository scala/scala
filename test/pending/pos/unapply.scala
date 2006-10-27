case class MyTuple2[A,B](val _1:A, val snd:B)

object Foo {
  def unapply(x:Any): Option[Product2[Int,String]] = {
    if(x.isInstanceOf[Bar]) {
      val y = x.asInstanceOf[Bar]
      Some(MyTuple2(y.size, y.name))
    } else None
  }

  def main(args:Array[String]) = {
    val b = new Bar
    b match {
      case Foo(s:Int,n:String) => Console.println("size "+s+" name "+n)
    }
    b.size = 54
    b.name = "large"
    b match {
      case Foo(s:Int,n:String) => Console.println("size "+s+" name "+n)
    }
  }
}

class Bar {
  var size: Int    = 50
  var name: String = "medium"
}

