object FooSeq {
  def unapplySeq(x:Any): Option[Product2[Int,Seq[String]]] = {
    if(x.isInstanceOf[Bar]) {
      val y = x.asInstanceOf[Bar]
      Some(y.size, y.name)
    } else None
  }

  def main(args:Array[String]) = {
    val b = new Bar
    b match {
      case FooSeq(s:Int,_,n:String) => Console.println("size "+s+" name "+n)
    }
    b.size = 54
    b.name = List("large","L")
    b match {
      case FooSeq(s:Int,_,n:String) => Console.println("size "+s+" name "+n)
    }
  }
}

class Bar {
  var size: Int    = 50
  var name: Seq[String] = List("medium","M")
}

