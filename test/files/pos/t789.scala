object main { // don't do this at home

  trait Impl

  trait SizeImpl extends Impl { def size = 42 }

  trait ColorImpl extends Impl { def color = "red" }

  type Both = SizeImpl with ColorImpl

  def info(x:Impl) = x match {
    case x:Both      => "size  "+x.size+" color "+x.color // you wish
    case x:SizeImpl  => "size  "+x.size
    case x:ColorImpl => "color "+x.color
    case _           => "n.a."
  }

  def info2(x:Impl) = x match {
    case x:SizeImpl with ColorImpl  => "size  "+x.size+" color "+x.color // you wish
    case x:SizeImpl  => "size  "+x.size
    case x:ColorImpl => "color "+x.color
    case _           => "n.a."
  }


  def main(args:Array[String]): Unit = {
    // make up some class that has a size
    class MyNode extends SizeImpl
    Console.println("hello " + info(new MyNode))
    Console.println("hello " + info2(new MyNode))
  }
}
