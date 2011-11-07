object Test {
  def main(args: Array[String]): Unit = {
    Seq("") match { 
      case Seq("")      => println("abc")
      case Seq(_, _, x) => println(x)
    }
    
    Seq(1, 2, "def") match { 
      case Seq("")      => println("abc")
      case Seq(_, _, x) => println(x)
    }
  }
}