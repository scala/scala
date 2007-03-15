

object Test {

  def method1() = {
    val x = "Hello, world";
    val y = 100;

     y match {
       case _: Int
        if (x match { case t => t.trim().length() > 0 }) =>
          false;
       case _ => true;
    }
  }

  def method2(): scala.Boolean = {
    val x: String = "Hello, world";
    val y: scala.Int = 100;
    {
      var temp1: scala.Int = y;
      var result: scala.Boolean = false;
      if (
        {
          var result1: scala.Boolean = true;
          if (y == 100)
            result1
          else
            throw new MatchError("crazybox.scala, line 11")
        } && (y > 90)
      )
        result
      else
        throw new MatchError("crazybox.scala, line 9")
    }
  }

  def main(args: Array[String]): Unit = {
    method1();
    method2();
  }

}

