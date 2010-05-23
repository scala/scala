object Test {
  def main(args: Array[String]): Unit = {
    var x = true

    ( { if (x) new scala.util.Random() } .asInstanceOf[Runnable] )
  }
}


