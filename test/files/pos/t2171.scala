final object test {
  def logIgnoredException(msg: => String) =
    try 0 catch { case ex => println(msg) }

    def main (args: Array[String]): Unit =
      while (true) logIgnoredException ("...") 
}
