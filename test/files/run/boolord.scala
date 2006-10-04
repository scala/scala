object Test {
  def main(args: Array[String]): unit = {
    Console.println("false < false = " + (false < false))
    Console.println("false < true = " + (false < true))
    Console.println("true < false = " + (true < false))
    Console.println("true < true = " + (true < true))
  }
}
