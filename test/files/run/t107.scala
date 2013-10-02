object Test {
  def main(args : Array[String]) : Unit = {
    var hash : Long = 0
    val bytes = Array(1.toByte, 2.toByte, 3.toByte)
    hash += bytes(0)
    Console.println(hash)
  }
}