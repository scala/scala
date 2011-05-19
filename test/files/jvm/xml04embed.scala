object Test {
  def main(args: Array[String]) {
    val ya = <x>{{</x>
    println(ya.text)
    val ua = <x>}}</x>
    println(ua.text)
    val za = <x>{{}}{{}}{{}}</x>
    println(za.text)
  }
}
