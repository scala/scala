package p
class O(val o: String) {
  class I[T](val i: String) {
    println(s"new O($o).I[]($i)")
  }
}
