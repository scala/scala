



object Test {
  def main(args: Array[String]) {
    val r = 'a' to 'z'
    for (i <- -2 to (r.length + 2)) {
      assert(r.take(i) == r.toList.take(i), (i, r.take(i)))
      assert(r.drop(i) == r.toList.drop(i), (i, r.drop(i)))
    }
  }
}
