import collection.mutable.UnrolledBuffer



object Test {

  def main(args: Array[String]) {
    val buf = UnrolledBuffer(1 to 50: _*)
    val dub = buf ++ buf

    println(dub)
  }

}
