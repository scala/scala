object test17 {
  def main(args : Array[String]) = {
    val value =
      if (false)
        new java.lang.Float(0)
      else if (false)
        new java.lang.Long(0)
      else
        new java.lang.Integer(0)

    println(value)
  }
}
