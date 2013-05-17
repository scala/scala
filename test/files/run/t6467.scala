



import collection._



object Test extends App {

  def compare(s1: String, s2: String) {
    assert(s1 == s2, s1 + "\nvs.\n" + s2)
  }

  compare(List(1, 2, 3, 4).aggregate(new java.lang.StringBuffer)(_ append _, _ append _).toString, "1234")
  compare(List(1, 2, 3, 4).par.aggregate(new java.lang.StringBuffer)(_ append _, _ append _).toString, "1234")
  compare(Seq(0 until 100: _*).aggregate(new java.lang.StringBuffer)(_ append _, _ append _).toString, (0 until 100).mkString)
  compare(Seq(0 until 100: _*).par.aggregate(new java.lang.StringBuffer)(_ append _, _ append _).toString, (0 until 100).mkString)

}