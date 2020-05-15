trait t11843 {
  def ss = Symbol("ss")

  "".isInstanceOf[Int]
  "".asInstanceOf[Int]
  "".$isInstanceOf[Int]
  "".$asInstanceOf[Int]
  ss.isInstanceOf[String]
  ss.asInstanceOf[String]
  ss.$isInstanceOf[String]
  ss.$asInstanceOf[String]

  "hello, world" synchronized {
    println("23:30 on my mark...")
  }
}
