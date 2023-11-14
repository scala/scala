object C {
  import A._, B._
  implicitly[Ordering[Int]]

  def main(args: Array[String]): Unit = ()
}
