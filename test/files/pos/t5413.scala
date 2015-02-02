object Fail {
  def nom (guard : => Boolean) (something : => Unit) { }
  def main(args: Array[String]) {
    nom {
      val i = 0
      (i != 3)
    }()
  }
}
