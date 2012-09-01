object Test {
  implicit case class IntOps( i: Int ) {
    def twice = i * 2
  }
}

