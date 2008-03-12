class Test {
  new Object { self =>
    def f(other: Any): Boolean =
      other match {
        case that: self.type => true
        case _ => false
      }
  }
}
