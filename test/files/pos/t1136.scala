object test {
  def foo(s: Int*) {
    s.toList match {
      case t: List[Int] => foo(t: _*)
      //case _ =>  // unreachable code
    }
  }
}
