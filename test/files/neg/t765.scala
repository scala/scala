object test {
  for (e <- List()) { //required
    val bar = new Bar123
    val res = bar.f       //required
    ()
  }
}
