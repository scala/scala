object test {
  for (val e <- List()) { //required
    val bar = new Bar123
    val res = bar.f       //required
    ()
  }
}
