object Test extends App {

  @inline implicit class C(s: String) {
    def nElems = s.length
  }

  assert("abc".nElems == 3)

}


