object Test extends App {

  implicit class C(s: String) {
    def nElems = s.length
  }

  "abc".nElems

}

