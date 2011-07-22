// ticket #4593
trait A {

  class B
  case object D extends B

  class C {

    var x: B = D

    def y = synchronized {
      x match {
        case D => {}
      }
    }

  }

}

