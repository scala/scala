class C {
  implicit def conversion(m: Int)(implicit nada: Nothing): String = ???

  class C { // rename class to get correct error, can't find implicit: Nothing.
    locally(0 : String) // was: "value conversion is not a member of C.this.C"
  }
}

object Test2 {
  trait T; trait U
  new T {
    implicit def conversion(m: Int)(implicit nada: Nothing): String = ???

    new U { // nested anonymous classes also share a name.
      locally(0 : String) // was: "value conversion is not a member of U"
    }
  }
}
