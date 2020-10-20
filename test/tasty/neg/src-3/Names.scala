package tastytest

// lets test bounds on an opaque type, here Name is just a refinement to String
object Names {

  opaque type Name >: Null <: String = String

  object Name {
    def read(str: String): Option[Name] = Option.when(str(0).isUpper)(str)
  }

  // to test rejection of plain string
  def initial(name: Name): Char = name(0)

}
