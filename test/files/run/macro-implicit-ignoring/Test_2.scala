object Test extends App {

  println(implicitly[TypeClass[String]].value)
  println(TypeClass.ignoreDefault[String])
  locally {
    implicit val overridenDefaultString: TypeClass[String] = new TypeClass[String] {
      def value: String = "not-ignoring-user-provided-implicit"
    }
    println(implicitly[TypeClass[String]].value)
    println(TypeClass.ignoreDefault[String])
  }
}
