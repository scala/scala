trait V {
  // ok
  // error: java.lang.IllegalArgumentException: Could not find proxy for val f: Function1 in List(value f, value v, trait V, package <empty>, package <root>) (currentOwner= value <local V$class> )
  val v = { val f = (x: Int) => x + 1; f(2) }

  // ok
  // assertion failed:
  //   Trying to access the this of another class: tree.symbol = trait V, class symbol = object V$class compilation unit: fields.scala
  val developmentVersion =
    for {
      v <- scalaPropOrNone("maven.version.number")
      if v endsWith "-SNAPSHOT"
      ov <- scalaPropOrNone("version.number")
    } yield ov

  def scalaPropOrNone(name: String): Option[String] = ???
}

object O extends V