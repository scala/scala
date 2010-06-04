object Test {
  val List(_*) = List(1)
	val Array( who, what @ _* ) = "Eclipse plugin cannot not handle this" split (" ")
}
