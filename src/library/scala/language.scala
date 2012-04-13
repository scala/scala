package scala

object language {

  import languageFeature._

  implicit val dynamics: dynamics = ???

  implicit val postfixOps: postfixOps = ???

  implicit val reflectiveCalls: reflectiveCalls = ???

  implicit val implicitConversions: implicitConversions = ???

  implicit val higherKinds: higherKinds = ???

  implicit val existentials: existentials = ???

  object experimental {

    import languageFeature.experimental._

    implicit val macros: macros = ???
  }
}
