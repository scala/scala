trait U {
  trait ValOrDefDefApi {
    def name: Any
  }
  type ValOrDefDef <: ValOrDefDefApi
  type ValDef <: ValOrDefDef with ValDefApi { type T }
  trait ValDefApi extends ValOrDefDefApi { this: ValDef => }
  val emptyValDef: ValDef // the result type is volatile
}

object Test {
  val u: U = ???

  (null: Any) match {
    case _: u.emptyValDef.T => // and, unlike in pos/t6185.scala, we shouldn't allow this.
  }
}
