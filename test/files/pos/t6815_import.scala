trait U {
  trait ValOrDefDefApi {
    def name: Any
  }
  type ValOrDefDef <: ValOrDefDefApi
  type ValDef <: ValOrDefDef with ValDefApi
  trait ValDefApi extends ValOrDefDefApi { this: ValDef => }
  val emptyValDef: ValDef // the result type is volatile
}

object Test {
  val u: U = ???

  // but we shouldn't let that stop us from treating it as a stable identifier for import
  import u.emptyValDef.name
}
