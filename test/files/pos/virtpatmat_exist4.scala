trait Global {
  trait Tree
  trait Symbol { def foo: Boolean }
}

trait IMain { self:  MemberHandlers =>
  val global: Global
  def handlers: List[MemberHandler]
}

trait MemberHandlers {
  val intp: IMain
  import intp.global._
  sealed abstract class MemberHandler(val member: Tree) {
    def importedSymbols: List[Symbol]
  }
}

object Test {
  var intp: IMain with MemberHandlers = null

  val handlers = intp.handlers
  handlers.filterNot(_.importedSymbols.isEmpty).zipWithIndex foreach {
    case (handler, idx) =>
      val (types, terms) = handler.importedSymbols partition (_.foo)
  }
}

object Test2 {
  type JClass = java.lang.Class[_]

  def tvarString(bounds: List[AnyRef]) = {
    bounds collect { case x: JClass => x }
  }
}