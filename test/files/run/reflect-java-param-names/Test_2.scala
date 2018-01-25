object Test extends App {
  import reflect.runtime.universe._

  val j_1 = symbolOf[J_1[_]]
  val constr = j_1.info.decl(termNames.CONSTRUCTOR)
  val inst   = j_1.info.decl(TermName("inst"))
  val statik = j_1.companion.info.decl(TermName("statik"))

  def check(info: Type) {
    assert(info.paramLists.head.map(_.name) == List(TermName("i"), TermName("j")), info)
  }

  check(constr.info)
  check(inst.info)
  check(statik.info)
}