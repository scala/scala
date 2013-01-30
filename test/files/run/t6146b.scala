import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
  | :power
  | val u = rootMirror.universe
  | import u._, language._
  | class C { object O { class E }}; object D extends C
  | deepMemberType(typeOf[D.type], typeOf[c.O.E forSome { val c: C }].typeSymbol)
  | class C { class C1 { class E }}; object D extends C
  | deepMemberType(typeOf[D.type], typeOf[c1.E forSome { val c1: C#C1 }].typeSymbol)
  """.stripMargin.trim
}