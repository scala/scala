import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
  |:power
  |val u = rootMirror.universe
  |import u._, scala.reflect.internal.Flags
  |class C
  |val sym = u.typeOf[C].typeSymbol
  |sym.isContravariant
  |sym setFlag Flags.INCONSTRUCTOR
  |sym.isClassLocalToConstructor
  |sym.isContravariant // was true
  |""".stripMargin.trim
}