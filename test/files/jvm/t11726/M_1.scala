import language.experimental.{macros => m}
import reflect._

object M_1 {

  def test(phase: Int, u: api.Universe)(m: u.Mirror): String = {
    val cs = m.staticClass("jli.CS_" + phase)
    val sd = m.staticClass("jli.SD_" + phase)
    s"""$cs.info: ${cs.info}
       |$sd.companion.moduleClass.info: ${sd.companion.asModule.moduleClass.info}
     """.stripMargin.trim
  }

  def testMacro_impl(c: macros.blackbox.Context)(p: c.Tree): c.Tree = {
    import c.universe._
    val Literal(Constant(ph: Int)) = p
    Literal(Constant(test(ph, c.universe)(rootMirror)))
  }
  def testMacro(p: Int): String = macro testMacro_impl

}