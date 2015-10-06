package backendAttrs.genBCode

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._

  val name = "backendCustomAttrs"
  val description = "A sample plugin to test BackendPlugin pluginsCustomAttributes with GenBCode"
  val components = Nil
  addBackendPlugin(BPlugin1)
  addBackendPlugin(BPlugin2)
  addBackendPlugin(BPlugin3)

  def isTestSymbol(sym: ClassSymbol) = sym.name.toString.contains("TestAttr")

  class BPluginTemplate(customAttrs: List[ClassAttr]) extends BackendPlugin {
    override def isActive(): Boolean = true
    override def pluginsCustomAttributes(sym: ClassSymbol): List[ClassAttr] =
      if (isTestSymbol(sym)) customAttrs else Nil
  }

  object BPlugin1 extends BPluginTemplate(List(ClassAttr("CUSTOMATTR1", Array(0,1,1,0)), ClassAttr("CUSTOMATTR2", Array(1,0,0,1))))
  object BPlugin2 extends BPluginTemplate(List(ClassAttr("CUSTOMATTR3", Array(0,1,1,0)))) {
    override def isActive(): Boolean = false
  }
  object BPlugin3 extends BPluginTemplate(List(ClassAttr("CUSTOMATTR4", Array(0,1,1,0))))
}