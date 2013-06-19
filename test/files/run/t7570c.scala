import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import definitions._
import Flag._

object Test extends App {
  val tb = cm.mkToolBox()
  val csym = tb.define(q"""class C { override def toString = "C" }""")
  println((csym, csym.isClass, csym.isModule, csym.isModuleClass))
  val dsym = tb.define(q"""object D { override def toString = "D" }""".asInstanceOf[ModuleDef])
  println((dsym, dsym.isClass, dsym.isModule, dsym.isModuleClass))
}