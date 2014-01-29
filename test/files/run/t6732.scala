import scala.reflect.runtime.universe._
import definitions._

object Test extends App {
  def test(sym: Symbol): Unit = {
    println(s"${showRaw(sym, printKinds = true)}: ${sym.isModule}, ${sym.isModuleClass}, ${sym.isPackage}, ${sym.isPackageClass}")
  }
  test(ScalaPackage)
  test(ScalaPackageClass)
  test(ListModule)
  test(ListModule.moduleClass)
}