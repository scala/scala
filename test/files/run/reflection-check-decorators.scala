import scala.reflect.runtime.universe._

object Test extends App {
  def test(name: String) = {
    println("testing " + name + "...")
    val api_name = name + "Api"
    val api = typeOf[scala.reflect.api.Symbols].member(newTypeName(api_name))
    val api_tests = api.typeSignature.declarations.toList.filter(_.name.toString startsWith "is")
    val api_test_names = api_tests map (_.name.toString)
    val reflected_name = "Reflected" + name
    val reflected = typeOf[scala.reflect.runtime.ReflectedSymbols].member(newTypeName(reflected_name))
    val reflected_tests = reflected.typeSignature.declarations.toList.filter(_.name.toString startsWith "is")
    reflected_tests foreach (meth => assert(meth.isOverride, meth))
    val reflected_test_names = reflected_tests map (_.name.toString)
    api_test_names diff reflected_test_names sortBy Predef.identity foreach (name => println("  " + name))
  }

  def detect = {
    val types = typeOf[scala.reflect.api.Symbols].declarations.toList.filter(sym => sym.isType && !sym.isClass)
    types map (_.name.toString) sortBy Predef.identity
  }

  detect foreach (test(_))
}