import scala.reflect.runtime.universe._

object Test extends App {
  def test(macroName: String) = {
    val macroType = typeOf[Macros.type].member(TypeName(macroName)).asType
    println(macroType)
    println(showRaw(macroType.typeSignature))

    val typeMacro = typeOf[Macros.type].member(TermName(macroName + "$macro")).asMethod
    println(typeMacro)
    println(typeMacro.typeSignature)
    typeMacro.paramss.foreach(params => {
      println("=========")
      params.foreach(param => println(s"$param: ${showRaw(param.typeSignature)}"))
    })
    println("=========")
    println(showRaw(typeMacro.returnType))
    println()
  }

  test("Foo1")
  test("Foo2")
}