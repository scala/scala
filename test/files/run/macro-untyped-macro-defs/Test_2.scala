import scala.reflect.runtime.universe._

object Test extends App {
  def test(methName: String) = {
    val meth = typeOf[Macros.type].member(TermName(methName)).asMethod
    println(meth)
    println(meth.typeSignature)
    meth.paramss.foreach(params => {
      println("=========")
      params.foreach(param => println(s"$param: ${showRaw(param.typeSignature)}"))
    })
    println("=========")
    println(showRaw(meth.returnType))
    println()
  }

  test("foo1")
  test("foo2")
  test("foo3")
}