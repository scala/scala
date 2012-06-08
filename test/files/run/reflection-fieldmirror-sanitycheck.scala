import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A {
    var x: Int = 42
  }

  val a = new A

  val im: InstanceMirror = cm.reflect(a)
  val cs = im.symbol
  //val f = cs.typeSignature.declaration(newTermName("x" + nme.LOCAL_SUFFIX_STRING)).asTermSymbol
  val f = cs.typeSignature.declaration(newTermName("x")).asTermSymbol
  try {
    val fm: FieldMirror = im.reflectField(f)
    println("this indicates a failure")
  } catch {
    case ex: Throwable =>
      println(ex.getMessage)
  }
}
