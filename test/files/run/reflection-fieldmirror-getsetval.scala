import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A {
    val x: Int = 42
  }

  val a = new A

  val im: InstanceMirror = cm.reflect(a)
  val cs = im.symbol
  val f = cs.typeSignature.declaration(newTermName("x" + nme.LOCAL_SUFFIX_STRING)).asTermSymbol
  val fm: FieldMirror = im.reflectField(f)
  try {
    println(fm.get)
    fm.set(2)
    println(fm.get)
    println("this indicates a failure")
  } catch {
    case ex: Throwable =>
      println(ex.getMessage)
  }
}
