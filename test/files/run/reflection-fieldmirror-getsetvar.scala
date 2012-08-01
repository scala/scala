import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  class A {
    var x: Int = 42
  }

  val a = new A

  val im: InstanceMirror = cm.reflect(a)
  val cs = im.symbol
  val f = cs.typeSignature.declaration(newTermName("x" + nme.LOCAL_SUFFIX_STRING)).asTerm
  val fm: FieldMirror = im.reflectField(f)
  println(fm.get)
  fm.set(2)
  println(fm.get)
}
