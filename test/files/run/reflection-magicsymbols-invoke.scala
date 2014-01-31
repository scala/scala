import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.reflect.runtime.{currentMirror => cm}

package scala {
  object ExceptionUtils {
    def unwrapThrowable(ex: Throwable): Throwable = scala.reflect.runtime.ReflectionUtils.unwrapThrowable(ex)
  }
}

object Test extends App {
  def key(sym: Symbol) = sym + ": " + sym.info
  def test(tpe: Type, receiver: Any, method: String, args: Any*) {
    def wrap[T](op: => T) =
      try {
        var result = op.asInstanceOf[AnyRef]
        if (scala.runtime.ScalaRunTime.isArray(result))
          result = scala.runtime.ScalaRunTime.toObjectArray(result).toList
        println(result)
      } catch {
        case ex: Throwable =>
          val realex = scala.ExceptionUtils.unwrapThrowable(ex)
          println(realex.getClass + ": " + realex.getMessage)
      }
    print(s"testing ${tpe.typeSymbol.name}.$method: ")
    wrap({
      if (method == termNames.CONSTRUCTOR.toString) {
        val ctor = tpe.decl(termNames.CONSTRUCTOR).asMethod
        cm.reflectClass(ctor.owner.asClass).reflectConstructor(ctor)(args: _*)
      } else {
        val meth = tpe.decl(TermName(method).encodedName.toTermName).asMethod
        cm.reflect(receiver).reflectMethod(meth)(args: _*)
      }
    })
  }

  println("============\nAny")
  println("it's important to print the list of Any's members")
  println("if some of them change (possibly, adding and/or removing magic symbols), we must update this test")
  typeOf[Any].members.toList.sortBy(key).foreach(sym => println(key(sym)))
  test(typeOf[Any], "2", "!=", "2")
  test(typeOf[Any], "2", "##")
  test(typeOf[Any], "2", "==", "2")
  test(typeOf[Any], "2", "asInstanceOf")
  test(typeOf[Any], "2", "asInstanceOf", typeOf[String])
  test(typeOf[Any], "2", "equals", "2")
  test(typeOf[Any], "2", "getClass")
  test(typeOf[Any], "2", "hashCode")
  test(typeOf[Any], "2", "isInstanceOf")
  test(typeOf[Any], "2", "isInstanceOf", typeOf[String])
  test(typeOf[Any], "2", "toString")

  println("============\nAnyVal")
  println("it's important to print the list of AnyVal's members")
  println("if some of them change (possibly, adding and/or removing magic symbols), we must update this test")
  typeOf[AnyVal].decls.toList.sortBy(key).foreach(sym => println(key(sym)))
  test(typeOf[AnyVal], null, termNames.CONSTRUCTOR.toString)
  test(typeOf[AnyVal], 2, "getClass")

  println("============\nAnyRef")
  println("it's important to print the list of AnyRef's members")
  println("if some of them change (possibly, adding and/or removing magic symbols), we must update this test")
  typeOf[AnyRef].members.toList.sortBy(key).foreach(sym => println(key(sym)))
  test(typeOf[AnyRef], "2", "!=", "2")
  test(typeOf[AnyRef], "2", "##")
  test(typeOf[AnyRef], "2", "$asInstanceOf")
  test(typeOf[AnyRef], "2", "$asInstanceOf", typeOf[String])
  test(typeOf[AnyRef], "2", "$isInstanceOf")
  test(typeOf[AnyRef], "2", "$isInstanceOf", typeOf[String])
  test(typeOf[AnyRef], "2", "==", "2")
  test(typeOf[AnyRef], "2", "clone")
  test(typeOf[AnyRef], "2", "eq", "2")
  test(typeOf[AnyRef], "2", "equals", "2")
  test(typeOf[AnyRef], "2", "finalize")
  test(typeOf[AnyRef], "2", "getClass")
  test(typeOf[AnyRef], "2", "hashCode")
  test(typeOf[AnyRef], "2", "ne", "2")
  test(typeOf[AnyRef], "2", "notify")
  test(typeOf[AnyRef], "2", "notifyAll")
  test(typeOf[AnyRef], "2", "synchronized", "2")
  test(typeOf[AnyRef], "2", "toString")
  println("TODO: also test AnyRef.wait overloads")

  println("============\nArray")
  println("it's important to print the list of Array's members")
  println("if some of them change (possibly, adding and/or removing magic symbols), we must update this test")
  ArrayClass.info.members.toList.sortBy(key).foreach(sym => println(key(sym)))
  test(ArrayClass.info, Array(1, 2), "length")
  test(ArrayClass.info, Array(1, 2), "apply", 0)
  test(ArrayClass.info, Array(1, 2), "update", 0, 0)
  test(ArrayClass.info, Array(1, 2), "clone")

  println("============\nOther")
  test(typeOf[String], "2", "+", 3)

  println("============\nCTM")
  test(PredefModule.moduleClass.info, Predef, "classOf")
  test(PredefModule.moduleClass.info, Predef, "classOf", typeOf[String])
  test(typeOf[scala.reflect.api.Universe], scala.reflect.runtime.universe, "reify", "2")
}