import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.ClassTag

package scala {
  object ExceptionUtils {
    def unwrapThrowable(ex: Throwable): Throwable = scala.reflect.runtime.ReflectionUtils.unwrapThrowable(ex)
  }
}

object Test extends App {
  def key(sym: Symbol) = {
    sym match {
      // initialize parameter symbols
      case meth: MethodSymbol => meth.paramLists.flatten.map(_.info)
    }
    sym + ": " + sym.info
  }

  def convert(value: Any, tpe: Type) = {
    import scala.runtime.BoxesRunTime._
    if (tpe =:= typeOf[Byte]) toByte(value)
    else if (tpe =:= typeOf[Short]) toShort(value)
    else if (tpe =:= typeOf[Char]) toCharacter(value)
    else if (tpe =:= typeOf[Int]) toInteger(value)
    else if (tpe =:= typeOf[Long]) toLong(value)
    else if (tpe =:= typeOf[Float]) toFloat(value)
    else if (tpe =:= typeOf[Double]) toDouble(value)
    else if (tpe =:= typeOf[String]) value.toString
    else if (tpe =:= typeOf[Boolean]) value.asInstanceOf[Boolean]
    else throw new Exception(s"not supported: value = $value, tpe = $tpe")
  }

  def test[T: ClassTag](tpe: Type, receiver: T, method: String, args: Any*) {
    def wrap[T](op: => T) =
      try {
        var result = op.asInstanceOf[AnyRef]
        if (scala.runtime.ScalaRunTime.isArray(result))
          result = scala.runtime.ScalaRunTime.toObjectArray(result).toList
        println(s"[${result.getClass}] =======> $result")
      } catch {
        case ex: Throwable =>
          val realex = scala.ExceptionUtils.unwrapThrowable(ex)
          println(realex.getClass + ": " + realex.getMessage)
      }
    val meth = tpe.decl(TermName(method).encodedName.toTermName)
    val testees = if (meth.isMethod) List(meth.asMethod) else meth.asTerm.alternatives.map(_.asMethod)
    testees foreach (testee => {
      val convertedArgs = args.zipWithIndex.map { case (arg, i) => convert(arg, testee.paramLists.flatten.apply(i).info) }
      print(s"testing ${tpe.typeSymbol.name}.$method(${testee.paramLists.flatten.map(_.info).mkString(','.toString)}) with receiver = $receiver and args = ${convertedArgs.map(arg => arg + ' '.toString + arg.getClass).toList}: ")
      wrap(cm.reflect(receiver).reflectMethod(testee)(convertedArgs: _*))
    })
  }
  def header(tpe: Type) {
    println(s"============\n$tpe")
    println("it's important to print the list of Byte's members")
    println("if some of them change (possibly, adding and/or removing magic symbols), we must update this test")
    tpe.members.toList.sortBy(key).foreach(sym => println(key(sym)))
  }

  def testNumeric[T: ClassTag](tpe: Type, value: T) {
    header(tpe)
    List("toByte", "toShort", "toChar", "toInt", "toLong", "toFloat", "toDouble") foreach (meth => test(tpe, value, meth))
    test(tpe, value, "==", 2)
    test(tpe, value, "!=", 2)
    test(tpe, value, "<", 2)
    test(tpe, value, "<=", 2)
    test(tpe, value, ">", 2)
    test(tpe, value, ">=", 2)
    test(tpe, value, "+", 2)
    test(tpe, value, "-", 2)
    test(tpe, value, "*", 2)
    test(tpe, value, "/", 2)
    test(tpe, value, "%", 2)
  }

  def testIntegral[T: ClassTag](tpe: Type, value: T) {
    testNumeric(tpe, value)
    test(tpe, value, "unary_~")
    test(tpe, value, "unary_+")
    test(tpe, value, "unary_-")
    test(tpe, value, "<<", 2)
    test(tpe, value, ">>", 2)
    test(tpe, value, ">>>", 2)
    test(tpe, value, "|", 2)
    test(tpe, value, "&", 2)
    test(tpe, value, "^", 2)
  }

  def testBoolean() {
    header(typeOf[Boolean])
    test(typeOf[Boolean], true, "unary_!")
    test(typeOf[Boolean], true, "==", true)
    test(typeOf[Boolean], true, "!=", true)
    test(typeOf[Boolean], true, "||", true)
    test(typeOf[Boolean], true, "&&", true)
    test(typeOf[Boolean], true, "|", true)
    test(typeOf[Boolean], true, "&", true)
    test(typeOf[Boolean], true, "^", true)
  }

  def testUnit() {
    header(typeOf[Unit])
  }

  testNumeric(typeOf[Byte], 2.toByte)
  testNumeric(typeOf[Short], 2.toShort)
  testNumeric(typeOf[Char], 2.toChar)
  testNumeric(typeOf[Int], 2.toInt)
  testNumeric(typeOf[Long], 2.toLong)
  testNumeric(typeOf[Float], 2.toFloat)
  testNumeric(typeOf[Double], 2.toDouble)
  testBoolean()
  testUnit()
}