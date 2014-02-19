import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.language.reflectiveCalls

class Y[T](val i: T) extends AnyVal {
  override def toString = s"Y($i)"
}
class Z[T](val i: T) extends AnyRef {
  override def toString = s"Z($i)"
}

object a {
  def yg_1[T](y: Y[T])           = y.i
  def yi_2(y: Y[Int])            = y.i
  def ys_3(y: Y[String])         = y.i
  def ya_4(ys: Array[Y[String]]) = ys.toList.map(_.i)
  def yl_5(ys: List[Y[String]])  = ys.map(_.i)
  def yv_6(ys: Y[String]*)       = ys.toList.map(_.i)
  def yni_7(y: => Y[Int])        = y.i
  def yns_8(y: => Y[String])     = y.i

  def zg_1[T](z: Z[T])           = z.i
  def zi_2(z: Z[Int])            = z.i
  def zs_3(z: Z[String])         = z.i
  def za_4(zs: Array[Z[String]]) = zs.toList.map(_.i)
  def zl_5(zs: List[Z[String]])  = zs.map(_.i)
  def zv_6(zs: Z[String]*)       = zs.toList.map(_.i)
  def zni_7(z: => Z[Int])        = z.i
  def zns_8(z: => Z[String])     = z.i
}

object Test extends App {
  def test(methName: String, arg: Any) = {
    val moduleA = cm.reflect(a)
    val msym = moduleA.symbol.info.decl(TermName(methName)).asMethod
    println(s"meth = $msym")
    val mmirror = moduleA.reflectMethod(msym)
    val mresult =
      try { mmirror(arg) }
      catch {
        case ex: Exception =>
          val ex1 = scala.reflect.runtime.ReflectionUtils.unwrapThrowable(ex)
          s"${ex1.getClass}: ${ex1.getMessage}"
      }
    println(s"as seen by Scala reflection: ${msym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].defString}")
    println(s"as seen by Java reflection: ${mmirror.asInstanceOf[{val jmeth: java.lang.reflect.Method}].jmeth}")
    println(s"result = $mresult")
  }

  test("yg_1", new Y(1))
  test("yg_1", new Y("1"))
  test("yi_2", new Y(2))
  test("yi_2", new Y("2"))
  test("ys_3", new Y(3))
  test("ys_3", new Y("3"))
  test("ya_4", Array(new Y(4)))
  test("ya_4", Array(new Y("4")))
  test("yl_5", List(new Y(5)))
  test("yl_5", List(new Y("5")))
  // FIXME: disabled because of SI-7056
  // test("yv_6", new Y(6))
  // test("yv_6", new Y("6"))
  test("yni_7", new Y(7))
  test("yns_8", new Y("8"))

  test("zg_1", new Z(1))
  test("zg_1", new Z("1"))
  test("zi_2", new Z(2))
  test("zi_2", new Z("2"))
  test("zs_3", new Z(3))
  test("zs_3", new Z("3"))
  test("za_4", Array(new Z(4)))
  test("za_4", Array(new Z("4")))
  test("zl_5", List(new Z(5)))
  test("zl_5", List(new Z("5")))
  // FIXME: disabled because of SI-7056
  // test("zv_6", new Z(6))
  // test("zv_6", new Z("6"))
  test("zni_7", new Z(7))
  test("zns_8", new Z("8"))
}