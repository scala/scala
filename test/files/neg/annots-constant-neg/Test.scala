import scala.annotation._
import java.lang.annotation._

class Ann(
  value: Int,
  a: String = "",
  b: Class[_] = classOf[String],
  c: Array[Object] = Array()) extends ConstantAnnotation

// non-constant defaults are allowed
class Ann1(value: Int = Test.nonConst) extends ConstantAnnotation {
  def this(s: String) = this(0) // err
}
class Ann2(x: Int)(y: Int) extends ConstantAnnotation // err
class Ann3 extends ConstantAnnotation
class Ann4(x: Int = 0, value: Int) extends ConstantAnnotation
class Ann5() extends ConstantAnnotation
class Ann6(x: Int) extends ConstantAnnotation // scala/bug#11724
class Ann7[T](x: T) extends annotation.ConstantAnnotation // scala/bug#11724

object Test {
  final val const = 1
  def nonConst = 2

  @JAnn(0) def t1 = 0
  @JAnn(const + 1 + const) def t2 = 0
  @JAnn(nonConst) def t3 = 0 // err

  @JAnn() def t4 = 0 // err
  @JAnn(value = 0) def t5 = 0
  @JAnn(value = 0, a = "slkdjf" + "mix") def t6 = 0
  @JAnn(0, "") def t7 = 0 // err
  @JAnn(0, a = "") def t8 = 0 // err

  @JAnn(value = 0, a = "moin", b = classOf[Object], c = Array(""), d = new SuppressWarnings(value = Array("", "")), e = RetentionPolicy.CLASS) def t9 = 0
  @JAnn(value = 0, a = null) def t10 = 0 // err
  @JAnn(value = 0, b = getClass) def t11 = 0 // err
  @JAnn(value = 0, c = new Array(1)) def t12 = 0 // err
  @JAnn(value = 0, d = null) def t13 = 0 // err
  @JAnn(value = 0, d = null) def t14 = 0 // err

  @JAnn(value = 0, b = classOf[Int]) def t15 = 0
  @JAnn(value = 0, b = java.lang.Integer.TYPE) def t16 = 0 // err

  // nested annotation is ok
  @JAnn(value = 0, c = Array(new SuppressWarnings(value = Array("")))) def t17 = 0
  // but the nested annotation needs to be itself a Java annotation
  @JAnn(value = 0, c = Array(new inline)) def t18 = 0 // err

  @Ann(1) def u1 = 0
  @Ann(const) def u2 = 0
  @Ann(nonConst) def u3 = 0 // err

  @Ann() def u4 = 0 // err
  @Ann(value = 0) def u5 = 0
  @Ann(value = 0, a = "") def u6 = 0
  @Ann(0, "") def u7 = 0
  @Ann(0, a = "") def u8 = 0

  @Ann(value = 0, a = "moin", b = classOf[Object], c = Array("")) def u9 = 0
  @Ann(value = 0, a = null) def u10 = 0 // err
  @Ann(value = 0, b = getClass) def u11 = 0 // err
  @Ann(value = 0, c = new Array(1)) def u12 = 0 // err

  @Ann(value = 0, b = classOf[Int]) def u15 = 0
  @Ann(value = 0, b = java.lang.Integer.TYPE) def u16 = 0 // err

  // nested annotations are only allowed for Java annotations, not for Scala ConstantAnnotations
  @Ann(value = 0, c = Array(new SuppressWarnings(value = Array("")))) def u17 = 0 // err
  // the outer and the nested annotation need to be Java annotations
  @Ann(value = 0, c = Array(new inline)) def u18 = 0 // err

  @Ann1() def v1 = 0
  @Ann1(0) def v2 = 0
  @Ann1(value = 0) def v3 = 0
  @Ann1(x = "") def v4 = 0 // err
  @Ann1 def v5 = 0
  @Ann1(0)(0) def v6 = 0 // err
  @Ann2 def v7 = 0 // err
  @Ann2(x = 0) def v8 = 0 // err
  @Ann2(x = 0)(y = 0) def v9 = 0 // warn
  @Ann3 def v10 = 0
  @Ann3(0) def v11 = 0 // err
  @Ann4(0) def v12 = 0
  @Ann4(0, 1) def v13 = 0
  @Ann4(x = 0, value = 1) def v14 = 0
  @Ann4(value = 1, x = 0) def v15 = 0
  @Ann5 def v16 = 0
  @Ann5() def v17 = 0
  @Ann5(0) def v18 = 0 // err

  @Ann6(1) def w1 = 0

  @Ann7(1) def x1 = 0
  @Ann7(Array("")) def x2 = 0
}
