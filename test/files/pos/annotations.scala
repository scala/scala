class ann(i: Int) extends scala.annotation.Annotation
class cfann(x: String) extends annotation.ClassfileAnnotation

// annotations on abstract types
abstract class C1[@annotation.elidable(0) +T, U, V[_]]
abstract class C2[@deprecated
                  @ann(1) T <: Number,
                  V]
abstract class C3 {
  @ann(2) type X <: Number
}

object Test {

  // bug #1028
  val x = 1
  @ann(x) val a = ()
  @ann({val yy = 2; yy}) val b = ()
  val bb: Int @ann({val yy = 2; yy}) = 10

  def c: Int @ann(x) = 1
  def d: String @ann({val z = 0; z - 1}) = "2"
  def e[@deprecated T, U](x: T) = x

  //bug #1214
  val y = new (Integer @ann(0))(2)

  import scala.beans.BeanProperty

  // bug #637
  trait S { def getField(): Int }
  class O extends S { @BeanProperty val field = 0 }

  // bug #1070
  trait T { @BeanProperty var field = 1 }

  // annotation on annotation constructor
  @(ann @ann(100))(200) def foo() = 300

  // #2984
  private final val NAMESPACE = "/info"
  @cfann(x = NAMESPACE + "/index") def index = "success"
}

// test forward references to getters / setters
class BeanPropertyTests {
  @scala.beans.BeanProperty lazy val lv1 = 0

  def foo() {
    val bp1 = new BeanPropertyTests1

    println(lv1)
    println(getLv1())
    println(bp1.getLv2())

    println(getV1())
    setV1(10)
    bp1.setV2(100)
  }

  @scala.beans.BeanProperty var v1 = 0

}

class BeanPropertyTests1 {
  @scala.beans.BeanProperty lazy val lv2 = "0"
  @scala.beans.BeanProperty var v2 = 0
}

// test mixin of getters / setters, and implementing abstract
// methods using @BeanProperty
class C extends T with BeanF {
  def foo() {
    setF("doch!")
    setG(true)
    this.getF()
  }
}

trait T {
  @scala.beans.BeanProperty var f = "nei"
  @scala.beans.BooleanBeanProperty var g = false
}

trait BeanF {
  def getF(): String
  def setF(n: String): Unit

  def isG(): Boolean
  def setG(nb: Boolean): Unit
}


class Ann3(arr: Array[String]) extends annotation.ClassfileAnnotation
class Ann4(i: Int) extends annotation.ClassfileAnnotation
class Ann5(value: Class[_]) extends annotation.ClassfileAnnotation

object Test3 {
  final val i = 1083
  final val cls = classOf[String]
}

class Test4 {
  @Ann3(arr = Array("dlkfj", "DSF"))
  @Ann4(i = 2908)
  @Ann4(i = Test3.i)
  @Ann5(value = classOf[Int])
  @Ann5(Test3.cls)
  def foo {}
}
