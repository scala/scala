import Test._

abstract class S_2[T] extends J_0[T] {
  def scalaArray_c: Array[T] = scalaArray_a
  def scalaArray_a: Array[T]
  def scalaArrayGet_cc: T = scalaArray_c(0)
  def scalaArrayGet_ac: T
  def scalaArrayGet_ca: T = scalaArray_a(0)
  def scalaArrayGet_aa: T

  def scalaVarargs_a(ts: T*): Unit
  def scalaVarargs_c(ts: T*): Unit = ()

  // All the commented out lines fail with AbstractMethodErrors.
  // java.lang.AbstractMethodError: S_2.javaVarargs_c([Ljava/lang/Object;)V
  def varargsArray(x: Array[T]): Unit = {
    scalaVarargs_a(x: _*)
    scalaVarargs_c(x: _*)
    // javaVarargs_a(x: _*)
    // javaVarargs_c(x: _*)
  }
  def varargsSeq(x: Seq[T]): Unit = {
    scalaVarargs_a(x: _*)
    scalaVarargs_c(x: _*)
    // javaVarargs_a(x: _*)
    // javaVarargs_c(x: _*)
  }
  def varargsArgs(x1: T, x2: T): Unit = {
    scalaVarargs_a(x1, x2)
    scalaVarargs_c(x1, x2)
    // javaVarargs_a(x1, x2)
    // javaVarargs_c(x1, x2)
  }

  def varargsAll() {
    varargsArray(scalaArray_a)
    varargsArray(scalaArray_c)
    varargsSeq(scalaArray_a.toSeq)
    varargsSeq(scalaArray_c.toSeq)
    varargsArgs(scalaArrayGet_ac, scalaArrayGet_ac)
    varargsArgs(scalaArrayGet_aa, scalaArrayGet_aa)
  }

  override def toString =
    List(scalaArrayGet_cc, scalaArrayGet_ac, scalaArrayGet_ca, scalaArrayGet_aa) mkString ", "
}
object S_2 {
  def scalaArray_static[U]: Array[U] = null
}

class JString extends J_1[String] {
  def javaArray_a() = arr1
  def javaArrayGet_ac() = javaArray_c()(0)
  def javaArrayGet_aa() = javaArray_a()(0)
  def javaVarargs_a(ts: String*): Unit = println(ts mkString " ")

  override def toString =
    List(javaArrayGet_cc(), javaArrayGet_ca(), javaArrayGet_ac(), javaArrayGet_aa()).mkString(", ")
}
class JLongBox extends J_1[java.lang.Long] {
  def javaArray_a() = arr2box
  def javaArrayGet_ac() = javaArray_c()(0)
  def javaArrayGet_aa() = javaArray_a()(0)
  def javaVarargs_a(ts: java.lang.Long*): Unit = println(ts mkString " ")

  override def toString =
    List(javaArrayGet_cc(), javaArrayGet_ca(), javaArrayGet_ac(), javaArrayGet_aa()).mkString(", ")
}

class SString extends S_2[String] {
  def scalaArray_a = arr1
  def scalaArrayGet_ac = scalaArray_c(0)
  def scalaArrayGet_aa = scalaArray_a(0)
  def scalaVarargs_a(ts: String*): Unit = println(ts mkString " ")
  def javaVarargs_a(ts: String*): Unit = println(ts mkString " ")
  def javaVarargs_c(ts: String*): Unit = println(ts mkString " ")
}
class SLong extends S_2[Long] {
  def scalaArray_a = arr2
  def scalaArrayGet_ac = scalaArray_c(0)
  def scalaArrayGet_aa = scalaArray_a(0)
  def scalaVarargs_a(ts: Long*): Unit = println(ts mkString " ")
  def javaVarargs_a(ts: Long*): Unit = println(ts mkString " ")
  def javaVarargs_c(ts: Long*): Unit = println(ts mkString " ")
}
class SLongBox extends S_2[java.lang.Long] {
  def scalaArray_a = arr2box
  def scalaArrayGet_ac = scalaArray_c(0)
  def scalaArrayGet_aa = scalaArray_a(0)
  def scalaVarargs_a(ts: java.lang.Long*): Unit = println(ts mkString " ")
  def javaVarargs_a(ts: java.lang.Long*): Unit = println(ts mkString " ")
  def javaVarargs_c(ts: java.lang.Long*): Unit = println(ts mkString " ")
}

object Test {
  val arr1    = Array[String]("bippy")
  val arr2    = Array[Long](14653490L) // official value of bippy as a Long
  val arr2box = arr2 map Long.box

  def returnString(m: java.lang.reflect.Method) = {
    val s1 = m.getReturnType
    val s2 = m.getGenericReturnType
    if (s1 == s2) s1.getName else s"${s1.getName} ($s2)"
  }
  def clean(s: Any) = ("" + s)
                        .replaceAllLiterally("java.lang.", "jl.")
                        .replaceAllLiterally("scala.collection.", "sc.")
                        .replaceAllLiterally("public ", "")

  def mString(m: java.lang.reflect.Method) = "%7s  %13s def  %18s():  %-20s  /  %s".format(
    if (m.isBridge) "bridge" else "",
    "in " + clean(m.getDeclaringClass.getName),
    clean(m.getName),
    clean(returnString(m)),
    clean(m.toGenericString)
  )

  def classes = List(classOf[JString], classOf[JLongBox], classOf[SString], classOf[SLong], classOf[SLongBox])
  def methods(c: Class[_]) =
    (c.getMethods
        filterNot (_.getDeclaringClass == classOf[Object])
        sortBy (m => (m.getDeclaringClass.getName, m.getName, clean(m.toGenericString), m.isBridge)))

  def main(args: Array[String]): Unit = {
    println("")
    classes foreach { c =>
      println("" + c)
      methods(c) map mString foreach (m => println("  " + m))
      println("")
    }
    val js = List(new JString, new JLongBox)
    val ss = List(new SString, new SLong, new SLongBox)

    js ++ ss foreach println
    ss foreach (_.varargsAll)
  }
}
