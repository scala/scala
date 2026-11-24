// A SAM lambda that LambdaMetaFactory cannot implement is expanded to an anonymous class that forwards
// to the lifted body in the enclosing class. The class must not capture the enclosing instance if the
// body does not use it: the instance would be retained, and serializing the lambda would fail.

import java.io._

trait Get[A] { def get(): A }
// since scala/scala#11205: `get()I` cannot be bridged to `get()Object` by LMF
trait GetInt extends Get[Int] with Serializable { def get(): Int }
// never eligible for LMF
trait WithVal extends Serializable { val v = 1; def get(): Int }
trait Fun extends (Int => Int) with Serializable

class Holder { // not Serializable
  val field = 10
  def member = 20

  def bridged: GetInt = () => 42
  def withVal: WithVal = () => 42
  def fun: Fun = x => x + 1
  def local(i: Int): GetInt = () => i + 1
  def nested: GetInt = () => { val g: GetInt = () => 1; g.get() + 1 }
  def localDef: GetInt = { def loc(i: Int) = i * 2; () => loc(21) }

  def usesField: GetInt = () => field
  def usesMember: WithVal = () => member
  def nestedUsesThis: GetInt = () => { val g: GetInt = () => field; g.get() + 1 }
  def localDefUsesThis: GetInt = { def loc(i: Int) = i * field; () => loc(2) }

  // the class of a PartialFunction literal is in the same position if it calls a local method
  def pf: PartialFunction[Any, Int] = { def loc(i: Int) = i + 1; { case i: Int if loc(i) > 0 => loc(i) } }
  def pfUsesThis: PartialFunction[Any, Int] = { def loc(i: Int) = i + field; { case i: Int if loc(i) > 0 => loc(i) } }

  // The lambda reaches its own class through the local method. The outer instance is not needed, but the analysis
  // does not see that: the lifted body refers to `this` through the constructor call in `mk`.
  def selfReaching: WithVal = { def mk(n: Int): WithVal = () => if (n > 0) mk(n - 1).get() + 1 else 0; mk(5) }

  class Inner { def bridged: GetInt = () => 43 }
}

trait HolderTrait { def bridged: GetInt = () => 44 }

class Other { val y = 6; abstract class Sam { def get(): Int; def outerY = y } }

// SAM types that are classes with an outer instance. The constructor of the anonymous class passes that instance to
// the super constructor. Where it is derived from the outer parameter, call sites must not pass null for it.
class Outers(val o: Other, val j: J_1) extends J_1 {
  abstract class Member { def get(): Int; def outerX = x }

  def member: Int = { val s: Member = () => 1; s.get() + s.outerX }
  def localClass: Int = { abstract class L { def get(): Int; def outerX = x }; val s: L = () => 2; s.get() + s.outerX }
  def javaInner: Int = { val s: Sam = () => 3; s.get() + s.outerX() }
  def otherJava: Int = { val s: j.Sam = () => 4; s.get() + s.outerX() }
  def otherScala: Int = { val s: o.Sam = () => 5; s.get() + s.outerY }
  def inLocalClass: Int = { class L { def m: Sam = () => 6 }; val s = new L().m; s.get() + s.outerX() }
  def localToLambda: WithVal = () => { abstract class L { def get(): Int; def outerX = x }; val s: L = () => 7; s.get() + s.outerX }
  def nestedJavaInner: WithVal = () => { val s: Sam = () => 8; s.get() + s.outerX() }
  // the prefix is captured as a local value, so the outer parameter is not needed for the super constructor call
  def stablePrefix(p: Other): p.Sam = () => 9
}

object Test {
  def roundTrip[T](o: T): T = {
    val bo = new ByteArrayOutputStream
    val oo = new ObjectOutputStream(bo)
    oo.writeObject(o)
    oo.close()
    new ObjectInputStream(new ByteArrayInputStream(bo.toByteArray)).readObject().asInstanceOf[T]
  }

  def capturesOuter(o: AnyRef): Boolean =
    o.getClass.getDeclaredFields.exists(f => !java.lang.reflect.Modifier.isStatic(f.getModifiers) && !f.getType.isPrimitive)

  def check(name: String, f: AnyRef, captures: Boolean)(result: AnyRef => Any): Unit = {
    val serialized = try result(roundTrip(f)).toString catch { case e: NotSerializableException => e.toString }
    println(s"$name: ${result(f)}, captures outer: ${capturesOuter(f)}, serialized: $serialized")
    assert(capturesOuter(f) == captures, name)
  }

  def main(args: Array[String]): Unit = {
    val h = new Holder
    check("bridged", h.bridged, captures = false)(_.asInstanceOf[GetInt].get())
    check("withVal", h.withVal, captures = false)(_.asInstanceOf[WithVal].get())
    check("fun", h.fun, captures = false)(_.asInstanceOf[Fun](1))
    check("local", h.local(1), captures = false)(_.asInstanceOf[GetInt].get())
    check("nested", h.nested, captures = false)(_.asInstanceOf[GetInt].get())
    check("localDef", h.localDef, captures = false)(_.asInstanceOf[GetInt].get())
    check("inner", (new h.Inner).bridged, captures = false)(_.asInstanceOf[GetInt].get())
    check("trait", (new HolderTrait {}).bridged, captures = false)(_.asInstanceOf[GetInt].get())

    check("usesField", h.usesField, captures = true)(_.asInstanceOf[GetInt].get())
    check("usesMember", h.usesMember, captures = true)(_.asInstanceOf[WithVal].get())
    check("nestedUsesThis", h.nestedUsesThis, captures = true)(_.asInstanceOf[GetInt].get())
    check("localDefUsesThis", h.localDefUsesThis, captures = true)(_.asInstanceOf[GetInt].get())

    check("pf", h.pf, captures = false)(_.asInstanceOf[PartialFunction[Any, Int]](1))
    check("pfUsesThis", h.pfUsesThis, captures = true)(_.asInstanceOf[PartialFunction[Any, Int]](1))
    check("selfReaching", h.selfReaching, captures = true)(_.asInstanceOf[WithVal].get())

    def value(name: String)(result: => Any): Unit = println(s"$name: ${try result catch { case e: NullPointerException => e }}")
    val outers = new Outers(new Other, new J_1)
    value("member")(outers.member)
    value("localClass")(outers.localClass)
    value("javaInner")(outers.javaInner)
    value("otherJava")(outers.otherJava)
    value("otherScala")(outers.otherScala)
    value("inLocalClass")(outers.inLocalClass)
    value("localToLambda")(outers.localToLambda.get())
    value("nestedJavaInner")(outers.nestedJavaInner.get())
    val s = outers.stablePrefix(outers.o)
    value("stablePrefix")(s.get() + s.outerY)
    assert(!s.getClass.getDeclaredFields.exists(_.getType == classOf[Outers]))
  }
}
