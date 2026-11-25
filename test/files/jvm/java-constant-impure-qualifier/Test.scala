// A constant-typed Java field selected through an impure qualifier is not folded:
// the qualifier is evaluated and null-checked. Java sources are compiled jointly.
import scala.tools.asm.Opcodes.GETFIELD
import scala.tools.asm.tree.FieldInsnNode
import scala.tools.partest.BytecodeTest
import scala.jdk.CollectionConverters._

object Holder { val k: K = new K }

class Sub extends K { def viaThis = this.inst; def viaIdent = inst }

class C { final val X = 42 }

// annotation arguments are not evaluated, so the qualifier is irrelevant there and the value is accepted
@SerialVersionUID(K.make().inst) class Annotated extends Serializable

object Use {
  def make(): C = { println("make C"); null }

  // impure qualifiers: keep the field access
  def nullInst: Int = K.make().inst
  def nullStr: String = K.make().str
  def nonNullInst: Int = K.mk().inst
  def plusOne: Int = K.mk().inst + 1
  def literalTyped: 42 = K.mk().inst
  final val finalVal = K.mk().inst
  def finalValPlusOne: 43 = finalVal + 1
  def toLong: Long = K.mk().inst
  def toByte: Byte = K.mk().inst
  def plusOneToLong: Long = K.mk().inst + 1
  def negate: Int = -K.mk().inst
  def cond: String = if (K.mk().flag) "yes" else "no"
  def and: Boolean = K.mk().flag && K.mk().flag
  def closure: () => Int = () => K.mk().inst
  def byName(k: => K): Int = k.inst
  def scalaPlusOne: Int = make().X + 1

  // pure qualifiers: still inlined
  def local: Int = { val k: K = null; k.inst }
  def stablePath: Int = Holder.k.inst
  def static: Int = K.STATIC
  def pureFolded: 43 = Holder.k.inst + 1
}

object Test extends BytecodeTest {
  def run(label: String)(body: => Any): Unit =
    try println(s"$label: ${body}")
    catch { case _: NullPointerException => println(s"$label: NPE") }

  def show(): Unit = {
    run("nullInst")(Use.nullInst)
    run("nullStr")(Use.nullStr)
    run("nonNullInst")(Use.nonNullInst)
    run("plusOne")(Use.plusOne)
    run("literalTyped")(Use.literalTyped)
    run("finalVal")(Use.finalVal)
    run("finalValPlusOne")(Use.finalValPlusOne)
    run("toLong")(Use.toLong)
    run("toByte")(Use.toByte)
    run("plusOneToLong")(Use.plusOneToLong)
    run("negate")(Use.negate)
    run("cond")(Use.cond)
    run("and")(Use.and)
    run("closure")(Use.closure())
    run("byName")(Use.byName(K.mk()))
    run("scalaPlusOne")(Use.scalaPlusOne)
    run("local")(Use.local)
    run("stablePath")(Use.stablePath)
    run("static")(Use.static)
    run("pureFolded")(Use.pureFolded)
    run("viaThis")(new Sub().viaThis)
    run("annotation")(java.io.ObjectStreamClass.lookup(classOf[Annotated]).getSerialVersionUID)

    val use = loadClassNode("Use$")
    def readsField(m: String) = getMethod(use, m).instructions.iterator.asScala.exists {
      case f: FieldInsnNode => f.getOpcode == GETFIELD && f.owner == "K"
      case _                => false
    }
    for (m <- List("nullInst", "nullStr", "nonNullInst", "plusOne", "literalTyped", "toLong", "toByte", "plusOneToLong", "negate", "byName", "local", "stablePath", "pureFolded"))
      println(s"$m reads K field: ${readsField(m)}")
    println(s"finalVal has a field: ${use.fields.asScala.exists(_.name == "finalVal")}")
    val sub = loadClassNode("Sub")
    for (m <- List("viaThis", "viaIdent"))
      println(s"Sub.$m reads K field: ${getMethod(sub, m).instructions.iterator.asScala.exists(_.getOpcode == GETFIELD)}")
  }
}
