import scala.tools.asm.Opcodes
import scala.tools.asm.tree._
import scala.tools.partest.BytecodeTest

import scala.collection.JavaConverters._

object Test extends BytecodeTest {

  def check(cls: String, mth: String) = {
    val clasz = loadClassNode(s"strictfp/$cls")
    //println(clasz.methods.asScala.map(_.name).toList)
    val meth  = clasz.methods.asScala.find(_.name == mth).get
    println(s"$cls.$mth: ${(meth.access & Opcodes.ACC_STRICT) != 0}")
  }

  override def show() = {
    check("A", "foo")
    check("A", "bar$1")
    check("B", "foo")
    check("B", "bar$2")
    check("C", "foo")
    check("C", "bar$3")
    check("D", "foo")
    check("D", "bar$4")
    check("E", "foo")
    check("E$", "foo$extension")
    check("E$", "bar$5")
    check("F", "foo")
    check("F$", "foo$extension")
    check("F$", "bar$6")
    check("G$I", "foo")
    check("G$I", "bar$7")
    check("G$I$", "foo")
    check("G$I$", "bar$8")
    check("H$I", "foo")
    check("H$I", "bar$9")
    check("H$I$", "foo")
    check("H$I$", "bar$10")
    check("I", "foo")
    check("I$", "foo")
    check("I$", "foo$extension")
    check("I$", "bar$11")
    check("I$", "bar$12")
    check("J", "foo")
    check("J$M$1", "foo")
    check("J$M$1", "bar$13")
    check("K", "foo")
    check("K$M$2", "foo")
    check("K$M$2", "bar$14")
  }
}