import java.io._

import scala.reflect.io.AbstractFile
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.util._
import scala.reflect.internal.util.AbstractFileClassLoader

object Test {
  def main(args: Array[String]) {
    run()
  }

  def run(): Unit = {
    val settings = new Settings()
    settings.Yreplclassbased.value = true
    settings.usejavacp.value = true

    var imain: IMain = null
    object extract extends ((AnyRef) => Unit) with Serializable {
      var value: AnyRef = null

      def apply(a: AnyRef) = value = a
    }

    val code =
      """val x = {println("  evaluating x"); 0 }
        |def getX() = x
        |class U extends Serializable { println("constructing U"); val x = 0 ; override def toString = "U" }
        |lazy val y = {println("  evaluating y"); 0 }
        |class D; val z = {println("  evaluating z"); 0}; val zz = {println("  evaluating zz"); 0}
        |object O extends Serializable { val apply = {println("  evaluating O"); 0} }
        |class A(i: Int) { println("  constructing A") }
        |type AA = A
        |val u = new U()
        |extract(() => new AA(x + getX() + y + z + zz + O.apply + u.x))
      """.stripMargin

    imain = new IMain(settings)
    println("== evaluating lines")
    imain.directBind("extract", "(AnyRef => Unit)", extract)
    code.lines.foreach(imain.interpret)

    val virtualFile: AbstractFile = extract.value.getClass.getClassLoader.asInstanceOf[AbstractFileClassLoader].root
    val newLoader = new AbstractFileClassLoader(virtualFile, getClass.getClassLoader)

    def deserializeInNewLoader(string: Array[Byte]): AnyRef = {
      val bis = new ByteArrayInputStream(string)
      val in = new ObjectInputStream(bis) {
        override def resolveClass(desc: ObjectStreamClass) = Class.forName(desc.getName, false, newLoader)
      }
      in.readObject()
    }
    def serialize(o: AnyRef): Array[Byte] = {
      val bos = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bos)
      out.writeObject(o)
      out.close()
      bos.toByteArray
    }
    println("== evaluating lambda")
    extract.value.asInstanceOf[() => Any].apply()
    println("== reconstituting into a fresh classloader")
    val reconstituted = deserializeInNewLoader(serialize(extract.value)).asInstanceOf[() => Any]
    println("== evaluating reconstituted lambda")
    reconstituted.apply() // should not print("evaluating x") a second time
  }
}
