import scala.tools.partest.BytecodeTest
import scala.collection.JavaConverters._

package p1 {
  package p2 {
    object Singleton {
      object Singleton {
        object Singleton
      }
    }
  }
}

class A1 {
  class B1 {
    @scala.beans.BeanInfo
    class C1
  }
}

class A2 {
  class B2 {
    class C2
  }
  def f: B2#C2 = null
}


object Test extends BytecodeTest {
  import p1.p2._

  def nested(c: Class[_]) = s" ${c.getName}: ${c.getDeclaredClasses.toList}"

  def nprintln(s: String) = println("\n"+s)
  def printInner(cname: String): Unit = {
    val cnode = loadClassNode(cname)
    println(cnode.innerClasses.asScala.toList.map(i => s"className[${i.name}] outerClassName[${i.outerName}] innerName[${i.innerName}] access[${i.access}]").mkString(" ", "\n ", ""))
  }

  def show() {

    println("getClass on module gives module class")
    println(" " + Singleton.Singleton.getClass)

    nprintln("Nested module classes are found through reflection")
    println(nested(Singleton.Singleton.getClass))

    nprintln("Reflection can find direct nested classes (A1-B1-C1)")
    println(nested(classOf[A1]))
    println(nested(classOf[A1#B1]))
    println(nested(classOf[A1#B1#C1]))

    nprintln("Reflection can find direct nested classes (A2-B2-C2)")
    println(nested(classOf[A2]))
    println(nested(classOf[A2#B2]))
    println(nested(classOf[A2#B2#C2]))

    nprintln("The InnerClass attribute of a mirror class contains the members of the module class:")
    printInner("p1.p2.Singleton") // mirror class
    println("The module members are not in the InnerClass table of the module class (unless referenced):")
    printInner("p1.p2.Singleton$")

    nprintln("An outer class has a InnerClass attribute for direct nested classes")
    printInner("A1")
    println("A nested class has an InnerClass attribute for itself (and also for its nested classes)")
    printInner("A1$B1")
    println("C1 is a nested class, so it has an InnerClass attribute for itself.\n"+
            "Because that attribute leads to an entry for B1 in the constant pool, C1 needs an InnerClass attribute for B1.")
    printInner("A1$B1$C1")

    nprintln("The BeanInfo class has the same InnerClass attributes as the corresponding bean")
    printInner("A1$B1$C1BeanInfo")

    nprintln("Class A2 mentions class C2 in the constant pool (due to method  f), therefore it needs an InnerClass attribute for C1")
    printInner("A2")
    println("B2")
    printInner("A2$B2")
    println("C2")
    printInner("A2$B2$C2")
  }
}
