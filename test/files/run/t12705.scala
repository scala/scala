
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code =
    """|case class Person(name: String)
       |$intp.classLoader.getResource("Person.class")
       |$intp.classLoader.loadClass("Person")""".stripMargin
       //|classOf[Person].getClassLoader.loadClass("Person")""".stripMargin
}

/*
java.lang.NoClassDefFoundError: Person (wrong name: Person)
  at java.base/java.lang.ClassLoader.defineClass1(Native Method)
  at java.base/java.lang.ClassLoader.defineClass(ClassLoader.java:1013)
  at scala.reflect.internal.util.AbstractFileClassLoader.findClass(AbstractFileClassLoader.scala:77)
  at java.base/java.lang.ClassLoader.loadClass(ClassLoader.java:588)
  at java.base/java.lang.ClassLoader.loadClass(ClassLoader.java:521)
  ... 76 elided
*/
