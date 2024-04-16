//> using javaOpt -Dneeds.forked.jvm.for.windows
// filter out absolute path to java
//> using filter hello.Hello

import java.io.File
import scala.tools.partest.ReplTest
import scala.util.Properties.propOrElse

object Test extends ReplTest {
  def java = propOrElse("javacmd", "java")
  def code = s""":sh $java -classpath $testOutput hello.Hello
                |.lines.foreach(println)""".stripMargin
}

package hello {
  object Hello {
    def main(a: Array[String]): Unit = {
      System.out.println("shello, world.")
    }
  }
}

