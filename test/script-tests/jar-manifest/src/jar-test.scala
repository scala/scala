import scala.tools.nsc.util.HasClassPath

package bippy {
  object Runner {
    var line = 0
    def echo(msgs: Any*) = {
      line += 1
      Console.println("%-2s %s".format(line, msgs mkString " "))
    }

    def bippyBoo(quuxParameter: Int) = 5
    def bippyBingle(imparametorium: String, antidisestablish: Int, x: Float) = ()

    def main(args: Array[String]): Unit = {
      echo(new dingus.Printable)
      val namer = new com.thoughtworks.paranamer.BytecodeReadingParanamer
      getClass.getMethods filter (_.getName startsWith "bippy") foreach { m =>
        echo(m.getName, "has parameters:", namer.lookupParameterNames(m).mkString(", "))
      }
      echo("")
      echo("Urls exposed through the classloader:")
      getClass.getClassLoader match {
        case x: HasClassPath => x.classPathURLs foreach (x => echo(x))
        case _               => echo("None! Seems unlikely we'd get this far then.")
      }
    }
  }
}

package dingus {
  class Printable {
    override def toString = "\"Greetings from dingus.jar!\""
  }
}