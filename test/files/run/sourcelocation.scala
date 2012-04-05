
import reflect.SourceLocation

object Test {

  def printInfo(m: SourceLocation) {
    def relative(name: String) = {
      val lastSlash = name.lastIndexOf('/')
      if (lastSlash == -1)
        name.substring(name.lastIndexOf('\\') + 1)
      else
        name.substring(lastSlash + 1)
    }
    println("line: "+m.line)
    println("char offset: "+m.charOffset)
    println("file: "+relative(m.fileName))
  }

  def inspect[T](x: T)(implicit m: SourceLocation): Int = {
    def withManifest()(implicit mm: SourceLocation) {
      printInfo(mm)
    }
    printInfo(m)
    withManifest()
    0
  }

  def main(args: Array[String]) {
    val l = List(1, 2, 3)
    val x = inspect(l)
    val y = {
      val z = 4*7
      inspect(l)
    }
  }

}
