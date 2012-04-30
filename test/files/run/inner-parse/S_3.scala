import scala.reflect.internal.JvmClassInfo

object Test {
  def main(args: Array[String]): Unit = {
    val cwd = sys.props("partest.output")
    
    for ((f, info) <- JvmClassInfo.classInfoList(cwd)) {
      println("file " + f.stripPrefix(cwd).substring(1).replace('\\', '/'))
      println(info)
    }
  }
}
