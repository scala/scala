// Main.scala
package object ops {
  object A
  def A(a: Any) = ()
}

object Test {
  def main(args: Array[String]): Unit = {
    val pack = scala.reflect.runtime.currentMirror.staticModule("ops.package")
    println(pack.info.decls.toList.map(_.toString).sorted.mkString("\n"))
  }
}
