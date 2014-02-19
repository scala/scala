import scala.reflect.runtime.universe._

object Test {
  def main(args: Array[String]) {
    val mc = new MegaClass
    val anns = mc.getClass.getAnnotations.map(_.annotationType.getName).toList.sorted
    println(s"class annotations: $anns")
    val N = typeTag[MegaClass].tpe.decls.size // was:  error reading Scala signature of MegaClass: 65935
    println(s"$N decls via runtime reflection")
  }
}
