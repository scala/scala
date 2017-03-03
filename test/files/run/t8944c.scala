case class Foo[A](private val ant: Any, elk: Any, private val cat: A*)

object Test {
  def main(args: Array[String]): Unit = {
    def pred(name: String) = Set("ant", "elk", "cat").exists(name contains _)
    println(classOf[Foo[_]].getDeclaredMethods.filter(m => pred(m.getName)).sortBy(_.getName).mkString("\n"))  
  }
}
