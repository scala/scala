class C {
  def m = {
    val f = (x: String) => x.trim
    f(" H ae i  ")
  }
}

object Test extends App {
  val testClassesDir = System.getProperty("partest.output")
  scala.tools.scalap.Main.main(Array("-cp", testClassesDir, "C"))
}