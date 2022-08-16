// javaVersion: 11+
// scalac: -release:11

trait T { val x = 42 }
class C extends T
object Test {
  def main(args: Array[String]): Unit = { new C(); () }
}
