import scala.reflect.runtime.universe._
object test extends App {
  val a = TermName("a")
  val t = TypeName("t")
  q"def $a f"
  q"$a("
  q"class $t { def foo = $a"
  q"import $t $t"
  q"package p"
  q"foo@$a"
  q"case class A"
  tq"$t => $t $t]"
  cq"pattern => body ; case pattern2 =>"
  pq"$a(bar"
}