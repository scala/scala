import scala.{specialized => spec}

class C1[@spec(Int, AnyRef) A, @spec(Int, AnyRef) B](v:A, w:B)

class C2[@spec(Unit, Boolean, Byte, Char, Short, Int, Long, Float, Double, AnyRef) A, @spec(Unit, Boolean, Byte, Char, Short, Int, Long, Float, Double, AnyRef) B](v:A, w:B)

object Test {
  def main(args:Array[String]) {
    println(new C1("abc", 123).getClass.getName)
    println(new C2[String, Int]("abc", 123).getClass.getName)
  }
}
