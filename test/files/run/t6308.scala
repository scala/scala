import scala.{specialized => sp}

// NOTE: `{ val c = caller; print(""); c }` is used instead of a simple `caller`,
//       because we want to prevent tail-call optimization from eliding the stack-
//       frames we want to inspect.

object Test {
  def caller = new Exception().getStackTrace()(1).getMethodName
  def f1[@sp(Int) A](a: A, b: Any)             = { val c = caller; print(""); c }
  def f2[@sp(Int) A, B](a: A, b: String)       = { val c = caller; print(""); c }
  def f3[B, @sp(Int) A](a: A, b: List[B])      = { val c = caller; print(""); c }
  def f4[B, @sp(Int) A](a: A, b: List[(A, B)]) = { val c = caller; print(""); c }

  def f5[@sp(Int) A, B <: Object](a: A, b: B)  = { val c = caller; print(""); c }

  type T = Object
  def aliasF1[@sp(Int) A, B <: T](a: A, b: String)           = { val c = caller; print(""); c }
  def aliasF2[@sp(Int) A, B <: AnyRef](a: A, b: String)      = { val c = caller; print(""); c }
  def aliasF3[B <: List[A], @specialized(Int) A](a: A, b: B) = { val c = caller; print(""); c }

  def main(args: Array[String]) {
    val s = ""
    val result =
      s"""|- Unspecialized type args
          |// Specialized
          |f1 ${f1(1,"some ref")}
          |f2 ${f2(1,"some ref")}
          |f3 ${f3(1,List("some ref"))}
          |f4 ${f4(1,Nil)}
          |f5 ${f5(1,s)}
          |
          |// Unspecialized type args
          |f4(Boolean) ${f4(Boolean,Nil)}
          |f4(String)  ${f4("",Nil)}
          |
          |aliasF1 ${aliasF1(1,s)}
          |aliasF2 ${aliasF2(1,s)}
          |aliasF3 ${aliasF3(1,List(0))}""".stripMargin
    println(result)
  }
}
