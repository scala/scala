import scala.{specialized => sp}

object Test {
  def caller = new Exception().getStackTrace()(1).getMethodName
  def f1[@sp(Int) A](a: A, b: Any)             = caller
  def f2[@sp(Int) A, B](a: A, b: String)       = caller
  def f3[B, @sp(Int) A](a: A, b: List[B])      = caller
  def f4[B, @sp(Int) A](a: A, b: List[(A, B)]) = caller

  def f5[@sp(Int) A, B <: Object](a: A, b: B)  = caller

  // `uncurryTreeType` calls a TypeMap on the call to this method and we end up with new
  // type parameter symbols, which are not found in `TypeEnv.includes(typeEnv(member), env)`
  // in `specSym`. (One of `uncurry`'s tasks is to expand type aliases in signatures.)
  type T = Object
  def todo1[@sp(Int) A, B <: T](a: A, b: String)           = caller
  def todo2[@sp(Int) A, B <: AnyRef](a: A, b: String)      = caller
  def todo3[B <: List[A], @specialized(Int) A](a: A, b: B) = caller

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
          |// Ideally these would be specialized
          |todo1 ${todo1(1,s)}
          |todo2 ${todo2(1,s)}
          |todo3 ${todo3(1,List(0))}""".stripMargin
    println(result)
  }
}
