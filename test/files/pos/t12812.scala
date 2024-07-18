
//> using options -Werror -Xsource:3 -language:postfixOps -Xlint

class C {
  def foo(max: Int) = (1 to max).map(1 to).foreach(r => println(r.mkString(",")))
}

//java.lang.NullPointerException: Cannot invoke "scala.reflect.internal.Symbols$Symbol.owner()" because the return value of "scala.reflect.internal.Trees$Tree.symbol()" is null
