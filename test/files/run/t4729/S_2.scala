import scala.language.reflectiveCalls
 // Scala class:
class ScalaVarArgs extends J_1 {
  // -- no problem on overriding it using ordinary class
  def method(s: String*) { println(s) }
}

object Test {
  def main(args: Array[String]) {
    //[1] Ok - no problem using inferred type
    val varArgs = new J_1 {
      def method(s: String*) { println(s) }
    }
    varArgs.method("1", "2")

    //[2] Ok -- no problem when explicit set its type after construction
    val b: J_1 = varArgs
    b.method("1", "2")

    //[3] Ok -- no problem on calling its method
    (new ScalaVarArgs).method("1", "2")
    (new ScalaVarArgs: J_1).method("1", "2")

    //[4] Not Ok -- error when assigning anonymous class to an explictly typed val
    // Compiler error:  object creation impossible, since method method in trait VarArgs of type (s: <repeated...>[java.lang.String])Unit is not defined
    val tagged: J_1 = new J_1 {
      def method(s: String*) { println(s) }
    }
  }
}
