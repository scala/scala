// macros should be built separately from their clients, so simple "scalac Printf.scala Test.scala" won't work
// 1) first build this file with "scalac -Xmacros Printf.scala"
// 2) the build the test with "scalac -cp <output directory of compiling Printf.scala> Test.scala"

object Printf extends App {
  def macro printf(format: String, params: Any*) : String = {
    var i = 0
    def gensym(name: String) = { i += 1; newTermName(name + i) }

    def createTempValDef(value: Tree, clazz: Class[_]): (Option[Tree], Tree) = {
      val local = gensym("temp")
      val tpe = if (clazz == classOf[Int]) Ident(newTypeName("Int"))
        else if (clazz == classOf[String]) Select(Select(Ident(newTermName("java")), newTermName("lang")), newTypeName("String"))
        else throw new Exception("unknown class " + clazz.toString)
      (Some(ValDef(Modifiers(), local, tpe, value)), Ident(local))
    }

    def tree_printf(format: Tree, params: Tree*) = {
      val Literal(Constant(s_format: String)) = format
      val paramsStack = scala.collection.mutable.Stack(params: _*)
      val parsed = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
        case "%d" => createTempValDef(paramsStack.pop, classOf[Int])
        case "%s" => createTempValDef(paramsStack.pop, classOf[String])
        case "%%" => (None, Literal(Constant("%")))
        case part => (None, Literal(Constant(part)))
      }

      val evals = for ((Some(eval), _) <- parsed if eval != None) yield eval
      val prints = for ((_, ref) <- parsed) yield {
        val print = Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName("print"))
        Apply(print, List(ref))
      }

      Block((evals ++ prints).toList, Literal(Constant(())))
    }

    tree_printf(format, params: _*)
  }
}
