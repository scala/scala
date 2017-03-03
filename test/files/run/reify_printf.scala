import java.io.{ ByteArrayOutputStream, PrintStream }
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.reflect.api._
import scala.reflect.api.Trees
import scala.reflect.internal.Types

object Test extends App {
  //val output = new ByteArrayOutputStream()
  //Console.setOut(new PrintStream(output))
  val toolbox = cm.mkToolBox()

  val tree = tree_printf(reify("hello %s").tree, reify("world").tree)
  val evaluated = toolbox.eval(tree)
  //assert(output.toString() == "hello world", output.toString() +" ==     hello world")

  /*
  // upd. Oh, good old times, our very-very first experiments with macros :)
  macro def printf(format: String, params: Any*) : String = tree_printf(format: Tree, (params: Seq[Tree]): _*)
   */

  var i = 0
  def gensym(name: String) = { i += 1; TermName(name + i) }

  def createTempValDef( value : Tree, tpe : Type ) : (Option[Tree],Tree) = {
    val local = gensym("temp")
    (
      Some(
        ValDef(
          NoMods
          , local
          , TypeTree(tpe)
          , value
        )
      )
      , Ident(local)
    )
  }

  def tree_printf(format: Tree, params: Tree*) = {
    val Literal(Constant(s_format: String)) = format
    val paramsStack = scala.collection.mutable.Stack(params: _*)
    val parsed = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => createTempValDef( paramsStack.pop, typeOf[Int] )
      case "%s" => createTempValDef( paramsStack.pop, typeOf[String] )
      case "%%" => {
        (None:Option[Tree], Literal(Constant("%")))
      }
      case part => {
        (None:Option[Tree], Literal(Constant(part)))
      }
    }

    val evals = for ((Some(eval), _) <- parsed if eval != None) yield (eval: Tree)
    val prints = for ((_, ref) <- parsed) yield
      Apply(
        Select(
          Select(
            Ident( TermName("scala") )
            , TermName("Predef")
          )
          , TermName("print")
        )
        , List(ref)
      ): Tree
    Block((evals ++ prints).toList, Literal(Constant(())))
  }
}
