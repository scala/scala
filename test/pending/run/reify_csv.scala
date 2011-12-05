import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val csv = """
    |    phase name;  id;  description
    |        parser;   1;  parse source into ASTs, perform simple desugaring
    |         namer;   2;  resolve names, attach symbols to named trees
    |packageobjects;   3;  load package objects
    |         typer;   4;  the meat and potatoes: type the trees
    |superaccessors;   5;  add super accessors in traits and nested classes
    |       pickler;   6;  serialize symbol tables
    |     refchecks;   7;  reference/override checking, translate nested objects
    |  selectiveanf;   8;
    |      liftcode;   9;  reify trees""".stripMargin.split("\n").map{_.trim()}.drop(1).toList

  val fields = csv.head.split(";").map{_.trim()}.toList
  println(fields)

  val code = scala.reflect.Code.lift({
    object Csv {
      case class record(`phase name`: String, id: String, description: String)

      object record {
        def parse(lines: List[String]) = {
          lines drop(1) map { line => line.split(";", -1).toList match {
            case phase$whitespace$name :: id :: description :: _ => record(phase$whitespace$name.trim(), id.trim(), description.trim())
            case _ => throw new Exception("format error")
          }}
        }
      }
    }

    Csv.record.parse(csv) foreach println
  })

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
