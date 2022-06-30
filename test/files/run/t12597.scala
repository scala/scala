import scala.tools.partest._
import scala.collection.mutable.LinkedHashMap

object Test extends CompilerTest {
  import global._
  override def extraSettings = super.extraSettings + " -Yrangepos -Ystop-after:parser"
  val tests = List(
    "class A1 { def t        = <a/>                      }",
    "class A2 { def t        = <a></a>                   }",
    "class A3 { def t        = <a></a>.toString          }",
    "class A4 { def t        = (<a></a>).toString        }",
    "class A5 { def t        = { <a></a> }.toString      }",
    "class A6 { def t        = <a></a><b/>               }",
    "class A7 { def t        = <a></a><b/>.toString      }",
    "class B1 { def t(c: A1) = c.toString                }",
    "class B2 { def t(c: A1) = c toString                }",
    "class B3 { def t(c: A1) = { val x = c; x }.toString }",
     //                        ^ 26                ^ 36
  )

  override def sources = tests

  def check(source: String, unit: CompilationUnit): Unit = unit.body foreach {
    case dd: DefDef if dd.name.startsWith("t") =>
      val poss = dd.rhs.collect {
        case t if t.pos != NoPosition => (t.pos.start, t.pos.end)
      }.distinct
      println(poss)
    case _ =>
  }
}
