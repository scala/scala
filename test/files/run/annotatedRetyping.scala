import scala.tools.partest._
import scala.tools.nsc._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp"

  def code = """
    class testAnn extends annotation.Annotation

    object t {
      def nt = 1
      def tr = "a"
    }

    class Test {
      List(1,2).map(x => {
        val another = ((t.nt, t.tr): @testAnn) match { case (_, _) => 1 }
        x
      })
    }
  """.trim


  // point of this test: type-check the "Annotated" tree twice. first time the analyzer plugin types it,
  // second time the typer.

  // bug was that typedAnnotated assigned a type to the Annotated tree. The second type check would consider
  // the tree as alreadyTyped, which is not cool, the Annotated needs to be transformed into a Typed tree.

  def show() {
    val global = newCompiler()
    import global._
    import analyzer._
    import collection.{mutable => m}

    object analyzerPlugin extends AnalyzerPlugin {
    val templates: m.Map[Symbol, (Template, Typer)] = m.Map()
      override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {
        defTree match {
          case impl: Template =>
            templates += typer.context.owner -> (impl, typer)

          case dd: DefDef if dd.symbol.isPrimaryConstructor && templates.contains(dd.symbol.owner) =>
            val (impl, templTyper) = templates(dd.symbol.owner)
            for (stat <- impl.body.filterNot(_.isDef)) {
              println("typing "+ stat)
              val statsOwner = impl.symbol orElse templTyper.context.owner.newLocalDummy(impl.pos)
              val tpr = analyzer.newTyper(templTyper.context.make(stat, statsOwner))
              tpr.typed(stat)
            }

          case _ =>
        }
        tpe
      }
    }

    addAnalyzerPlugin(analyzerPlugin)
    compileString(global)(code)
  }
}
