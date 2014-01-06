import scala.tools.partest._
import scala.tools.nsc._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp"

  def code = """
    class testAnn extends annotation.TypeConstraint

    class A(param: Double) extends { val x: Int = 1; val y = "two" } with AnyRef {
      type T = A

      val inferField = ("str": @testAnn)
      val annotField: Boolean @testAnn = false

      val lub1 = List('c', (1: Int @testAnn), "")
      val lub2 = if (annotField) (1: @testAnn) else 2

      def foo[T <: Int] = 0
      foo[Int @testAnn]

      var count = 0

      math.random // some statement

      def method: String = {
        math.random
        val f = inferField

        def nested(): String = {
          if(count == 1)
            return f
          "huhu"
        }
        nested()
      }

      def this(str: String) {
        this(str.toDouble)
        math.random
        count += 1
      }
    }
  """.trim


  def show() {
    val global = newCompiler()
    import global._
    import analyzer._

    val output = collection.mutable.ListBuffer[String]()

    object annotChecker extends AnnotationChecker {
      def hasTestAnn(tps: Type*) = {
        tps exists (_.annotations.map(_.toString) contains "testAnn")
      }

      def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
        if (hasTestAnn(tpe1, tpe2))
          output += s"annotationsConform($tpe1, $tpe2)"
        true
      }

      override def annotationsLub(tp: Type, ts: List[Type]): Type = {
        if (hasTestAnn(ts: _*))
          output += s"lub($ts)"
        tp
      }

      override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
        if (hasTestAnn(targs: _*))
          output += s"adaptBoundsToAnnots($bounds, $tparams, $targs)"
        bounds
      }
    }

    object analyzerPlugin extends AnalyzerPlugin {
      def treeClass(t: Tree) = t.getClass.toString.split('.').last

      override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
        output += s"pluginsPt($pt, ${treeClass(tree)})"
        pt
      }

      override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
        output += s"pluginsTyped($tpe, ${treeClass(tree)})"
        tpe
      }

      override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {
        output += s"pluginsTypeSig(${defTree.symbol}, ${treeClass(defTree)})"
        tpe
      }

      override def pluginsTypeSigAccessor(tpe: Type, typer: Typer, tree: ValDef, sym: Symbol): Type = {
        output += s"pluginsTypeSigAccessor(${tree.symbol})"
        tpe
      }


      override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
        output += s"canAdaptAnnotations(${treeClass(tree)}, $pt)"
        false
      }

      override def pluginsTypedReturn(tpe: Type, typer: Typer, tree: Return, pt: Type): Type = {
        output += s"pluginsTypedReturn($tree, $pt)"
        tpe
      }

    }

    addAnnotationChecker(annotChecker)
    addAnalyzerPlugin(analyzerPlugin)
    compileString(global)(code)

    val res = output.groupBy(identity).mapValues(_.size).map { case (k,v) => s"$k [$v]" }.toList.sorted
    println(res.mkString("\n"))
  }

}
