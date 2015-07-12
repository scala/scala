import scala.tools.partest._
import scala.tools.nsc._

object Test extends DirectTest {

  def code = """
    class C { class D }
    object O
  """.trim

  def show() {
    for (b <- List("GenASM", "GenBCode")) {
      val global = newCompiler("-usejavacp", s"-Ybackend:$b")
      import global._
      val r = new Run
      r.compileSources(newSourceFile(code) :: Nil)
      
      val results = collection.mutable.Buffer[(Boolean, String)]()

      // Nailing down defacto compiler API from SBT's usage
      // https://github.com/sbt/sbt/blob/adb41611cf73260938274915d8462d924df200c8/compile/interface/src/main/scala/xsbt/Analyzer.scala#L29-L41
      def isTopLevelModule(sym: Symbol) = sym.isTopLevel && sym.isModule
      for (unit <- currentRun.units if !unit.isJava) {
        val sourceFile = unit.source.file.file
        for (iclass <- unit.icode) {
          val sym = iclass.symbol
          def addGenerated(separatorRequired: Boolean) {
            results += (separatorRequired -> sym.fullName)
          }
          if (sym.isModuleClass && !sym.isImplClass) {
            if (isTopLevelModule(sym) && sym.companionClass == NoSymbol)
              addGenerated(false)
            addGenerated(true)
          } else
            addGenerated(false)
        }
      }
      val expected = List((false, "C"), (true, "O"), (false, "C$D"))
      assert(results.toList == expected, b + ": " + results.toList)
    }
  }
}
