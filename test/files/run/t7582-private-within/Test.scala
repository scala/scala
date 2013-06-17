import scala.tools.partest.DirectTest

// Testing that the `privateWithin` field is correctly populated on all
// the related symbols (e.g. module class) under separate compilation.
object Test extends DirectTest {
  def code = ???

  def show(): Unit = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    val global = newCompiler("-usejavacp", "-cp", classpath, "-d", testOutput.path)
    import global._
    withRun(global) { _ =>
      def check(sym: Symbol) = {
        sym.initialize
        println(f"${sym.accessString}%12s ${sym.accurateKindString} ${sym.name.decode}") // we want to see private[pack] for all of these.
      }
      val sym = rootMirror.getRequiredClass("pack.JavaPackagePrivate")
      val syms = Seq(sym, sym.companionModule, sym.companionModule.moduleClass)
      (syms ++ syms.flatMap(_.info.decls)).foreach(check)
    }
  }
}
