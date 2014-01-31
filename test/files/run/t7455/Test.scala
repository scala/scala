import scala.tools.partest._

// javac adds dummy parameters of type Outer$1 to synthetic access constructors
// This test shows that we strip them from the signatures. If we don't, we trigger
// parsing of Outer$1 which can fail if it references type parameters of the Outer.
//
// OLD OUTPUT:
//  private[package <empty>] def <init>(x$2: Outer$1): Outer$PrivateInner
//  error: error while loading Outer$1, class file 't7455-run.obj/Outer$1.class' is broken
//  (class java.util.NoSuchElementException/key not found: E)
//  ...
object Test extends DirectTest {
  override def code = ""

  def show {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    val compiler = newCompiler("-cp", classpath, "-d", testOutput.path)
    import compiler._, definitions._
    new compiler.Run

    for {
      name <- Seq("Outer", "Outer$PrivateInner", "Outer$PrivateStaticInner", "Outer$PublicInner")
      clazz = compiler.rootMirror.staticClass(name)
      constr <- clazz.info.member(termNames.CONSTRUCTOR).alternatives
    } {
      println(constr.defString)
      fullyInitializeSymbol(constr)
    }
  }
}
