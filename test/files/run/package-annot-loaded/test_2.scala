import scala.tools.partest.CompilerTest

object Test extends CompilerTest {

  override def extraSettings: String = "-usejavacp -nowarn -Ystop-after:typer"

  // Compiler doesn't compile anything. We just check that package
  // symbol contains annotations when loaded from class files
  override def code = ""

  def check(source: String, unit: global.CompilationUnit): Unit = {
    import global._

    // Package symbol
    val packageSym = rootMirror.staticPackage("foo.bar")
    // Package object symbol
    val poSym = rootMirror.staticModule("foo.bar.package")

    // Type of annotation
    val annotTpe = rootMirror.staticClass("foo.annot").tpe

    // Check annotation is on package object
    assert(poSym.annotations match {
      case List(Annotation(`annotTpe`, _, _)) => true
      case _ => false
    })

    // Check annotation is on package
    assert(packageSym.annotations match {
      case List(Annotation(`annotTpe`, _, _)) => true
      case _ => false
    })
  }

}
