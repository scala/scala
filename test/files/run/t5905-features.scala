
import tools.partest.DirectTest

// verify that all languageFeature names are accepted by -language
object Test extends DirectTest {
  override def code = "class Code { def f = (1 to 10) size }" // exercise a feature to sanity-check coverage of -language options

  override def extraSettings = s"-usejavacp -d ${testOutput.path}"

  override def show() = {
    val global = newCompiler("-Ystop-after:typer")
    compileString(global)("")   // warm me up, scotty
    import global._
    exitingTyper {
      //def isFeature(s: Symbol) = s.annotations.exists((a: AnnotationInfo) => a.tpe <:< typeOf[scala.annotation.meta.languageFeature])
      def isFeature(s: Symbol) = s hasAnnotation definitions.LanguageFeatureAnnot
      val langf  = definitions.languageFeatureModule.typeSignature
      val feats  = langf.declarations filter (s => isFeature(s)) map (_.name.decoded)
      val xmen   = langf.member(TermName("experimental")).typeSignature.declarations filter (s => isFeature(s)) map (s => s"experimental.${s.name.decoded}")
      val all    = (feats ++ xmen) mkString ","

      assert(feats.nonEmpty, "Test must find feature flags.")

      //compile("junk")   // tragically, does not fail the test, i.e., arg must not be totally borked

      //dynamics,postfixOps,reflectiveCalls,implicitConversions,higherKinds,existentials,experimental.macros
      compile(s"-language:$all")
    }
  }
}

