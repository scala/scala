import scala.tools.partest.nest.FileManager._

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  import u._

  val JavaUniverseTpe = typeOf[reflect.runtime.JavaUniverse]
  val DefinitionsModule = JavaUniverseTpe.member(TermName("definitions"))

  def forceCode(prefix: String, tp: Type): String = {
    def isLazyAccessorOrObject(sym: Symbol) = (
          (sym.isMethod && sym.asMethod.isLazy)
       || sym.isModule
    )
    val forceables = tp.members.sorted.filter(isLazyAccessorOrObject)
    forceables.map {
      sym =>
        val path = s"$prefix.${sym.name}"
        "    " + (
          if (sym.isPrivate || sym.isProtected) s"// inaccessible: $path"
          else path
        )
    }.mkString("\n")
  }

  val code =
    s"""|// Generated Code, validated by run/t6240-universe-code-gen.scala
        |package scala.reflect
        |package runtime
        |
        |trait JavaUniverseForce { self: runtime.JavaUniverse  =>
        |  def force() {
        |    Literal(Constant(42)).duplicate
        |    nme.flattenedName()
        |    nme.raw
        |    WeakTypeTag
        |    TypeTag
        |    TypeTag.Byte.tpe
        |    TypeTag.Short.tpe
        |    TypeTag.Char.tpe
        |    TypeTag.Int.tpe
        |    TypeTag.Long.tpe
        |    TypeTag.Float.tpe
        |    TypeTag.Double.tpe
        |    TypeTag.Boolean.tpe
        |    TypeTag.Unit.tpe
        |    TypeTag.Any.tpe
        |    TypeTag.AnyVal.tpe
        |    TypeTag.AnyRef.tpe
        |    TypeTag.Object.tpe
        |    TypeTag.Nothing.tpe
        |    TypeTag.Null.tpe
        |
        |${forceCode("this", JavaUniverseTpe)}
        |${forceCode("definitions", DefinitionsModule.info)}
        |${forceCode("refChecks", typeOf[scala.reflect.internal.transform.RefChecks])}
        |${forceCode("uncurry", typeOf[scala.reflect.internal.transform.UnCurry])}
        |${forceCode("erasure", typeOf[scala.reflect.internal.transform.Erasure])}
        |  }
        |}""".stripMargin

  import java.io.File
  val testFile = new File(sys.props("partest.test-path"))
  val actualFile = new java.io.File(testFile.getParent + "/../../../src/reflect/scala/reflect/runtime/JavaUniverseForce.scala").getCanonicalFile
  val actual = scala.io.Source.fromFile(actualFile)
  val actualLines = actual.getLines.toList
  val generatedLines = code.lines.toList
  if (actualLines != generatedLines) {
    val msg = s"""|${actualFile} must be updated.
                  |===========================================================
                  | DIFF:
                  |===========================================================
                  |${compareContents(actualLines, generatedLines)}
                  |===========================================================
                  | NEW CONTENTS:
                  |===========================================================
                  |${code}""".stripMargin

    assert(false, msg)
  }
}
