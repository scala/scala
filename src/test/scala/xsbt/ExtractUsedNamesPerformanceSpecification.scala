package xsbt

import java.net.URI
import java.nio.file.FileSystem
import java.nio.file.FileSystemNotFoundException
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Paths

import sbt.internal.util.UnitSpec

class ExtractUsedNamesPerformanceSpecification extends UnitSpec {
  private def initFileSystem(uri: URI): Option[FileSystem] = {
    try
      Option(FileSystems.getFileSystem(uri))
    catch {
      case _: FileSystemNotFoundException =>
        val env = Map("create" -> "true")
        import scala.collection.JavaConverters._
        Option(FileSystems.newFileSystem(uri, env.asJava))
      case _: IllegalArgumentException =>
        Option(FileSystems.getDefault)
    }
  }

  val TestResource = "/ExtractUsedNamesPerformance.scala.source"
  // Some difference between 2.10, 2.11, and 2.12
  val scalaDiff = Set("Any", "Nothing", "_root_", "StringAdd", "Option")

  it should "be executed in reasonable time" in {
    var zipfs: Option[FileSystem] = None
    val src = try {
      val fileUri = getClass.getResource(TestResource).toURI
      zipfs = initFileSystem(fileUri)
      new String(Files.readAllBytes(Paths.get(fileUri)))
    } finally
      zipfs.foreach { fs => try fs.close catch { case _: Throwable => /*ignore*/ } }
    import org.scalatest.concurrent.Timeouts._
    import org.scalatest.time.SpanSugar._
    val usedNames = failAfter(10 seconds) {
      val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
      compilerForTesting.extractUsedNamesFromSrc(src)
    }
    val expectedNamesForTupler = Set("<init>", "Object", "scala", "tupler", "TuplerInstances", "DepFn1", "HNil", "$anon", "Out", "Out0", "Tupler", "hnilTupler", "acme", "L", "Aux", "HList", "Serializable", "Unit")
    val expectedNamesForTuplerInstances = Set("E", "Tuple4", "e", "case7", "Tuple15", "s", "case19", "T7", "x", "TuplerInstances", "matchEnd19", "T20", "Tuple11", "HNil", "matchEnd6", "p16", "$anon", "T19", "p20", "T2", "p10", "case22", "p19", "n", "Tuple12", "case11", "Tuple22", "p12", "matchEnd7", "N", "p4", "T13", "case26", "Tuple19", "p7", "p5", "j", "Out", "T", "p23", "case15", "matchEnd20", "t", "p21", "matchEnd15", "J", "head", "case13", "u", "matchEnd18", "U", "Tupler", "f", "T8", "T16", "F", "Tuple3", "case8", "case18", "case24", "Boolean", "matchEnd21", "A", "matchEnd26", "a", "Tuple14", "T1", "::", "Nothing", "p18", "case20", "m", "matchEnd10", "M", "matchEnd25", "tail", "Tuple2", "matchEnd5", "p15", "matchEnd23", "I", "i", "matchEnd14", "AnyRef", "Tuple8", "matchEnd8", "case25", "T12", "p3", "case14", "case23", "T5", "matchEnd22", "T17", "v", "p22", "Tuple18", "G", "Tuple13", "matchEnd12", "<init>", "V", "q", "p11", "Q", "case12", "L", "b", "apply", "Object", "g", "B", "l", "==", "Out0", "Tuple1", "matchEnd9", "P", "p2", "T15", "Aux", "matchEnd24", "p", "scala", "matchEnd11", "Tuple20", "HList", "case17", "T9", "p14", "Tuple7", "matchEnd17", "T4", "case28", "T22", "p17", "C", "Tuple6", "MatchError", "T11", "x1", "H", "case16", "matchEnd13", "c", "Tuple9", "h", "T6", "T18", "r", "K", "Tuple17", "p9", "R", "ne", "T14", "case21", "k", "case10", "Tuple21", "O", "case9", "Tuple10", "Any", "T10", "case27", "Tuple5", "D", "p13", "o", "p6", "p8", "matchEnd16", "S", "T21", "Tuple16", "d", "T3")
    val expectedNamesForRefinement = Set("Out0")
    val `expectedNamesFor::` = Set("x", "T2", "ScalaRunTime", "Iterator", "T", "head", "asInstanceOf", "Boolean", "A", "$" + "isInstanceOf", "T1", "||", "::", "Nothing", "x$1", "any2stringadd", "acme", "typedProductIterator", "tail", "Tuple2", "AnyRef", "isInstanceOf", "Int", "<init>", "_hashCode", "apply", "Object", "x$0", "==", "Some", "IndexOutOfBoundsException", "T0", "Predef", "scala", "matchEnd4", "HList", "None", "x1", "toString", "H", "+", "&&", "Serializable", "Product", "case6", "::$1", "eq", "Any", "runtime", "String")
    val expectedNamesForDepFn1 = Set("DepFn1", "Out", "T", "AnyRef", "Object", "scala")
    val expectedNamesForHNil = Set("x", "HNil", "ScalaRunTime", "Iterator", "Boolean", "A", "T", "$" + "isInstanceOf", "::", "Nothing", "x$1", "acme", "typedProductIterator", "Int", "<init>", "apply", "Object", "IndexOutOfBoundsException", "scala", "HList", "toString", "H", "Serializable", "h", "Product", "Any", "runtime", "matchEnd3", "String", "T0")
    val expectedNamesForHList = Set("Tupler", "acme", "scala", "Serializable", "Product")
    assert(usedNames("acme.Tupler") -- scalaDiff === expectedNamesForTupler -- scalaDiff)
    assert(usedNames("acme.TuplerInstances") -- scalaDiff === expectedNamesForTuplerInstances -- scalaDiff)
    assert(usedNames("acme.TuplerInstances.<refinement>") -- scalaDiff === expectedNamesForRefinement -- scalaDiff)
    assert(usedNames("acme.$colon$colon") -- scalaDiff === `expectedNamesFor::` -- scalaDiff)
    assert(usedNames("acme.DepFn1") -- scalaDiff === expectedNamesForDepFn1 -- scalaDiff)
    assert(usedNames("acme.HNil") -- scalaDiff === expectedNamesForHNil -- scalaDiff)
    assert(usedNames("acme.HList") -- scalaDiff === expectedNamesForHList -- scalaDiff)
  }

  it should "correctly find Out0 (not stored in inspected trees) both in TuplerInstances and TuplerInstances.<refinement>" in {
    val src = """|sealed trait HList extends Product with Serializable
                 |trait DepFn1[T] {
                 |  type Out
                 |  def apply(t: T): Out
                 |}
                 |trait Tupler[L <: HList] extends DepFn1[L] with Serializable
                 |trait TuplerInstances {
                 |  type Aux[L <: HList, Out0] = Tupler[L] { type Out = Out0 }
                 |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNamesForTuplerInstances = Set("Tupler", "AnyRef", "L", "Out0", "scala", "HList", "Object")
    val expectedNamesForTuplerInstancesRefinement = Set("Out0")
    assert(usedNames("TuplerInstances") -- scalaDiff === expectedNamesForTuplerInstances -- scalaDiff)
    assert(usedNames("TuplerInstances.<refinement>") -- scalaDiff === expectedNamesForTuplerInstancesRefinement -- scalaDiff)
  }

  it should "correctly collect used names from macro extension" in {
    pending
    val ext = """|package acme
                 |import scala.reflect.macros.blackbox.Context
                 |
                 |object Foo {
                 |  def foo_impl[A](c: Context)(implicit atag: c.WeakTypeTag[A]): c.Expr[List[A]] = {
                 |    import c.universe._
                 |    reify { List.empty[A] }
                 |  }
                 |}""".stripMargin
    val cod = """|package acme
                 |import scala.language.experimental.macros
                 |
                 |class Bar {
                 |  def bar[Out] = macro Foo.foo_impl[Out]
                 |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val (_, analysis) = compilerForTesting.compileSrcs(List(List(ext), List(cod)), true)
    val usedNames = analysis.usedNames.toMap

    val expectedNamesForFoo = Set("TypeApplyExtractor", "mkIdent", "package", "<repeated>", "tpe", "in", "$u", "internal", "reify", "WeakTypeTag", "Name", "empty", "collection", "ThisType", "staticModule", "staticPackage", "Singleton", "T", "asInstanceOf", "ReificationSupportApi", "U", "Expr", "Universe", "TypeApply", "A", "Tree", "Nothing", "acme", "ClassSymbol", "blackbox", "AnyRef", "Context", "mkTypeTree", "immutable", "SelectExtractor", "<init>", "$treecreator1", "apply", "Object", "macros", "moduleClass", "Foo", "T0", "Symbol", "Predef", "scala", "asModule", "Internal", "$m", "TypeCreator", "TermNameExtractor", "ModuleSymbol", "staticClass", "universe", "c", "<refinement>", "TypeTree", "List", "Select", "TermName", "Mirror", "atag", "reificationSupport", "rootMirror", "reflect", "TypeRef", "Ident", "Any", "TreeCreator", "$typecreator2", "$m$untyped", "String", "Type")
    val expectedNamesForBar = Set("experimental", "package", "WeakTypeTag", "Out", "foo_impl", "Expr", "A", "Nothing", "acme", "AnyRef", "Context", "<init>", "language", "Object", "macros", "Bar", "Foo", "scala", "List", "Any")
    assert(usedNames("acme.Foo") === expectedNamesForFoo)
    assert(usedNames("acme.Bar") === expectedNamesForBar)
  }
}