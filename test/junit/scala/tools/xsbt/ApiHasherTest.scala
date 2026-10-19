package scala.tools.xsbt

import org.junit.Assert.assertEquals
import org.junit.Test

import java.nio.file.{ Files, Paths }
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.xsbt.visitor.{ ApiHashSink, ApiHasher }
import xsbti.VirtualFile
import xsbti.api.{ ClassLike, DefinitionType }

/** The bridge's `ApiHasher` computes what Zinc's `HashAPI` and `NameHashing` compute from the trees. */
class ApiHasherTest extends BridgeTesting {
  import ApiHasherTest._

  private var optimizedSealed = false
  private var extraOptions: List[String] = Nil

  override def mkCallback: TestCallback = new HashingCallback(optimizedSealed)
  override def compilerOptions: List[String] = super.compilerOptions ++ extraOptions

  private def check(srcs: String*): Int = withTemporaryDirectory { tmpDir =>
    val reporter = mkReporter
    val (files, callback0) =
      try compileSrcs(tmpDir, reporter, srcs: _*)
      catch {
        case e: xsbti.CompileFailed =>
          reporter.messages.filter(_.severity == xsbti.Severity.Error).take(20).foreach(println)
          throw e
      }
    compare(files, callback0.asInstanceOf[HashingCallback])
  }

  private def compare(files: Seq[VirtualFile], callback: HashingCallback): Int = {
    val mismatches = mutable.ListBuffer[String]()
    var n = 0
    for (file <- files) {
      val trees = callback.apis(file).map(c => key(c.name, c.definitionType) -> c).toMap
      val hashes = callback.hashes.getOrElse(file, Nil).map(h => key(h.name, h.definitionType) -> h).toMap
      if (trees.keySet != hashes.keySet) mismatches += s"$file: classes ${trees.keySet} vs ${hashes.keySet}"
      for ((k, tree) <- trees; h <- hashes.get(k)) {
        n += 1
        val expected = reference(tree, optimizedSealed)
        if (expected.apiHash != h.apiHash) mismatches += s"$k: apiHash"
        if (expected.extraHash != h.extraHash) mismatches += s"$k: extraHash"
        val actualNames = h.nameHashes.map(nh => (nh.name, nh.scope) -> nh.hash).toMap
        for (((name, scope), hash) <- expected.nameHashes if !actualNames.get((name, scope)).contains(hash))
          mismatches += s"$k: name hash $name ($scope)"
        for (nameScope <- actualNames.keySet -- expected.nameHashes.keySet)
          mismatches += s"$k: unexpected name hash $nameScope"
      }
    }
    assertEquals(s"$n classes compared", "", mismatches.take(50).mkString("\n"))
    n
  }

  @Test def members(): Unit = check(
    """package p
      |class C[T <: AnyRef](val x: Int, var y: String)(implicit ev: Ordering[T]) {
      |  def f[U >: T](u: U, rest: Int*)(g: => Unit): List[U] = Nil
      |  protected[p] def g: Int = 1
      |  private def h = 2
      |  private[this] val pt = 3
      |  type A = Map[String, T]
      |  type B <: Seq[T]
      |  final val K = 42
      |  lazy val l = "l"
      |  implicit def conv(i: Int): T = ???
      |  class Inner
      |  object InnerObj
      |  @deprecated("x", "y") def dep = 1
      |}
      |object C { def apply(): C[String] = ??? }
      |""".stripMargin
  )

  @Test def traits(): Unit = check(
    """trait T {
      |  private val v = 1
      |  private var w = 2
      |  private object O
      |  private[this] def d = 3
      |  def abs: Int
      |}
      |trait U extends T { override def toString = super.toString }
      |abstract class A extends U
      |""".stripMargin
  )

  @Test def refinements(): Unit = check(
    """object R {
      |  type S = { def x: Int; type T <: AnyRef }
      |  def a: S = ???
      |  def b: S = ???
      |  def c: S with Serializable = ???
      |  def d: { def y: { def z: Int } } = ???
      |  def e[F[_]]: { type L[X] = F[X] } = ???
      |  val f: AnyRef { val g: Int } = ???
      |}
      |""".stripMargin
  )

  @Test def existentials(): Unit = check(
    """class E {
      |  def a: List[_] = Nil
      |  def b: Map[_ <: AnyRef, _ >: Null] = ???
      |  def c: Class[_ <: List[_]] = ???
      |  def d(x: java.util.List[_ <: Number]): Unit = ()
      |  def e: T forSome { type T <: { def x: T } } = ???
      |}
      |""".stripMargin
  )

  @Test def sealedAndCase(): Unit = {
    val src =
      """sealed trait S
        |final case class A(i: Int) extends S
        |case object B extends S
        |sealed abstract class X { def s: S }
        |object X { case class Y(s: S) extends X }
        |""".stripMargin
    check(src)
    optimizedSealed = true
    check(src)
  }

  @Test def selfTypesAndValueClasses(): Unit = check(
    """trait Dep
      |trait Self { self: Dep with Serializable => def f = 1 }
      |trait Self2 { self => }
      |class V(val underlying: String) extends AnyVal { def length = underlying.length }
      |object Main { def main(args: Array[String]): Unit = () }
      |""".stripMargin
  )

  @Test def annotations(): Unit = check(
    """import scala.annotation._
      |class ann(x: Int) extends StaticAnnotation
      |@ann(1) class A @ann(2) () {
      |  @ann(3) val v: Int @ann(4) = 1
      |  def f(@ann(5) x: Int): Int @ann(6) = x
      |  def g(@deprecatedName("old") y: Int) = y
      |}
      |""".stripMargin
  )

  @Test def packageObjectsAndNesting(): Unit = check(
    """package p
      |package object po { type T = Int; implicit val i: Int = 1 }
      |class O { class I { class J { def j = 1 } }; object M { trait N } }
      |""".stripMargin
  )

  @Test def higherKindsAndPolymorphicTypes(): Unit = check(
    """trait Functor[F[_]] { def map[A, B](fa: F[A])(f: A => B): F[B] }
      |trait HK[M[X <: AnyRef] <: Iterable[X]] { def m: M[String] }
      |class Wrap[+A, -B] { def f[C >: A]: B => C = ??? }
      |object Lambda { type K[A] = ({ type L[B] = Either[A, B] })#L[Int] }
      |""".stripMargin
  )

  /**
   * Hashes a whole source tree: `-Dapi.hasher.corpus=dir1:dir2`, with compiler options in
   * `-Dapi.hasher.options`. Skipped otherwise.
   */
  @Test def corpus(): Unit = sys.props.get("api.hasher.corpus").foreach { dirs =>
    extraOptions = sys.props.get("api.hasher.options").toList.flatMap(_.split(' ').filter(_.nonEmpty))
    val srcs = dirs.split(java.io.File.pathSeparatorChar).toList.flatMap { d =>
      Files.walk(Paths.get(d)).iterator.asScala.filter(_.toString.endsWith(".scala")).toList.sortBy(_.toString)
    }
    val n = check(srcs.map(p => new String(Files.readAllBytes(p), "UTF-8")): _*)
    println(s"api.hasher.corpus: ${srcs.size} sources, $n classes, all equal")
  }
}

object ApiHasherTest {
  final case class Expected(apiHash: Int, extraHash: Int, nameHashes: Map[(String, xsbti.UseScope), Int])

  def key(name: String, dt: DefinitionType): String =
    name + (if (dt == DefinitionType.Module || dt == DefinitionType.PackageModule) "$" else "")

  /** What Zinc's `AnalysisCallback.api` computes. */
  def reference(c: ClassLike, optimizedSealed: Boolean): Expected = {
    import xsbt.api.{ HashAPI, NameHashing }
    val apiHash = HashAPI(c)
    val extraHash =
      if (c.definitionType == DefinitionType.Trait) HashAPI(_.hashAPI(c), includePrivateDefsInTrait = true)
      else apiHash
    val nameHashes = new NameHashing(optimizedSealed).nameHashes(c).map(nh => (nh.name, nh.scope) -> nh.hash).toMap
    Expected(apiHash, extraHash, nameHashes)
  }

  final class HashingCallback(val optimizedSealed: Boolean) extends TestCallback with ApiHashSink {
    val hashes = mutable.Map[VirtualFile, List[ApiHasher.ClassHashes]]()
    def wantsTrees = true
    def apiHashes(source: VirtualFile, h: ApiHasher.ClassHashes): Unit =
      hashes(source) = h :: hashes.getOrElse(source, Nil)
  }
}
