package scala.tools.xsbt

import org.junit.Test

import java.nio.file.{ Files, Paths }
import scala.jdk.CollectionConverters._
import scala.tools.xsbt.visitor.{ ApiHashSink, ApiHasher }
import xsbti.{ VirtualFile, VirtualFileRef }
import xsbti.api.ClassLike

/**
 * Allocation and CPU time of the `xsbt-api` phase, per way of producing the hashes:
 * `-Dapi.bench.corpus=dir -Dapi.bench.iterations=n`. Skipped otherwise.
 *
 * The phase also extracts used names, which is the same in every mode.
 */
class ApiAllocationBench extends BridgeTesting {
  import ApiAllocationBench._

  private var mode: String = ""
  private var profile: java.nio.file.Path = null

  override def mkCallback: TestCallback = mode match {
    case "trees"  => new TestCallback
    case "zinc"   => new ZincCallback
    case "hasher" => new HasherCallback
  }
  override def compilerOptions: List[String] =
    super.compilerOptions ++ List("-Ystop-before:jvm", "-Yprofile-destination", profile.toString)

  @Test def bench(): Unit = sys.props.get("api.bench.corpus").foreach { dir =>
    val iterations = sys.props.getOrElse("api.bench.iterations", "6").toInt
    val srcs = Files.walk(Paths.get(dir)).iterator.asScala.filter(_.toString.endsWith(".scala")).toList.sortBy(_.toString)
    val contents = srcs.map(p => new String(Files.readAllBytes(p), "UTF-8"))
    val modes = sys.props.get("api.bench.modes").fold(List("trees", "zinc", "hasher"))(_.split(',').toList)
    val results = modes.map(_ -> List.newBuilder[(Long, Long)]).toMap
    for (i <- 1 to iterations; m <- modes) {
      mode = m
      profile = Files.createTempFile("api-bench", ".csv")
      withTemporaryDirectory(dir => compileSrcs(dir, contents: _*))
      val row = Files.readAllLines(profile).asScala.map(_.split(',').map(_.trim)).find(r => r(0) == "main" && r(5) == "xsbt-api").get
      Files.delete(profile)
      if (i > iterations / 2) results(m) += ((row(14).toLong, row(12).toLong))
    }
    println(s"api.bench: ${srcs.size} sources, median of the last ${iterations - iterations / 2} runs")
    for (m <- modes) {
      val rs = results(m).result()
      val alloc = median(rs.map(_._1)) / (1024 * 1024)
      val cpu = median(rs.map(_._2)) / 1000000
      println(f"api.bench: $m%-7s xsbt-api allocated $alloc%6d MB, cpu $cpu%6d ms")
    }
  }
}

object ApiAllocationBench {
  def median(xs: List[Long]): Long = xs.sorted.apply(xs.size / 2)

  /** What Zinc's `AnalysisCallback.api` does with each class's API: hash it and keep a minimized copy. */
  final class ZincCallback extends TestCallback {
    override def api(source: VirtualFileRef, api: ClassLike): Unit = {
      super.api(source, api)
      import xsbt.api.{ APIUtil, HashAPI, NameHashing }
      val apiHash = HashAPI(api)
      val extraHash =
        if (api.definitionType == xsbti.api.DefinitionType.Trait) HashAPI(_.hashAPI(api), includePrivateDefsInTrait = true)
        else apiHash
      val nameHashes = new NameHashing(false).nameHashes(api)
      sink ^= apiHash ^ extraHash ^ nameHashes.length ^ System.identityHashCode(APIUtil.minimize(api))
    }
  }

  final class HasherCallback extends TestCallback with ApiHashSink {
    def wantsTrees = false
    def optimizedSealed = false
    def apiHashes(source: VirtualFile, h: ApiHasher.ClassHashes): Unit =
      sink ^= h.apiHash ^ h.extraHash ^ h.nameHashes.length
  }

  @volatile var sink = 0
}
