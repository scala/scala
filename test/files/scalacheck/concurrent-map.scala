


import java.util.concurrent._
import scala.collection._
import scala.collection.JavaConverters._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen._



case class Wrap(i: Int) {
  override def hashCode = i * 0x9e3775cd
}


object Test extends Properties("concurrent.TrieMap") {

  /* generators */

  val sizes = choose(0, 20000)

  val threadCounts = choose(2, 16)

  val threadCountsAndSizes = for {
    p <- threadCounts
    sz <- sizes
  } yield (p, sz);


  /* helpers */

  def inParallel[T](totalThreads: Int)(body: Int => T): Seq[T] = {
    val threads = for (idx <- 0 until totalThreads) yield new Thread {
      setName("ParThread-" + idx)
      private var res: T = _
      override def run() {
        res = body(idx)
      }
      def result = {
        this.join()
        res
      }
    }

    threads foreach (_.start())
    threads map (_.result)
  }

  property("concurrent getOrElseUpdate insertions") = forAll(threadCounts, sizes) {
    (p, sz) =>
    val chm = new ConcurrentHashMap[Wrap, Int]().asScala

    val results = inParallel(p) {
      idx =>
      for (i <- 0 until sz) yield chm.getOrElseUpdate(new Wrap(i), idx)
    }

    val resultSets = for (i <- 0 until sz) yield results.map(_(i)).toSet
    val largerThanOne = resultSets.zipWithIndex.find(_._1.size != 1)
    val allThreadsAgreeOnWhoInserted = {
      largerThanOne == None
    } :| s"$p threads agree on who inserted [disagreement (differentResults, position) = $largerThanOne]"

    allThreadsAgreeOnWhoInserted
  }


}






