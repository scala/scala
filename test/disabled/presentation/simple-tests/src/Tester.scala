package scala.tools.nsc
package interactive
package tests

import util._
import reporters._
import io.AbstractFile
import collection.mutable.ArrayBuffer

class Tester(ntests: Int, inputs: Array[SourceFile], settings: Settings) {

  val reporter = new StoreReporter
  val compiler = new Global(settings, reporter)

  def askAndListen[T, U](msg: String,  arg: T, op: (T, Response[U]) => Unit) {
    if (settings.verbose./*!*/value) print(msg+" "+arg+": ")
    val TIMEOUT = 10 // ms
    val limit/*?*/ = System.currentTimeMillis() + randomDelayMillis
    val res/*?*/ = new Response[U]
    op(arg, res)
    while (!res.isComplete && !res.isCancelled) {
      if (System.currentTimeMillis() > limit) {
        print("c"); res./*!*/cancel()
      } else res.get(TIMEOUT) match {
        case Some(Left(t)) =>
          /**/
          if (settings./*!*/verbose.value) println(t)
        case Some(Right(ex)) =>
          ex.printStackTrace()
          println(ex)
        case None =>
      }
    }
  }

  def askReload(sfs: SourceFile*) = askAndListen("reload", sfs.toList, compiler.askReload)
  def askTypeAt(pos: Position) = askAndListen("type at", pos, compiler.askTypeAt)
  def askTypeCompletion(pos: Position) = askAndListen("type at", pos, compiler.askTypeCompletion)
  def askScopeCompletion(pos: Position) = askAndListen("type at", pos, compiler.askScopeCompletion)

  val rand = new java.util.Random() 

  private def randomInverse(n: Int) = n / (rand.nextInt(n) + 1)

  private def randomDecreasing(n: Int) = {
    var r = rand.nextInt((1 to n).sum)
    var limit = n
    var result = 0
    while (r > limit) {
      result += 1
      r -= limit
      limit -= 1
    }
    result
  }

  def randomSourceFileIdx() = rand.nextInt(inputs.length)

  def randomBatchesPerSourceFile(): Int = randomDecreasing(100)

  def randomChangesPerBatch(): Int = randomInverse(50)

  def randomPositionIn(sf: SourceFile) = rand.nextInt(sf.content.length)

  def randomNumChars() = randomInverse(100)

  def randomDelayMillis = randomInverse(10000)

  class Change(sfidx: Int, start: Int, nchars: Int, toLeft: Boolean) {

    private var pos = start
    private var deleted: List[Char] = List()

    override def toString = 
      "In "+inputs(sfidx)+" at "+start+" take "+nchars+" to "+
      (if (toLeft) "left" else "right")

    def deleteOne() {
      val sf = inputs(sfidx)
      deleted = sf.content(pos) :: deleted
      val sf1 = new BatchSourceFile(sf.file, sf.content.take(pos) ++ sf.content.drop(pos + 1))
      inputs(sfidx) = sf1
      askReload(sf1)
    }

    def deleteAll() {
      print("/"+nchars)
      for (i <- 0 until nchars) {
        if (toLeft) {
          if (pos > 0 && pos <= inputs(sfidx).length) {
            pos -= 1
            deleteOne()
          } 
        } else {
          if (pos  < inputs(sfidx).length) {
            deleteOne()
          }
        }
      }
    }
    
    def insertAll() {
      for (chr <- if (toLeft) deleted else deleted.reverse) {
        val sf = inputs(sfidx)
        val (pre, post) = sf./*!*/content splitAt pos
        pos += 1
        val sf1 = new BatchSourceFile(sf.file, pre ++ (chr +: post))
        inputs(sfidx) = sf1
        askReload(sf1)
      }
    }
  }

  val testComment = "/**/"

  def testFileChanges(sfidx: Int) = {
    lazy val testPositions: Seq[Int] = {
      val sf = inputs(sfidx)
      val buf = new ArrayBuffer[Int]
      var pos = sf.content.indexOfSlice(testComment)
      while (pos > 0) {
        buf += pos
        pos = sf.content.indexOfSlice(testComment, pos + 1)
      }
      buf
    }
    def otherTest() {
      if (testPositions.nonEmpty) {
        val pos = new OffsetPosition(inputs(sfidx), rand.nextInt(testPositions.length))
        rand.nextInt(3) match {
          case 0 => askTypeAt(pos)
          case 1 => askTypeCompletion(pos)
          case 2 => askScopeCompletion(pos)
        }
      }
    }
    for (i <- 0 until randomBatchesPerSourceFile()) {
      val changes = Vector.fill(/**/randomChangesPerBatch()) {
        /**/
        new Change(sfidx, randomPositionIn(inputs(sfidx)), randomNumChars(), rand.nextBoolean())
      } 
      doTest(sfidx, changes, testPositions, otherTest) match {
        case Some(errortrace) =>
          println(errortrace)
          minimize(errortrace)
        case None =>
      }
    }
  }

  def doTest(sfidx: Int, changes: Seq[Change], testPositions: Seq[Int], otherTest: () => Unit): Option[ErrorTrace] = {
    print("new round with "+changes.length+" changes:")
    changes foreach (_.deleteAll())
    otherTest() 
    def errorCount() = compiler.ask(() => reporter.ERROR.count)
//    println("\nhalf test round: "+errorCount())
    changes.view.reverse foreach (_.insertAll())
    otherTest()
    println("done test round: "+errorCount())
    if (errorCount() != 0)
      Some(ErrorTrace(sfidx, changes, reporter.infos, inputs(sfidx).content))
    else 
      None
  }

  case class ErrorTrace(
    sfidx: Int, changes: Seq[Change], infos: collection.Set[reporter.Info], content: Array[Char]) {
    override def toString = 
      "Sourcefile: "+inputs(sfidx)+
      "\nChanges:\n  "+changes.mkString("\n  ")+
      "\nErrors:\n  "+infos.mkString("\n  ")+
      "\nContents:\n"+content.mkString
  }

  def minimize(etrace: ErrorTrace) {}

  /**/
  def run() {
    askReload(inputs: _*)
    for (i <- 0 until ntests)
      testFileChanges(randomSourceFileIdx())
  }
}

/* A program to do presentation compiler stress tests.
 * Usage:
 *
 *  scala scala.tools.nsc.interactive.test.Tester <n> <files>
 *
 * where <n> is the number os tests to be run and <files> is the set of files to test.
 * This will do random deletions and re-insertions in any of the files.
 * At places where an empty comment /**/ appears it will in addition randomly
 * do ask-types, type-completions, or scope-completions.
 */
object Tester {
  def main(args: Array[String]) {
    val settings = new Settings()
    val (_, filenames) = settings.processArguments(args.toList.tail, true)
    println("filenames = "+filenames)
    val files = filenames.toArray map (str => new BatchSourceFile(AbstractFile.getFile(str)): SourceFile)
    new Tester(args(0).toInt, files, settings).run()
    sys.exit(0)
  }
} 
