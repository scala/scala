import scala.tools.nsc.doc
import scala.tools.nsc.interactive._
import scala.tools.nsc.interactive.tests._
import scala.tools.nsc.util._
import scala.tools.nsc.io._

object Test extends InteractiveTest {
  override val keepDocComments = true

  val tags = Seq(
    "@example  `\"abb\".permutations = Iterator(abb, bab, bba)`",
    "@version 1.0, 09/07/2012",
    "@since 2.10",
    "@note Don't inherit!",
    "@see some other method",
    "@throws Predef.NoSuchElementException",
    "@todo this method is unsafe"
  )

  val comment = "This is a test comment."
  val caret = "<caret>"

  def text(nTags: Int) = 
    """|/** %s
       |
       | *  %s */
       |class Test
       |class User(t: %sTest)""".stripMargin.format(comment, tags take nTags mkString "\n", caret)

  override def execute() {
    val documenter = new Doc(compiler, new doc.Settings(_ => ()))
    import documenter.global
    for (i <- 1 to tags.length) {
      val markedText = text(i)
      val idx = markedText.indexOf(caret)
      val fileText = markedText.substring(0, idx) + markedText.substring(idx + caret.length)
      val source = sourceFiles(0) 
      val batch = new BatchSourceFile(source.file, fileText.toCharArray)
      val reloadResponse = new Response[Unit]
      global.askReload(List(batch), reloadResponse)
      reloadResponse.get.left.toOption match {
        case None =>
	  println("Couldn't reload")
        case Some(_) =>
          val treeResponse = new Response[global.Tree]
          val pos = global.rangePos(batch, idx, idx, idx)
          global.askTypeAt(pos, treeResponse)
          treeResponse.get.left.toOption match {
            case Some(tree) =>
              val sym = tree.tpe.typeSymbol
              documenter.retrieve(sym, sym.owner) match {
               case Some(HtmlResult(html)) => // HTML is badly formatted, don't want to insert it as testdata.
               case Some(_) => println("Got unexpected result")
               case None => println("Got no result")
	      }
            case None => println("Couldn't find a typedTree")
	  }
      }
    }
  }
}
