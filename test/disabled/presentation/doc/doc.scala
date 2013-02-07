import scala.tools.nsc.doc
import scala.tools.nsc.doc.base.LinkTo
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.interactive._
import scala.tools.nsc.interactive.tests._
import scala.tools.nsc.util._
import scala.tools.nsc.io._

object Test extends InteractiveTest {
  override val settings: doc.Settings = docSettings

  val tags = Seq(
    "@example  `\"abb\".permutations = Iterator(abb, bab, bba)`",
    "@version 1.0, 09/07/2012",
    "@since 2.10",
    "@todo this method is unsafe",
    "@note Don't inherit!",
    "@see some other method"
  )

  val comment = "This is a test comment."
  val caret = "<caret>"

  def text(nTags: Int) = 
    """|/** %s
       |
       | *  %s */
       |trait Commented {}
       |class User(c: %sCommented)""".stripMargin.format(comment, tags take nTags mkString "\n", caret)

  override def main(args: Array[String]) {
    val documenter = new Doc(settings) {
      val global: compiler.type = compiler

      def chooseLink(links: List[LinkTo]): LinkTo = links.head
    }
    for (i <- 1 to tags.length) {
      val markedText = text(i)
      val idx = markedText.indexOf(caret)
      val fileText = markedText.substring(0, idx) + markedText.substring(idx + caret.length)
      val source = sourceFiles(0) 
      val batch = new BatchSourceFile(source.file, fileText.toCharArray)
      val reloadResponse = new Response[Unit]
      compiler.askReload(List(batch), reloadResponse)
      reloadResponse.get.left.toOption match {
        case None =>
          reporter.println("Couldn't reload")
        case Some(_) =>
          val treeResponse = new compiler.Response[compiler.Tree]
          val pos = compiler.rangePos(batch, idx, idx, idx)
          compiler.askTypeAt(pos, treeResponse)
          treeResponse.get.left.toOption match {
            case Some(tree) =>
              val sym = tree.tpe.typeSymbol
              documenter.retrieve(sym, sym.owner) match {
               case Some(HtmlResult(comment)) =>
                 import comment._
                 val tags: List[(String, Iterable[Body])] =
                   List(("@example", example), ("@version", version), ("@since", since.toList), ("@todo", todo), ("@note",  note), ("@see", see))
                 val str = ("body:" + body + "\n") + 
                   tags.map{ case (name, bodies) => name + ":" + bodies.mkString("\n") }.mkString("\n")
                 reporter.println(str)
               case Some(_) => reporter.println("Got unexpected result")
               case None => reporter.println("Got no result")
              }
            case None => reporter.println("Couldn't find a typedTree")
          }
      }
    }
  }
}
