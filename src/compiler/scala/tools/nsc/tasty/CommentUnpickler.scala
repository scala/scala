package scala.tools.nsc.tasty
//
//import dotty.tools.dotc.core.Comments.Comment
//import dotty.tools.dotc.core.tasty.TastyBuffer.Addr
//import dotty.tools.dotc.util.Spans.Span
//
//import scala.collection.mutable.HashMap
//
//import java.nio.charset.Charset
//
class CommentUnpickler(reader: TastyReader) {
//  import reader._
//
//  private[tasty] lazy val comments: Map[Addr, Comment] = {
//    val comments = new HashMap[Addr, Comment]
//    while (!isAtEnd) {
//      val addr = readAddr()
//      val length = readNat()
//      if (length > 0) {
//        val bytes = readBytes(length)
//        val position = new Span(readLongInt())
//        val rawComment = new String(bytes, Charset.forName("UTF-8"))
//        comments(addr) = Comment(position, rawComment)
//      }
//    }
//    comments.toMap
//  }
//
//  def commentAt(addr: Addr): Option[Comment] =
//    comments.get(addr)
//
}
