/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import scala.xml.NodeSeq
import model.LinkTo
import model.comment._

trait HtmlPageBase {
  def linkToHtml(text: Inline, link: LinkTo, hasLinks: Boolean): NodeSeq

  /** Transforms an optional comment into an styled HTML tree representing its body if it is defined, or into an empty
    * node sequence if it is not. */
  def commentToHtml(comment: Option[Comment]): NodeSeq =
    comment map commentToHtml getOrElse NodeSeq.Empty

  /** Transforms a comment into an styled HTML tree representing its body. */
  def commentToHtml(comment: Comment): NodeSeq =
    bodyToHtml(comment.body)

  def bodyToHtml(body: Body): NodeSeq =
    body.blocks flatMap blockToHtml

  def blockToHtml(block: Block): NodeSeq = block match {
    case Title(in, 1) => <h3>{ inlineToHtml(in) }</h3>
    case Title(in, 2) => <h4>{ inlineToHtml(in) }</h4>
    case Title(in, 3) => <h5>{ inlineToHtml(in) }</h5>
    case Title(in, _) => <h6>{ inlineToHtml(in) }</h6>
    case Paragraph(in) => <p>{ inlineToHtml(in) }</p>
    case Code(data) =>
      <pre>{ SyntaxHigh(data) }</pre> //<pre>{ scala.xml.Text(data) }</pre>
    case UnorderedList(items) =>
      <ul>{ listItemsToHtml(items) }</ul>
    case OrderedList(items, listStyle) =>
      <ol class={ listStyle }>{ listItemsToHtml(items) }</ol>
    case DefinitionList(items) =>
      <dl>{items map { case (t, d) => <dt>{ inlineToHtml(t) }</dt><dd>{ blockToHtml(d) }</dd> } }</dl>
    case HorizontalRule() =>
      <hr/>
  }

  def listItemsToHtml(items: Seq[Block]) =
    items.foldLeft(NodeSeq.Empty){ (xmlList, item) =>
      item match {
        case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
          xmlList.init ++ <li>{ xmlList.last.child ++ blockToHtml(item) }</li>
        case Paragraph(inline) =>
          xmlList :+ <li>{ inlineToHtml(inline) }</li>  // LIs are blocks, no need to use Ps
        case block =>
          xmlList :+ <li>{ blockToHtml(block) }</li>
      }
  }

  def inlineToHtml(inl: Inline): NodeSeq = inl match {
    case Chain(items) => items flatMap (inlineToHtml(_))
    case Italic(in) => <i>{ inlineToHtml(in) }</i>
    case Bold(in) => <b>{ inlineToHtml(in) }</b>
    case Underline(in) => <u>{ inlineToHtml(in) }</u>
    case Superscript(in) => <sup>{ inlineToHtml(in) }</sup>
    case Subscript(in) => <sub>{ inlineToHtml(in) }</sub>
    case Link(raw, title) => <a href={ raw } target="_blank">{ inlineToHtml(title) }</a>
    case Monospace(in) => <code>{ inlineToHtml(in) }</code>
    case Text(text) => scala.xml.Text(text)
    case Summary(in) => inlineToHtml(in)
    case HtmlTag(tag) => scala.xml.Unparsed(tag)
    case EntityLink(target, link) => linkToHtml(target, link, true)
  }

  def tagsToHtml(comment: Comment): NodeSeq = {
    val example: NodeSeq =
      if(!comment.example.isEmpty)
          <div class="block">Example{ if (comment.example.length > 1) "s" else ""}:
              <ol>{
              val exampleXml: List[NodeSeq] =
                for(example <- comment.example ) yield
                  <li class="cmt">{ bodyToHtml(example) }</li>
              exampleXml.reduceLeft(_ ++ scala.xml.Text(", ") ++ _)
            }</ol>
            </div>
        else NodeSeq.Empty

    val version: NodeSeq =
      if(!comment.version.isEmpty) {
        <dt>Version</dt>
        <dd>{ for(body <- comment.version.toList) yield {bodyToHtml(body)} }</dd>
      } else NodeSeq.Empty

    val sinceVersion: NodeSeq =
      if(!comment.since.isEmpty) {
        <dt>Since</dt>
        <dd>{ for(body <- comment.since.toList) yield {bodyToHtml(body)} }</dd>
      } else NodeSeq.Empty

    val note: NodeSeq =
      if(!comment.note.isEmpty) {
        <dt>Note</dt>
        <dd>{
          val noteXml: List[NodeSeq] = (for(note <- comment.note ) yield <span class="cmt">{bodyToHtml(note)}</span> )
          noteXml.reduceLeft(_ ++ scala.xml.Text(", ") ++ _)
        }</dd>
      } else NodeSeq.Empty

    val seeAlso: NodeSeq =
      if(!comment.see.isEmpty) {
        <dt>See also</dt>
        <dd>{
          val seeXml:List[NodeSeq]=(for(see <- comment.see ) yield <span class="cmt">{bodyToHtml(see)}</span> )
          seeXml.reduceLeft(_ ++ _)
        }</dd>
      } else NodeSeq.Empty

    val exceptions: NodeSeq =
      if(!comment.throws.isEmpty) {
        <dt>Exceptions thrown</dt>
        <dd>{
          val exceptionsXml: Iterable[NodeSeq] =
            for(exception <- comment.throws.toList.sortBy(_._1) ) yield
              <span class="cmt">{scala.xml.Text(exception._1) ++ bodyToHtml(exception._2)}</span>
          exceptionsXml.reduceLeft(_ ++ scala.xml.Text("") ++ _)
        }</dd>
      } else NodeSeq.Empty

    val todo: NodeSeq =
      if(!comment.todo.isEmpty) {
        <dt>To do</dt>
        <dd>{
          val todoXml: List[NodeSeq] = (for(todo <- comment.todo ) yield <span class="cmt">{bodyToHtml(todo)}</span> )
          todoXml.reduceLeft(_ ++ scala.xml.Text(", ") ++ _)
        }</dd>
      } else NodeSeq.Empty

    example ++ version ++ sinceVersion ++ exceptions ++ todo ++ note ++ seeAlso
  }
}
