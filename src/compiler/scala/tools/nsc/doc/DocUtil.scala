/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.doc

import java.io.StringReader
import org.xml.sax.InputSource

import scala.collection.immutable._
import scala.xml._

object DocUtil {

  //def dquote(str: String): NodeSeq =
  //  DQUOTE :: Text(str) :: DQUOTE :: Nil

  def load(str: String): NodeSeq = {
    val xmlSrc =
      if (str.matches("^(<!--.*-->)*<[^>]+>.*<[^>]+>(<!--.*-->)*$")) str
      else "<span>" + str + "</span>"
    XML.load(new StringReader(xmlSrc))
  }

  //object DQUOTE extends SpecialNode {
   // def toString(sb: StringBuffer) = {
   //   sb.append("\""); sb
   // }
   // def label = "#PCDATA"
  //}

  def br(nodes: NodeSeq): NodeSeq = nodes ++ <br/>
  def hr(nodes: NodeSeq): NodeSeq = nodes ++ <hr/>

  trait UrlContext {
    def relative: String

    def aref(href0: String, target: String, text: String): NodeSeq = {
      val href = relative + Utility.escape(href0)
      if ((target ne null) && target.indexOf('<') != -1) throw new Error(target)

      val t0 = Text(text)
      if (target ne null)
        <a href={href} target={target}>{t0}</a>;
      else
        <a href={href}>{t0}</a>;
    }

    val encoding = Properties.encodingString
    val header =
      <meta http-equiv="content-type"
            content={"text/html; charset=" + encoding}/>
      <meta name="generator"
            content={"scaladoc (" + Properties.versionString +")"}/>
      <link rel="stylesheet" type="text/css" href={ relative + "style.css"}/>
      <script type="text/javascript" src={relative + "script.js"}></script>;

    def body0(hasBody: Boolean, nodes: NodeSeq): NodeSeq =
      if (!hasBody) nodes else <body onload="init()">{nodes}</body>;

    val dtype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

    def page(title: String, body: NodeSeq, hasBody: Boolean): NodeSeq =
      <html>
        <head><title>{Text(title)}</title>
        {header}
        </head>
        {body0(hasBody, body)}
      </html>
  } // UrlContext

  def div0(title: String): NodeSeq =
    <div class="doctitle-larger">{Text(title)}</div>;

  def merge[T](ts0: TreeSet[T], ts1: TreeSet[T]): TreeSet[T] = {
    var ts = ts0
    for (val t <- ts1.toList) ts = ts + t
    ts
  }

  def merge[T,S <: Ordered[S]](ts0: ListMap[T,TreeSet[S]],
                               ts1: ListMap[T,TreeSet[S]]): ListMap[T,TreeSet[S]] = {
    var ts = ts0
    for (val t <- ts1.elements) {
      if (!ts.contains(t._1))
        ts = ts.update(t._1, new TreeSet[S]);
      ts = ts.update(t._1, merge(ts(t._1), t._2))
    }
    ts
  }

}
