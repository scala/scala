package scala.tools.nsc.doc;

import scala.xml._;
import scala.collection.immutable._;
import java.io._;

object DocUtil {

  def dquote(str : String) : NodeSeq = {
    DQUOTE :: Text(str) :: DQUOTE :: Nil;

  }
  object DQUOTE extends SpecialNode {

    def toString(sb:StringBuffer) = {
      sb.append("\""); sb;
    }
    def label = "#PCDATA";

  }
  def br(nodes: NodeSeq) : NodeSeq = {
    val x = <BR/>;
    nodes.concat(x);
  }





  mixin abstract class UrlContext {

    def relative : String;
    def aref(href0: String, target : String, text : String) : NodeSeq = {
      val href = Utility.escape(href0);
      if (target.indexOf('<') != -1) throw new Error(target);

      val t0 = Text(text);

      <a href={(relative + href)} target={(target)}>{t0}</a>;
    }
    val header = <meta http-equiv="content-type" content="text/html; charset=iso-8859-1"/>
      <meta name="generator" content="scaladoc (1.4.0.4)"/>
      <link rel="stylesheet" type="text/css" href={ relative + "style.css" }/>
      <script type="text/javascript" src={relative + "script.js"}></script>;

    def body0(hasBody : Boolean, nodes : NodeSeq) : NodeSeq =
      if (!hasBody) nodes; else <BODY>{nodes}</BODY>;

    val dtype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">";

    def page(title : String, body : NodeSeq, hasBody : Boolean) : NodeSeq =
       <HTML>
          <HEAD><TITLE>{Text(title)}</TITLE>
          {header}
          </HEAD>
          {body0(hasBody, body)}
      </HTML>;

  }




    def div0(title : String) : NodeSeq = <DIV class="doctitle-larger">{Text(title)}</DIV>;


    def merge[T](ts0 : TreeSet[T], ts1 : TreeSet[T]): TreeSet[T] = {
      var ts = ts0;
      for (val t <- ts1.toList) ts = ts + t;
      ts;
    }
    def merge[T,S <: Ordered[S]](ts0 : ListMap[T,TreeSet[S]], ts1 : ListMap[T,TreeSet[S]]):
      ListMap[T,TreeSet[S]] = {
      var ts = ts0;
      for (val t <- ts1.elements) {
        if (!ts.contains(t._1))
          ts = ts.update(t._1, new TreeSet[S]);
        ts = ts.update(t._1, merge(ts(t._1), t._2));
      }
      ts;
    }
}
