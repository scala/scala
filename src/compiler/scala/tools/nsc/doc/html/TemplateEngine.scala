package scala.tools.nsc.doc.html

import scala.xml._
import scala.util.matching.Regex

class TemplateEngine(element: Elem) {
  val Pattern = new Regex("\\{\\{([A-Za-z]+)\\}\\}")

  private def renderMetaDataValue(value: Seq[Node], pairs: Map[String, Any]) = {
    def f(m: Regex.Match) =
      if (m.group(1) == null) {
        ""
      } else {
        pairs.getOrElse(m.group(1), "").toString
      }

    value.map {
      n => Text(Pattern.replaceAllIn(n.mkString, f _))
    }
  }

  private def renderMetaData(list: MetaData, pairs: Map[String, Any]): MetaData = {
    list match {
      case Null => list
      case unprefixed: UnprefixedAttribute =>
        new UnprefixedAttribute(unprefixed.key,
                                renderMetaDataValue(unprefixed.value, pairs),
                                renderMetaData(unprefixed.next, pairs))
      case prefixed: PrefixedAttribute =>
        new PrefixedAttribute(prefixed.pre, prefixed.key,
                              renderMetaDataValue(prefixed.value, pairs),
                              renderMetaData(prefixed.next, pairs))
    }
  }

  private def renderNode(node: Node, pairs: Map[String, Any]): Node =
    node match {
      case v @ <val/> => {
        v.attributes.get("name") match {
          case Some(s) => pairs.get(s.mkString) match {
            case Some(n: Node) => n
            case any => Text(any.mkString)
          }
          case None => Text("")
        }
      }
      case e: Elem => {
        val child = e.child.map(c => renderNode(c, pairs))
        Elem(e.prefix, e.label,
             renderMetaData(e.attributes, pairs),
             e.scope, child: _*)
      }
      case any => any
    }

  def render(pairs: Map[String, Any]) = renderNode(element, pairs)
  def render(pairs: (String, Any)*): Node =
    render(Map[String, Any](pairs : _*))
}
