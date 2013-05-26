// package foo

import scala.xml._
object Test {
  // guard caused verifyerror in oldpatmat
  def updateNodes(ns: Seq[Node]): Seq[Node] =
    for(subnode <- ns) yield subnode match {
      case <d>{_}</d> if true => <d>abc</d>
      case Elem(prefix, label, attribs, scope, children @ _*) =>
        Elem(prefix, label, attribs, scope, minimizeEmpty = true, updateNodes(children) : _*)
      case other => other
    }
  def main(args: Array[String]): Unit = {
    updateNodes(<b />)
  }
}

