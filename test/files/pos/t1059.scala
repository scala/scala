package com;

import scala.xml._

object Main {

    def main(args : Array[String]) : Unit = {

        var m : PartialFunction[Any, Any] = {

            case SafeNodeSeq(s @ _*) => println(s) }

        println(m(<a/> ++ <b/>))
        println(m.isDefinedAt(<a/> ++ <b/>))

    }

}

object SafeNodeSeq {

    def unapplySeq(any: Any) : Option[Seq[Node]] = any match { case s: Seq[_] => Some(s flatMap ( _ match {

        case n: Node => n case _ => NodeSeq.Empty

    })) case _ => None }

} 
