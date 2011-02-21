import scala.xml.{NodeSeq, Elem}

class EO extends App with Moo{
  def cat = <cat>dog</cat>

  implicit def nodeSeqToFlog(in: Elem): Flog = new Flog(in)
}

trait Moo {
  def cat: Flog
}

class Flog(val in: NodeSeq)
