class NodeSeq
class Elem extends NodeSeq

class EO extends App with Moo {
  // return type is Flog, inherited from overridden method.
  // implicit conversions are applied because expected type `pt` is `Flog` when `computeType(rhs, pt)`.
  def cat = (??? : Elem)

  implicit def nodeSeqToFlog(in: Elem): Flog = new Flog(in)
}

trait Moo {
  def cat: Flog
}

class Flog(val in: NodeSeq)
