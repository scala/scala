trait SeqLike[+Repr]
trait Seq extends SeqLike[Seq]

trait MySeq extends Seq with SeqLike[MySub]
trait MySub extends MySeq
