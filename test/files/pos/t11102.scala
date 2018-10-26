object Test {
  def f(x: ImmutableSeq) = x match {
    case ImmutableCons(x, xs @ _*)  => xs
  }
  def f(x: MutableSeq) = x match {
    case MutableCons(x, xs @ _*)    => xs
  }
  def f(x: CollectionSeq) = x match {
    case CollectionCons(x, xs @ _*) => xs
  }
  def f(x: ScalaSeq) = x match {
    case ScalaCons(x, xs @ _*)      => xs
  }
  def f(x: DefaultSeq) = x match {
    case DefaultCons(x, xs @ _*)    => xs
  }
}

/** 
 * collection.immutable.Seq
 */
abstract class ImmutableSeq
extends collection.immutable.Seq[Int]
 with UnimplementedSeq

object ImmutableCons {
  def unapplySeq(x: ImmutableCons) =
    Some((x.first, x.more))
}

abstract class ImmutableCons
extends ImmutableSeq {
  def first: Int
  def more: collection.immutable.Seq[ImmutableCons]
}

/** 
 * collection.mutable.Seq
 */
abstract class MutableSeq
extends collection.mutable.Seq[Int]
with UnimplementedSeq

object MutableCons {
  def unapplySeq(x: MutableCons) =
    Some((x.first, x.more.toSeq))                           // !
}

abstract class MutableCons
extends MutableSeq {
  def first: Int
  def more: collection.mutable.Seq[MutableCons]
}

/**
 * collection.Seq
 */
abstract class CollectionSeq
extends collection.Seq[Int]
with UnimplementedSeq

object CollectionCons {
  def unapplySeq(x: CollectionCons) =
    Some((x.first, x.more.toSeq))                           // !
}

abstract class CollectionCons
extends CollectionSeq {
  def first: Int
  def more: collection.Seq[CollectionCons]
}

/**
 * scala.Seq
 */
abstract class ScalaSeq
extends collection.Seq[Int]
with UnimplementedSeq

object ScalaCons {
  def unapplySeq(x: ScalaCons) =
    Some((x.first, x.more))
}

abstract class ScalaCons
extends ScalaSeq {
  def first: Int
  def more: scala.Seq[ScalaCons]
}

/**
 * Seq
 */
abstract class DefaultSeq
extends Seq[Int]
with UnimplementedSeq

object DefaultCons {
  def unapplySeq(x: DefaultCons) =
    Some((x.first, x.more))
}

abstract class DefaultCons
extends DefaultSeq {
  def first: Int
  def more: Seq[DefaultCons]
}

/**
 * Unimplemented sequence.
 */
trait UnimplementedSeq {
  def iterator: Iterator[Int]           = ???
  def apply(i: Int): Int                = ???
  def length: Int                       = ???
  def update(idx: Int, elem: Int): Unit = ???
}
