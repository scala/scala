package strawman
package collection
package immutable

import strawman.collection.mutable.Builder
import strawman.collection.IterableFactory

import scala.Any

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]]

trait SeqOps[+A, +CC[A] <: Seq[A], +C] extends collection.SeqOps[A, CC, C]

object Seq extends IterableFactory.Delegate[Seq](List)
