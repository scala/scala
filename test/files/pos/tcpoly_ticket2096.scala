// smallest expression of monad i can find
trait MBrace[C[X] <: MBrace[C,X],A] {
 def nest( a : A ) : C[A]
 def flatten[T <: C[C[A]]]( bsq : T ) : C[A]
}

// a monad that is a Seq
trait MBraceSeq[C[X] <: MBrace[C,X] with Seq[X],A] extends MBrace[C,A]

// one of the simplest witnesses of monad i can find
case class MSequitor[A]( a_ : A* ) extends Seq[A] with MBrace[MSequitor,A]
{
 override def nest( a : A ) = new MSequitor[A]( a )
 override def flatten[T <: MSequitor[MSequitor[A]]]( bsq : T ) : MSequitor[A] = {
   (new MSequitor[A]( ) /: bsq)( {
     ( acc : MSequitor[A], e : MSequitor[A] ) => ( acc ++ e ).asInstanceOf[MSequitor[A]]
   } )
 }
 override def length = a_.length
 override def iterator = a_.iterator
 override def apply( n : Int ) = a_.apply( n )
}

//  type arguments [MSequitor,A] do not conform to trait MBraceSeq's type parameter bounds [C[_] <: MBrace[C,A] with Seq[A],A]
// a statement of the instance relation
class MBraceSequitor[A] extends MBraceSeq[MSequitor,A] {
 val empty : MSequitor[A] = new MSequitor[A]( )
 override def nest( a : A ) = empty.nest( a )
 override def flatten[T <: MSequitor[MSequitor[A]]]( bsq : T ): MSequitor[A] = empty.flatten( bsq )
}