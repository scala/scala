package philips.adolf.paul

trait Sys[ S <: Sys[ S ]] {
  type Tx
}

object HASkipList {
  sealed trait NodeLike[ S <: Sys[ S ], @specialized( Int ) A ] {
    def size : Int
    def key( i: Int ): A
  }
  sealed trait Node[ S <: Sys[ S ], @specialized( Int ) A ] extends NodeLike[ S, A ] {
    def isLeaf   : Boolean
    def isBranch : Boolean
    def asBranch : Branch[ S, A ]
  }
  sealed trait BranchLike[ S <: Sys[ S ], @specialized( Int ) A ] extends NodeLike[ S, A ] {
    def down( i: Int )( implicit tx: S#Tx ) : Node[ S, A ] = sys.error("")
  }
  sealed trait HeadOrBranch[ S <: Sys[ S ], A ]
  final class Branch[ S <: Sys[ S ], @specialized( Int ) A ]()
  extends BranchLike[ S, A ] with HeadOrBranch[ S, A ] with Node[ S, A ] {
    def size:Int=1234
    def key(i: Int):A=sys.error("TODO")
    def isLeaf   : Boolean = false
    def isBranch : Boolean = true
    def asBranch : Branch[ S, A ] = this
  }
}
sealed trait HASkipList[ S <: Sys[ S ], @specialized( Int ) A ]

class HASkipListView[ S <: Sys[ S ], A ]( private val l: HASkipList[ S, A ])( implicit system: S ) {
  import HASkipList.Node
  private def buildBoxMap( n: Node[ S, A ], isRight: Boolean )( implicit tx: S#Tx ) : (Box, NodeBox) = {
    val sz = n.size
    val szm = sz - 1
    val keys = IndexedSeq.tabulate( sz ) { i =>
      val key     = n.key( i )
      (key, if( isRight && i == szm ) "M" else key.toString)
    }
    val chbo = if( n.isLeaf ) None else {
      val nb = n.asBranch
      Some( IndexedSeq.tabulate( sz )( i => buildBoxMap( nb.down( i ), isRight && (i == szm) )))
    }
    val b    = NodeBox( n, keys, chbo.map( _.map( _._2 )))
    val bb   = chbo match {
      case Some( chbt ) =>
        val chb  = chbt.map( _._1 )
        val h    = Horiz( bs = chb )
        Vert( bs = IndexedSeq[Box]( b, h ))
      case None => b
    }

    (bb, b)
  }

  private trait Box
  private case class Horiz( spacing: Int = 20, bs: IndexedSeq[ Box ]) extends Box
  private final case class Vert( spacing: Int = 20, bs: IndexedSeq[ Box ]) extends Box
  private final case class NodeBox( n: Node[ S, A ], keys: IndexedSeq[ (A, String) ], downs: Option[ IndexedSeq[ NodeBox ]]) extends Box
}
