class CTxnLocal[ T ] {
    def set( x: T )( implicit t: Txn ) {}
    def get( implicit t: Txn ) : T = null.asInstanceOf[ T ]
    def initialValue( t: Txn ) : T = null.asInstanceOf[ T ]
}

trait Txn

trait ProcTxn {
    def ccstm: Txn
}

trait TxnLocal[ @specialized T ] {
   def apply()( implicit tx: ProcTxn ) : T
   def set( v: T )( implicit tx: ProcTxn ) : Unit
   def swap( v: T )( implicit tx: ProcTxn ) : T
   def transform( f: T => T )( implicit tx: ProcTxn ) : Unit
}

object TxnLocal {
   def apply[ @specialized T ] : TxnLocal[ T ] = new Impl( new CTxnLocal[ T ])
   def apply[ @specialized T ]( initValue: => T ) : TxnLocal[ T ] = new Impl( new CTxnLocal[ T ] {
      override def initialValue( tx: Txn ): T = initValue
   })

   private class Impl[ T ]( c: CTxnLocal[ T ]) extends TxnLocal[ T ] {
      def apply()( implicit tx: ProcTxn ) : T = c.get( tx.ccstm )
      def set( v: T )( implicit tx: ProcTxn ) : Unit = c.set( v )( tx.ccstm )
      def swap( v: T )( implicit tx: ProcTxn ) : T = {
         // currently not implemented in CTxnLocal
         val oldV = apply
         set( v )
         oldV
      }
      def transform( f: T => T )( implicit tx: ProcTxn ) {
         set( f( apply ))
      }
   }
}


object Transition {
   private val currentRef = TxnLocal[ Transition ]( Instant )
   def current( implicit tx: ProcTxn ) : Transition = currentRef()
}

sealed abstract class Transition
case object Instant extends Transition

