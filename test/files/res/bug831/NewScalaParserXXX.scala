package bug831;

trait ScalaNodeScannerXXX {
  type Node <: NodeImpl;
  trait NodeImpl { def self : Node; }
  type Unfixed <: Node with UnfixedImpl;
  trait UnfixedImpl extends NodeImpl { def self : Unfixed; }
}
//def f = { Console.println("hello"); 42; }
//for (val ns <-n; val i <- 0.until(ns)) yield f;


trait NewScalaScannerXXX extends ScalaNodeScannerXXX { 
  type Unfixed <: Node with UnfixedImpl;
  trait UnfixedImpl extends super.UnfixedImpl with NodeImpl;
  type Statement <: Unfixed with StatementImpl;
  trait StatementImpl extends UnfixedImpl { def self : Statement; }
  type NewLine <: Statement with NewLineImpl;
  trait NewLineImpl extends StatementImpl { 
    def self : NewLine; 
    def isActive : Boolean = true;
  }
  object ArrowMode extends Enumeration { val Def, Case, Expr = Value }
}

trait ScalaPrecedenceXXX extends NewScalaScannerXXX { 
  type NewLine <: Statement with NewLineImpl;
  trait NewLineImpl extends super.NewLineImpl with StatementImpl {  
    def self : NewLine; 
    override def isActive = super[NewLineImpl].isActive;
  }
}
trait NewScalaParserXXX extends NewScalaScannerXXX with ScalaPrecedenceXXX {
  type NewLine <: Statement with NewLineImpl;
  trait MyNewLine extends super[NewScalaScannerXXX].NewLineImpl;
  trait NewLineImpl extends MyNewLine with 
    super[ScalaPrecedenceXXX].NewLineImpl with
    StatementImpl { 
    def self : NewLine; 
    override def isActive = super[MyNewLine].isActive;
  }
}

