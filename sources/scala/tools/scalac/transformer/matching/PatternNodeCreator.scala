import scala.tools.util.Position;

import scalac._;
import scalac.ast._;
import scalac.atree.AConstant;
import scalac.util._;
import scalac.symtab._;
//import PatternNode._;
import Tree._;

import java.util.Vector ;

package scala.tools.scalac.transformer.matching {

  /** PatternNode factory.
   *  we inherit the globals from PatternTool.
   */

  class PatternNodeCreator(unit:CompilationUnit,  val owner:Symbol)
  extends PatternTool(unit) {

    /** the owner of the variable symbols that might be created */
    //Symbol owner;

    def SequencePat(pos: Int , tpe:Type ,  len:int) = {
      val sym = newVar(Position.FIRSTPOS, tpe);
      val node = new SequencePat(sym, len);
      node.pos = pos;
      node.setType(tpe);
      node;
    }

    def SeqContainerPat(pos: int, tpe: Type, seqpat:Tree ) = {
      val sym = newVar(Position.NOPOS, tpe);
      val node = new SeqContainerPat(sym, seqpat);
      node.pos = pos;
      node.setType(tpe);
      node;
    }

    def DefaultPat(pos: int, tpe: Type) = {
      val node = new DefaultPat();
      node.pos = pos;
      node.setType(tpe);
      node;
    }

    def ConstrPat(pos: int, tpe: Type) = {
      val node = new ConstrPat(newVar(pos, tpe));
      node.pos = pos;
      node.setType(tpe);
      node;
    }

    def ConstantPat(pos: int, tpe: Type, value:AConstant ) = {
      val node = new ConstantPat( value );
      node.pos = pos;
      node.setType(tpe);
      node;
    }

    def VariablePat(pos: int,  tree:Tree) = {
      val node = new VariablePat( tree );
      node.pos = pos;
      node.setType(tree.getType());
      node;
    }

    def AltPat(pos: int, header:Header ) = {
      val node = new AltPat(header);
      node.pos = pos;
      node.setType(header.getTpe());
      node;
    }

    // factories

    def Header(pos: int, tpe: Type,  selector:Tree) = {
      val node = new Header(selector, null);
      node.pos = pos;
      node.setType(tpe);
      node;
    }

    def Body(pos: int) = {
      val node = new Body(new Array[Array[ValDef]](0), new Array[Tree](0), new Array[Tree](0));
      node.pos = pos;
      node;
    }

    def Body(pos: int, bound:Array[ValDef] ,  guard:Tree,  body:Tree) = {
      val node = new Body(Predef.Array[Array[ValDef]](bound), Predef.Array[Tree](guard), Predef.Array[Tree](body));
      node.pos = pos;
      node;
    }

    def newVar(pos: int, name: Name, tpe: Type): Symbol= {
      val sym = owner.newVariable(pos, 0, name);
      sym.setType(tpe);
      //System.out.println("PatternNodeCreator::newVar creates symbol "+sym);
      //System.out.println("owner: "+sym.owner());
      sym;
    }

    def newVar(pos: int, tpe: Type): Symbol = {
      newVar(pos, fresh.newName("temp"), tpe);
    }
  }
}
