/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id:

import scalac.{Global => scalac_Global}

import scala.collection.mutable.HashMap;
import scala.collection.mutable.HashSet;

import scalac.symtab._;
import scalac.atree._;
import scalac.util.Debug;

package scala.tools.scalac.icode {

/** This class represents the intermediate code of a method*/
class ICode(label: String, global: scalac_Global) {

  //##################################################
  // Public fields

  /* The set of all blocks */
  val blocks: HashSet[IBasicBlock] = new HashSet;

  /* The start block of the method */
  var startBlock : IBasicBlock = null;

  /* The stack produced by this method */
  var producedStack : ICTypeStack = null;

  //##################################################
  // Private fields
  private var currentLabel : int = 0;

  private var aTreeLabels : HashMap[Symbol, IBasicBlock] = new HashMap;

  //##################################################
  // Constructor code

  startBlock = newBlock;
  startBlock.initStack(new ICTypeStack);

  //##################################################
  // Public methods

  /** This method produce the intermediate code for
    * the given ATree */
  def generate(acode : ACode) = {
    val ctx = gen(acode, new GenContext(startBlock, null));

    ctx.emit(RETURN()); // ??? RETURN instruction rather come from ATree
    genTypeStack;

    producedStack = ctx.currentBlock.endStack;
  }

  /* This method applies the given function to all the block */
  def icTraverse(f: IBasicBlock => unit) = {
    // ?? Define order (actually preorder)
    val visited : HashMap[IBasicBlock, boolean] = new HashMap;
    visited.incl(blocks.elements.map((x: IBasicBlock) => Pair(x, false)));

    var blockToVisit : List[IBasicBlock] = startBlock::Nil;

    while (blockToVisit != Nil) {
      blockToVisit match {
	case b::xs => {
	  if (!visited(b)) {
	    f(b);
	    blockToVisit = b.successors:::xs;
	    visited += b -> true;
	  } else
	    blockToVisit = xs;
	}
      }
    }
  }


  /** This methods returns a string representation of the ICode */
  override def toString() : String = "ICode '"+label+"'"; //

  //##################################################
  // Public method - printing (for debug use)

  /** This method print the code */
  def print() : unit = print(System.out);

  def print(out: java.io.PrintStream) : unit = {
    icTraverse((bb: IBasicBlock) => {
      out.println("Block #"+bb.label);
      out.println("Substituable variables : ");
      if (bb.substituteVars != null)
	bb.substituteVars.foreach(out.print);
      else
	out.println(" {Empty} ");
      out.println("Instructions:");
      bb.bbTraverse((ici: ICInstruction) =>
	out.println("  "+ici.toString()));
      out.print  ("Successors: ");
      bb.successors.foreach((bb: IBasicBlock) => out.print(bb.label+", "));
      out.println (""); // ?? Del
      out.println ();
    });
  }


  def logType = {
    global.log ("// Typing "+toString());
    this.icTraverse((bb: IBasicBlock) => {
      global.log ("Typing block #"+bb.label);
      var typer = new ICTypeStack;
      bb.bbTraverse((ic: ICInstruction) => {
	typer = typer.eval(ic);
	global.log(ic.toString()+" -> "+typer.toString());
      });

    });
   }

  //##################################################
  // Private methods

  /* Compute a unique new label */
  private def nextLabel = {
    currentLabel = currentLabel + 1;
    currentLabel;
  }

  /* This method :
   * 1/ create a new block
   * 2/ add the new block to the set
   * 3/ return the new block
   */
  private def newBlock : IBasicBlock = {
    val block = new IBasicBlock(nextLabel);
    blocks += block;
    block;
  }

  /* Generate the code from a given ATree */
  private def gen(aCode: ACode, ctx: GenContext) : GenContext  =
    aCode match {
      case ACode.Void => {
	global.log ("ICodeGenerator::emit: Void node found");
	ctx;
      }

      case ACode$This(clasz) =>
	ctx.emit(THIS(clasz));

      case ACode$Constant(c) =>
	ctx.emit(CONSTANT(c));

      case ACode$Load(ALocation$Module(_)) => {
	global.log ("ICodeGenerator::emit: Load(Module) node found");
	ctx;
      }

      case ACode$Load(ALocation$Field(obj, field ,isStatic)) => {
	var ctx1 = gen(obj, ctx);
	ctx.emit(LOAD_FIELD(field, isStatic));
      }

      case ACode$Load(ALocation$Local(local,isArgument)) =>
	ctx.emit(LOAD_LOCAL(local, isArgument));

      case ACode$Load(ALocation$ArrayItem(array, index)) => {
	var ctx1 = gen(array, ctx);
	ctx1 = gen(index, ctx1);
	ctx1.emit(LOAD_ARRAY_ITEM());
      }

      case ACode$Store(ALocation$Module(_),_) => {
	global.log ("ICodeGenerator::emit: Store(Module(_)) node found");
	ctx;
      }

      case ACode$Store(ALocation$Field(obj, field, isStatic), value) => {
	var ctx1 = gen(obj, ctx);
	ctx1 = gen(value, ctx1);
	ctx1.emit(STORE_FIELD(field, isStatic));
      }

      case ACode$Store(ALocation$Local(local, isArgument), value) => {
	val ctx1 = gen(value, ctx);
	ctx1.emit(STORE_LOCAL(local, isArgument));
      }

      case ACode$Store(ALocation$ArrayItem(array, index), value) => {
	var ctx1 = gen(array, ctx);
	ctx1 = gen(index, ctx1);
	ctx1 = gen(value, ctx1);
	ctx1.emit(STORE_ARRAY_ITEM());
      }

      case ACode$Apply(AFunction$Method(_,sym,AInvokeStyle.New),_,vargs) => {
	val vargs_it = new IterableArray(vargs).elements;
	// !!! Depend the backend in use
	ctx.emit(NEW(sym.owner()));
	ctx.emit(DUP(sym.owner().getType()));
	var ctx1 = ctx;
	vargs_it.foreach((varg: ACode) => ctx1 = gen(varg, ctx1));
	ctx1.emit(CALL_METHOD(sym,AInvokeStyle.StaticInstance));
      }

      case ACode$Apply(AFunction$Method(obj,sym,style),_,vargs) => {
	val vargs_it = new IterableArray(vargs).elements;
	var ctx1 = ctx;
	style match {
	  case AInvokeStyle.StaticClass =>
	    ; // NOP
	  case _ =>
	    ctx1 = gen(obj, ctx1);
	}
	vargs_it.foreach((varg: ACode) => ctx1 = gen(varg, ctx1));
	ctx1.emit(CALL_METHOD(sym,style));
      }

      case ACode$Apply(AFunction$Primitive(p),_,vargs) => {
	val vargs_it = new IterableArray(vargs).elements;
	var ctx1 = ctx;

	vargs_it.foreach((varg: ACode) => ctx1 = gen(varg, ctx1));
	ctx1.emit(CALL_PRIMITIVE(p));
      }

      case ACode$Apply(AFunction$NewArray(element),_,vargs) => {
	var ctx1 = gen(vargs(0), ctx); // The size is given as first argument
	ctx1.emit(CREATE_ARRAY(element));
      }

      case ACode$IsAs(value,typ,false) => {
	var ctx1 = gen(value, ctx);
	ctx1.emit(IS_INSTANCE(typ));
      }

      case ACode$IsAs(value,typ,true) => {
	var ctx1 = gen(value, ctx);
	ctx1.emit(CHECK_CAST(typ));
      }

      case ACode$If(test, success, failure) => {
	genAlt(test, success, failure, ctx, newBlock);
      }

      case ACode$Switch(test,tags,bodies) => {

	val switchBodies = List.fromArray(bodies, 0, bodies.length);
	var switchBlocks: List[IBasicBlock] = Nil;
	for (val i : ACode <- switchBodies) switchBlocks = newBlock::switchBlocks;
	val switchPairs = switchBodies.zip(switchBlocks);

	val nextBlock = newBlock;

	var ctx1 = gen(test, ctx);
	ctx1.emit(SWITCH(tags, switchBlocks));
	ctx1.currentBlock.addSuccessors(switchBlocks);

	switchPairs.foreach((p: Pair[ACode, IBasicBlock]) => {
	  val code = p._1;
	  val block = p._2;

	  val ctx2 = gen(code, new GenContext(block, nextBlock));
	  ctx2.closeBlock;
	});

	ctx1.changeBlock(nextBlock);
      }

      case ACode$Synchronized(lock, value) => {
	var ctx1 = gen(lock, ctx);
	ctx1.emit(MONITOR_ENTER());
	ctx1 = gen(value,ctx);
	ctx1 = gen(lock, ctx);
	ctx1.emit(MONITOR_EXIT());
      }

      case ACode$Block(_,statements,value) => {
	val statements_it = new IterableArray(statements).elements;
	var ctx1 = ctx;
	statements_it.foreach((st: ACode) => ctx1 = gen(st, ctx1));
	ctx1 = gen(value, ctx1);
	ctx1;
      }

      case ACode$Label(label, locals, value) => {

	val loopBlock = newBlock;
	var ctx1 = ctx.changeBlock(loopBlock);
	ctx.nextBlock = loopBlock;
	ctx.closeBlock;

	aTreeLabels += label -> loopBlock;

	loopBlock.substituteVars = List.fromIterator((new IterableArray(locals)).elements);
	gen(value, ctx1);
      }

      case ACode$Goto(label,vargs) => {

	val vargs_it = new IterableArray(vargs).elements;
	global.log("Current label mapping: "+aTreeLabels.keys.foreach((s: Symbol) => global.log(s.toString())));
	global.log("Looking for sym: "+label);
	val gotoBlock = aTreeLabels(label);
        var ctx1 = ctx;

	// Stack-> :
	vargs_it.foreach((varg: ACode) => ctx1 = gen(varg, ctx1));

	// Stack-> :varg0:varg1:...:vargn
	// We should have the same number of varg that substitute vars

	gotoBlock.substituteVars.reverse.foreach(
	  (varg: Symbol) => ctx1.emit(STORE_LOCAL(varg, false)) // ?? false
	);
	ctx1.nextBlock = gotoBlock;
	ctx1.closeBlock;

	ctx1;
      }

      case ACode$Return(_,value) => {
	var ctx1 = gen(value, ctx);
	ctx1.emit(RETURN());
      }
      case ACode$Throw(value) => {
	var ctx1 = gen(value, ctx);
	ctx1.emit(THROW());
      }

      case ACode$Drop(value, typ) => {
	var ctx1 = gen(value, ctx);
	//global.log("Type de Drop: "+typ+" = unboxed:"+typ.unbox() );
	if (! typ.isSameAs(global.definitions.UNIT_TYPE()))
	  typ.unbox() match { // !!! Hack
	    case Type$UnboxedType(TypeTags.UNIT) =>
	      global.log("it matches UNIT !"); // debug ; // NOP
	    case _ =>
	      ctx1.emit(DROP(typ));
	  }
	  else
	  global.log("it matches SCALAC_UNIT :-(");
	ctx1;
      }
    }

  /* This method genrates an alternative. In the case of an if instruction
  * It looks at the kind of alternatives and creates new basic blocks
  *
  * @param nextBlock : represents the block where alternatives meet again*/
  private def genAlt (cond : ACode, success: ACode, failure: ACode, ctx: GenContext, nextBlock: IBasicBlock) : GenContext = {
    val successBlock = success match {
      case ACode.Void => nextBlock;

      case ACode$If(test, innerSuccess, innerFailure) => {
	val ctx1 = new GenContext(newBlock, null);
	val ctx2 = genAlt(test, innerSuccess, innerFailure, ctx1, nextBlock);
	//ctx2.closeBlock; // or close when CJUMP is emitted
	ctx1.currentBlock;
      }

      case _ => {
	val ctx1 = new GenContext(newBlock, nextBlock);
	val ctx2 = gen(success, ctx1);
	ctx2.closeBlock;
	ctx1.currentBlock;
      }
    }

    val failureBlock = failure match {
      case ACode.Void => nextBlock;

      case ACode$If(test, innerSuccess, innerFailure) => {
	val ctx1 = new GenContext(newBlock, null);
	val ctx2 = genAlt(test, innerSuccess, innerFailure, ctx1, nextBlock);
	//ctx2.closeBlock; // or close when CJUMP is emitted
	ctx1.currentBlock;
      }

      case _ => {
	val ctx1 = new GenContext(newBlock, nextBlock);
	val ctx2 = gen(failure, ctx1);
	ctx2.closeBlock;
	ctx1.currentBlock;
      }
    }

    var ctx1 = genCond(cond, successBlock, failureBlock, ctx);
    ctx1.changeBlock(nextBlock);
  }

  /* This methods generate the test and the jump instruction */
  private def genCond(cond: ACode, successBlock: IBasicBlock, failureBlock: IBasicBlock, ctx: GenContext) : GenContext= {
    var ctx1 = ctx;
    cond match {
      case ACode$Apply(AFunction$Primitive(APrimitive$Test(op,_,zero)),_, vargs) => {
	ctx1 = gen(vargs(0),ctx1);
	if (zero)
	  ctx1.emit(CZJUMP(successBlock, failureBlock, op));
	else {
	  ctx1 = gen(vargs(1), ctx1);
	  ctx1.emit(CJUMP(successBlock, failureBlock, op));
	}
	ctx1.currentBlock.addSuccessors(successBlock::failureBlock::Nil);
      }

      case ACode$Apply(AFunction$Primitive(APrimitive$Negation(_)),_,vargs) =>
	ctx1 = genCond(vargs(0), failureBlock, successBlock, ctx1);
      // ??? Test with TestOp opposite ?
      case _ => {
	ctx1 = gen(cond, ctx1);
	ctx1.emit(CONSTANT(AConstant.INT(1)));
	ctx1.emit(CJUMP(successBlock, failureBlock, ATestOp.EQ));
	ctx1.currentBlock.addSuccessors(successBlock::failureBlock::Nil);
      }
    }
    ctx1;
  }

  /* This method generate the type stacks of all the blocks
   * of this method.*/
  private def genTypeStack = {
    icTraverse((bb: IBasicBlock) => {
      global.log("// Typing block #"+bb.label);
      global.log("\t-> "+bb.endStack);
      bb.typeBlock;
      // !!! Here we must test if all the meeting blocks have the
      // !!! same stack.
      bb.successors.foreach((suc: IBasicBlock) => suc.initStack(bb.endStack));
    });
  }

}

  /* This class represents the context of generating icode */
  class GenContext(current: IBasicBlock, next: IBasicBlock) {

    // ##################################################
    // Public fields

    /* The block in which we add instructions */
    val currentBlock = current;

    /* The block were we jump at the end of the current block
     * It can be the *null* value */
    var nextBlock    = next;

    // ##################################################
    // Constructor code

    assert(current != null, "No current block");

    // ##################################################
    // Public methods

    /* append an instruction at the end of the current block */
    def emit(instr: ICInstruction) : GenContext = {
      currentBlock.emit(instr);
      this;
    }

    /* Split the current block. The next block is still the same */
    def changeBlock(bb: IBasicBlock) = {
      new GenContext(bb, next);
    }

    /* Close the current block. If there's a known next block
     * It append a jump instruction to the block.
     * This method do something only the first time it's called */
    def closeBlock = {
      if (!currentBlock.isClosedBlock) {
	if (nextBlock != null) {
	  emit(JUMP(nextBlock));
	  currentBlock.addSuccessor(nextBlock);
	}
	currentBlock.close;
      }
    }
  }

} // package
