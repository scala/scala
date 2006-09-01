/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.util.Position

/** PatternNode factory.
 *  we inherit the globals from PatternTool.
 */

trait PatternNodeCreator requires (TransMatcher with PatternNodes) {
  import global._

  /**
   *  @param pos ...
   *  @param tpe ...
   *  @param len ...
   *  @return ...
   */
  def pSequencePat(pos: PositionType, tpe: Type, len: int) = {
    //assert (tpe != null)
    val sym = newVar(FirstPos, tpe)
    //Console.println("pncrea::sequencePat sym.pos = "+sym.pos)
    val node = new SequencePat(sym, len)
    node.pos = pos
    node.tpe = tpe
    //Console.println("pncrea::sequencePat sym.pos = "+sym.pos)
    node
  }

  /**
   *  @param pos         ...
   *  @param tpe         ...
   *  @param castedRest1 ...
   *  @param minlen      ...
   *  @return            ...
   */
  def pRightIgnoringSequencePat(pos: PositionType, tpe: Type,
                                castedRest1: Symbol, minlen: int) = {
    //assert (tpe != null)
    val sym = newVar(FirstPos, tpe)
    var castedRest = if(castedRest1 != null) castedRest1 else newVar(pos, tpe)
    val node = new RightIgnoringSequencePat(sym, castedRest, minlen)
    node.pos = pos
    node.tpe = tpe
    node
  }

  /**
   *  @param pos    ...
   *  @param tpe    ...
   *  @param seqpat ...
   *  @return       ...
   */
  def pSeqContainerPat(pos: PositionType, tpe: Type, seqpat: Tree ) = {
    //assert (tpe != null)
    val sym = newVar(NoPos, tpe)
    val node = new SeqContainerPat(sym, seqpat)
    node.pos = pos
    node.setType(tpe)
    node
  }

  /**
   *  @param pos ...
   *  @param tpe ...
   *  @return    ...
   */
  def pDefaultPat(pos: PositionType, tpe: Type) = {
    //assert (tpe != null)
    val node = new DefaultPat()
    node.pos = pos
    node.setType(tpe)
    node
  }

  def pConstrPat(pos: PositionType, tpe: Type) = {
    //assert (tpe != null)
    val node = new ConstrPat(newVar(pos, tpe))
    node.pos = pos
    node.setType(tpe)
    node
  }

  def pConstantPat(pos: PositionType, tpe: Type, value: Any /*AConstant*/) = {
    //assert (tpe != null)
    val node = new ConstantPat(value)
    node.pos = pos
    node.setType(tpe)
    node
  }

  def pVariablePat(pos: PositionType, tree: Tree) = {
    //assert (tree.tpe != null)
    val node = new VariablePat(tree)
    node.pos = pos
    node.setType(tree.tpe)
    node
  }

  /**
   *  @param pos    ...
   *  @param header ...
   *  @return       ...
   */
  def pAltPat(pos: PositionType, header: Header) = {
    val node = new AltPat(header)
    node.pos = pos
    node.setType(header.getTpe())
    node
  }

  // factories

  def pHeader(pos: PositionType, tpe: Type, selector: Tree) = {
    //assert (tpe != null)
    val node = new Header(selector, null)
    node.pos = pos
    node.setType(tpe)
    node
  }

  def pBody(pos: PositionType) = {
    val node = new Body(new Array[Array[ValDef]](0),
                        new Array[Tree](0),
                        new Array[Tree](0))
    node.pos = pos
    node
  }

  def pBody(pos: PositionType, bound: Array[ValDef], guard: Tree, body: Tree) = {
    val node = new Body(Predef.Array[Array[ValDef]](bound),
                        Predef.Array[Tree](guard),
                        Predef.Array[Tree](body))
    node.pos = pos
    node
  }

  /**
   *  @param pos  ...
   *  @param name ...
   *  @param tpe  ...
   *  @return     ...
   */
  def newVar(pos: PositionType, name: Name, tpe: Type): Symbol = {
    /** hack: pos has special meaning*/
    val sym = currentOwner.newVariable(pos, name)
    //Console.println("patnodcre::newVar sym = "+sym+ "tpe = "+tpe)
    sym.setInfo(tpe)
    //System.out.println("PatternNodeCreator::newVar creates symbol "+sym)
    //System.out.println("owner: "+sym.owner())
    sym
  }

  def newVar(pos: PositionType, tpe: Type): Symbol =
    newVar(pos, cunit.fresh.newName("temp"), tpe).setFlag(Flags.SYNTHETIC)

}

