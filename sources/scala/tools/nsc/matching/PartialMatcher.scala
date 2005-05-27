/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$
package scala.tools.nsc.transmatch;

/** container. classes AlgebraicMatcher and SequenceMatcher get input and
 *  store their results in here. resembles the 'Memento' design pattern,
 *  could also be named 'Liaison'
 */
class PartialMatcher(owner1: Symbol, root1: Tree, resultType1: Type) {

  /** owner of the code we create (input)
   */
  var owner: Symbol = owner1;

  /** the selector value (input)
   */
  var selector:Tree  = root1;

  /** type of the result of the whole match (input)
   */
  var resultType:Type  = resultType1 ;

  /** tree representing the matcher (output)
   */
  var tree: Tree  = _ ;

  var pos: int = root1.pos;

  //assert( owner != null ) : "owner is null";
  //assert owner != Symbol.NONE ;
  //this.owner      = owner;

  //assert root != null;
  //assert root.type != null;
  //this.selector   = root;

  //assert this.resultType != Type.NoType;
  //this.resultType = resultType;

  //this.pos        = root.pos; // for convenience only

}


