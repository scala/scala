/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

abstract class TypesAsValues extends Transform {

  // inherits abstract value `global' and class `Phase' from Transform

  import global._;                  // the global environment
  import definitions._;             // standard classes and methods
  import typer.typed;               // methods to type trees
  import posAssigner.atPos;         // for filling in tree positions

  protected val phaseName: String = "typesAsValues-phase";
  protected def newTransformer: Transformer = new TypesAsValuesTransformer;

  class TypesAsValuesTransformer extends Transformer {

    override def transform(tree: Tree): Tree = tree; //@todo

  }

}
