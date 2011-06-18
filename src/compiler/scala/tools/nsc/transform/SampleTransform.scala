/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

/** A sample transform.
 */
abstract class SampleTransform extends Transform {
  // inherits abstract value `global` and class `Phase` from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees

  /** the following two members override abstract members in Transform */
  val phaseName: String = "sample-phase"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SampleTransformer(unit)

  class SampleTransformer(unit: CompilationUnit) extends Transformer {

    override def transform(tree: Tree): Tree = {
      val tree1 = super.transform(tree);      // transformers always maintain `currentOwner`.
      tree1 match {
        case Block(List(), expr) =>           // a simple optimization
          expr
        case Block(defs, sup @ Super(qual, mix)) => // A hypthothetic transformation, which replaces
                                                    // {super} by {super.sample}
          treeCopy.Block(                           // `copy` is the usual lazy tree copier
            tree1, defs,
            typed(                              // `typed` assigns types to its tree argument
              atPos(tree1.pos)(                 // `atPos` fills in position of its tree argument
                Select(                         // The `Select` factory method is defined in class `Trees`
                  sup,
                  currentOwner.newValue(        // creates a new term symbol owned by `currentowner`
                    tree1.pos,
                    newTermName("sample"))))))  // The standard term name creator
        case _ =>
          tree1
      }
    }
  }
}
