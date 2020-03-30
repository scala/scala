/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

/** A sample transform.
 */
abstract class SampleTransform extends Transform {
  // inherits abstract value `global` and class `Phase` from Transform

  import global.{treeCopy => _, _} // the global environment
  import typer.typed    // method to type trees

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
        case Block(defs, sup @ Super(qual, mix)) => // A hypothetical transformation, which replaces
                                                    // {super} by {super.sample}
          treeCopy.Block(                           // `copy` is the usual lazy tree copier
            tree1, defs,
            typed(                              // `typed` assigns types to its tree argument
              atPos(tree1.pos)(                 // `atPos` fills in position of its tree argument
                Select(                         // The `Select` factory method is defined in class `Trees`
                  sup,
                  currentOwner.newValue(        // creates a new term symbol owned by `currentOwner`
                    newTermName("sample"),      // The standard term name creator
                    tree1.pos)))))
        case _ =>
          tree1
      }
    }
  }
}
