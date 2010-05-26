/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

/** <p>
 *    A base class for transforms.
 *  </p>
 *  <p>
 *    A transform contains a compiler phase which applies a tree transformer.
 *  </p>
 */
trait InfoTransform extends Transform {
  import global.{Symbol, Type, InfoTransformer, infoTransformers}

  def transformInfo(sym: Symbol, tpe: Type): Type

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase =
    new Phase(prev)

  protected def changesBaseClasses = true
  protected def keepsTypeParams = true

  class Phase(prev: scala.tools.nsc.Phase) extends super.Phase(prev) {
    override val keepsTypeParams = InfoTransform.this.keepsTypeParams

    if (infoTransformers.nextFrom(id).pid != id) {
      // this phase is not yet in the infoTransformers
      val infoTransformer = new InfoTransformer {
        val pid = id
        val changesBaseClasses = InfoTransform.this.changesBaseClasses
        def transform(sym: Symbol, tpe: Type): Type = transformInfo(sym, tpe)
      }
      infoTransformers.insert(infoTransformer)
    }
  }
}

