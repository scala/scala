/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author
 */
// $Id$

package scala.tools.nsc.transform

/** <p>
 *    A base class for transforms.
 *    A transform contains a compiler phase which applies a tree transformer.
 *  </p>
 *  <dl class="subclasses">
 *    <dt><b>Direct Known Subclasses:</b></dt>
 *    <dd>
 *      <a href="AddInterfaces.html" target="contentFrame">AddInterfaces</a>,
 *      <a href="ExplicitOuter.html" target="contentFrame">ExplicitOuter</a>,
 *      <a href="Flatten.html" target="contentFrame">Flatten</a>,
 *      <a href="LambdaLift.html" target="contentFrame">LambdaLift</a>,
 *      <a href="Mixin.html" target="contentFrame">Mixin</a>,
 *      <a href="UnCurry.html" target="contentFrame">UnCurry</a>
 *   </dd>
 *  </dl>
 */
abstract class InfoTransform extends Transform {
  import global.{Symbol, Type, InfoTransformer, infoTransformers}

  def transformInfo(sym: Symbol, tpe: Type): Type

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase =
    new Phase(prev)

  protected def changesBaseClasses = true

  class Phase(prev: scala.tools.nsc.Phase) extends super.Phase(prev) {
    if (infoTransformers.nextFrom(id).pid != id) {
      val infoTransformer = new InfoTransformer {
        val pid = id
        val changesBaseClasses = InfoTransform.this.changesBaseClasses
        def transform(sym: Symbol, tpe: Type): Type = transformInfo(sym, tpe)
      }
      infoTransformers.insert(infoTransformer)
    }
  }
}

