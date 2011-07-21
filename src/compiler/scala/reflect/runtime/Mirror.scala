package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}

/** The mirror for standard runtime reflection from Java.
 */
class Mirror extends Universe with api.Mirror {

  def classWithName(name: String): Symbol = classToScala(java.lang.Class.forName(name))
  def getClass(obj: Any): Symbol = classToScala(obj.getClass)
  def getType(obj: Any): Type = typeToScala(obj.getClass)

  def getValue(receiver: AnyRef, field: Symbol): Any = fieldToJava(field).get(receiver)
  def setValue(receiver: AnyRef, field: Symbol, value: Any): Unit = fieldToJava(field).set(receiver, value)
  def invoke(receiver: AnyRef, meth: Symbol, args: Any*): Any = methodToJava(meth).invoke(receiver, args)

}

object Mirror extends Mirror

/** test code; should go to tests once things settle down a bit
 */
object Test extends Mirror with App {
  val sym = classToScala(classOf[scala.collection.Iterable[_]])
  println(sym)
  println("parents = "+sym.info.parents)
  println("decls = "+(sym.info.decls.toList map (_.defString)))
  val ms = sym.info.members.toList map (_.initialize)
  println("members = "+(ms map (_.defString) mkString ("\n  ")))
}