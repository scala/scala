package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}
import java.lang.reflect.Array

/** The mirror for standard runtime reflection from Java.
 */
class Mirror extends Universe with RuntimeTypes with api.Mirror {

  import definitions._

  def classWithName(name: String): Symbol = classToScala(java.lang.Class.forName(name))
  def getClass(obj: AnyRef): Symbol = classToScala(obj.getClass)
  def getType(obj: AnyRef): Type = typeToScala(obj.getClass)
  // to do add getClass/getType for instances of primitive types, probably like this:
  // def getClass[T <: AnyVal : Manifest](x: T): Symbol = manifest[T].getClass

  def getValue(receiver: AnyRef, field: Symbol): Any = {
    fieldToJava(field).get(receiver)
  }
  def setValue(receiver: AnyRef, field: Symbol, value: Any): Unit = {
    fieldToJava(field).set(receiver, value)
  }
  def invoke(receiver: AnyRef, meth: Symbol, args: Any*): Any = {
    if (meth.owner == ArrayClass) {
      meth.name match {
        case nme.length => return Array.getLength(receiver)
        case nme.apply => return Array.get(receiver, args(0).asInstanceOf[Int])
        case nme.update => return Array.set(receiver, args(0).asInstanceOf[Int], args(1))
      }
    }
    methodToJava(meth).invoke(receiver, args.asInstanceOf[Seq[AnyRef]]: _*)
  }

  override def classToType(jclazz: java.lang.Class[_]): Type = typeToScala(jclazz)
  override def classToSymbol(jclazz: java.lang.Class[_]): Symbol = classToScala(jclazz)

  def staticClass(name: String): Symbol = definitions.getClass(newTypeName(name))
  def staticModule(name: String): Symbol = definitions.getModule(newTermName(name))

 /** Selects type symbol with given name from the defined members of prefix type
   */
  def selectType(owner: Symbol, name: String): Symbol =
    owner.info.decl(newTypeName(name))

  /** Selects term symbol with given name and type from the defined members of prefix type
   *  @pre   The prefix type
   *  @name  The name of the selected member
   *  @tpe   The type of the selected member
   */
  def selectTerm(owner: Symbol, name: String, tpe: Type): Symbol =
    owner.info.decl(newTermName(name)) suchThat (_.tpe == tpe)

  def selectParam(owner: Symbol, idx: Int): Symbol = {
    def selectInList(params: List[Symbol], idx: Int, fallback: Type): Symbol = {
      if (params.isEmpty) selectIn(fallback, idx)
      else if (idx == 0) params.head
      else selectInList(params.tail, idx - 1, fallback)
    }
    def selectIn(tpe: Type, idx: Int): Symbol = tpe match {
      case PolyType(tparams, res) => selectInList(tparams, idx, res)
      case MethodType(params, res) => selectInList(params, idx, res)
      case _ => NoSymbol
    }
    selectIn(owner.info, idx)
  }

}

object Mirror extends Mirror

/** test code; should go to tests once things settle down a bit
 *
object Test extends Mirror with App {
  val sym = classToScala(classOf[scala.collection.Iterable[_]])
  println(sym)
  println("parents = "+sym.info.parents)
  println("decls = "+(sym.info.decls.toList map (_.defString)))
  val ms = sym.info.members.toList map (_.initialize)
  println("members = "+(ms map (_.defString) mkString ("\n  ")))
}
*/