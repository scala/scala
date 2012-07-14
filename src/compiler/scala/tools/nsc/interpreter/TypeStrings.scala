/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.{ reflect => r }
import r.TypeVariable
import scala.reflect.NameTransformer
import NameTransformer._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.{ClassTag, classTag}
import typechecker.DestructureTypes
import scala.reflect.internal.util.StringOps.ojoin
import language.implicitConversions

/** A more principled system for turning types into strings.
 */
trait StructuredTypeStrings extends DestructureTypes {
  val global: Global
  import global._
  import definitions._

  case class LabelAndType(label: String, typeName: String) { }
  object LabelAndType {
    val empty = LabelAndType("", "")
  }
  case class Grouping(ldelim: String, mdelim: String, rdelim: String, labels: Boolean) {
    def join(elems: String*): String = (
      if (elems.isEmpty) ""
      else elems.mkString(ldelim, mdelim, rdelim)
    )
  }
  val NoGrouping      = Grouping("", "", "", false)
  val ListGrouping    = Grouping("(", ", ", ")", false)
  val ProductGrouping = Grouping("(", ", ", ")", true)
  val ParamGrouping   = Grouping("(", ", ", ")", true)
  val BlockGrouping   = Grouping(" { ", "; ", "}", false)

  private implicit def lowerName(n: Name): String = "" + n
  private def str(level: Int)(body: => String): String = "  " * level + body
  private def block(level: Int, grouping: Grouping)(name: String, nodes: List[TypeNode]): String = {
    val l1 = str(level)(name + grouping.ldelim)
    val l2 = nodes.map(_ show level + 1)
    val l3 = str(level)(grouping.rdelim)

    l1 +: l2 :+ l3 mkString "\n"
  }
  private def maybeBlock(level: Int, grouping: Grouping)(name: String, nodes: List[TypeNode]): String = {
    import grouping._
    val threshold = 70

    val try1 = str(level)(name + grouping.join(nodes map (_.show(0, grouping.labels)): _*))
    if (try1.length < threshold) try1
    else block(level, grouping)(name, nodes)
  }
  private def shortClass(x: Any) = {
    if (settings.debug.value) {
      val name   = (x.getClass.getName split '.').last
      val isAnon = name.reverse takeWhile (_ != '$') forall (_.isDigit)
      val str    = if (isAnon) name else (name split '$').last

      " // " + str
    }
    else ""
  }

  sealed abstract class TypeNode {
    def grouping: Grouping
    def nodes: List[TypeNode]

    def show(indent: Int, showLabel: Boolean): String = maybeBlock(indent, grouping)(mkPrefix(showLabel), nodes)
    def show(indent: Int): String = show(indent, true)
    def show(): String = show(0)

    def withLabel(l: String): this.type = modifyNameInfo(_.copy(label = l))
    def withType(t: String): this.type  = modifyNameInfo(_.copy(typeName = t))

    def label       = nameInfo.label
    def typeName    = nameInfo.typeName

    protected def mkPrefix(showLabel: Boolean) = {
      val pre = if (showLabel && label != "") label + " = " else ""
      pre + typeName
    }
    override def toString = show() // + "(toString)"
    private var nameInfo: LabelAndType = LabelAndType.empty
    private def modifyNameInfo(f: LabelAndType => LabelAndType): this.type = {
      nameInfo = f(nameInfo)
      this
    }
  }
  case class TypeAtom[T](atom: T) extends TypeNode {
    def grouping = NoGrouping
    def nodes = Nil
    override protected def mkPrefix(showLabel: Boolean) =
      super.mkPrefix(showLabel) + atom + shortClass(atom)
  }
  case class TypeProduct(nodes: List[TypeNode]) extends TypeNode {
    def grouping: Grouping = ProductGrouping
    def emptyTypeName = ""
    override def typeName = if (nodes.isEmpty) emptyTypeName else super.typeName
  }

  /** For a NullaryMethod, in = TypeEmpty; for MethodType(Nil, _) in = TypeNil */
  class NullaryFunction(out: TypeNode) extends TypeProduct(List(out)) {
    override def typeName = "NullaryMethodType"
  }
  class MonoFunction(in: TypeNode, out: TypeNode) extends TypeProduct(List(in, out)) {
    override def typeName = "MethodType"
  }
  class PolyFunction(in: TypeNode, out: TypeNode) extends TypeProduct(List(in, out)) {
    override def typeName = "PolyType"
  }

  class TypeList(nodes: List[TypeNode]) extends TypeProduct(nodes) {
    override def grouping = ListGrouping
    override def emptyTypeName = "Nil"
    override def typeName = "List"
  }
  class TypeScope(nodes: List[TypeNode]) extends TypeProduct(nodes) {
    override def grouping = BlockGrouping
    override def typeName = "Scope"
    override def emptyTypeName = "EmptyScope"
  }

  object TypeEmpty extends TypeNode {
    override def grouping = NoGrouping
    override def nodes = Nil
    override def label = ""
    override def typeName = ""
    override def show(indent: Int, showLabel: Boolean) = ""
  }

  object intoNodes extends DestructureType[TypeNode] {
    def withLabel(node: TypeNode, label: String): TypeNode   = node withLabel label
    def withType(node: TypeNode, typeName: String): TypeNode = node withType typeName

    def wrapEmpty                             = TypeEmpty
    def wrapSequence(nodes: List[TypeNode])   = new TypeList(nodes)
    def wrapProduct(nodes: List[TypeNode])    = new TypeProduct(nodes)
    def wrapPoly(in: TypeNode, out: TypeNode) = new PolyFunction(in, out)
    def wrapMono(in: TypeNode, out: TypeNode) = if (in == wrapEmpty) new NullaryFunction(out) else new MonoFunction(in, out)
    def wrapAtom[U](value: U)                 = new TypeAtom(value)
  }

  def show(tp: Type): String = intoNodes(tp).show
}


/** Logic for turning a type into a String.  The goal is to be
 *  able to take some arbitrary object 'x' and obtain the most precise
 *  String for which an injection of x.asInstanceOf[String] will
 *  be valid from both the JVM's and scala's perspectives.
 *
 *  "definition" is when you want strings like
 */
trait TypeStrings {
  private val ObjectClass = classOf[java.lang.Object]
  private val primitives = Set[String]("byte", "char", "short", "int", "long", "float", "double", "boolean", "void")
  private val primitiveMap = primitives.toList map { x =>
    val key = x match {
      case "void" => "Void"
      case "int"  => "Integer"
      case "char" => "Character"
      case s      => s.capitalize
    }
    val value = x match {
      case "void" => "Unit"
      case s      => s.capitalize
    }

    ("java.lang." + key) -> ("scala." + value)
  } toMap

  def scalaName(s: String): String = {
    if (s endsWith MODULE_SUFFIX_STRING) s.init + ".type"
    else if (s == "void") "scala.Unit"
    else if (primitives(s)) "scala." + s.capitalize
    else primitiveMap.getOrElse(s, NameTransformer.decode(s))
  }
  // Trying to put humpty dumpty back together again.
  def scalaName(clazz: JClass): String = {
    val name      = clazz.getName
    val isAnon    = clazz.isScalaAnonymous
    val enclClass = clazz.getEnclosingClass
    def enclPre   = enclClass.getName + MODULE_SUFFIX_STRING
    def enclMatch = name startsWith enclPre

    scalaName(
      if (enclClass == null || isAnon || !enclMatch) name
      else enclClass.getName + "." + (name stripPrefix enclPre)
    )
  }
  def scalaName(ct: ClassTag[_]): String = scalaName(ct.runtimeClass)
  def anyClass(x: Any): JClass          = if (x == null) null else x.getClass

  private def brackets(tps: String*): String =
    if (tps.isEmpty) ""
    else tps.mkString("[", ", ", "]")

  private def tvarString(tvar: TypeVariable[_]): String = tvarString(tvar.getBounds.toList)
  private def tvarString(bounds: List[AnyRef]): String = {
    val xs = bounds filterNot (_ == ObjectClass) collect { case x: JClass => x }
    if (xs.isEmpty) "_"
    else scalaName(xs.head)
  }
  private def tparamString(clazz: JClass): String = {
    brackets(clazz.getTypeParameters map tvarString: _*)
  }

  private def tparamString[T: ru.TypeTag] : String = {
    // [Eugene++ to Paul] needs review!!
    def typeArguments: List[ru.Type] = ru.typeOf[T].typeArguments
    // [Eugene++] todo. need to use not the `rootMirror`, but a mirror with the REPL's classloader
    // how do I get to it? acquiring context classloader seems unreliable because of multithreading
    def typeVariables: List[java.lang.Class[_]] = typeArguments map (targ => ru.rootMirror.runtimeClass(targ))
    brackets(typeArguments map (jc => tvarString(List(jc))): _*)
  }

  /** Going for an overabundance of caution right now.  Later these types
   *  can be a lot more precise, but right now the tags have a habit of
   *  introducing material which is not syntactically valid as scala source.
   *  When this happens it breaks the repl.  It would be nice if we mandated
   *  that tag toString methods (or some other method, since it's bad
   *  practice to rely on toString for correctness) generated the VALID string
   *  representation of the type.
   */
  def fromTypedValue[T: ru.TypeTag : ClassTag](x: T): String = fromTag[T]
  def fromValue(value: Any): String                          = if (value == null) "Null" else fromClazz(anyClass(value))
  def fromClazz(clazz: JClass): String                       = scalaName(clazz) + tparamString(clazz)
  def fromTag[T: ru.TypeTag : ClassTag] : String             = scalaName(classTag[T].runtimeClass) + tparamString[T]

  /** Reducing fully qualified noise for some common packages.
   */
  def quieter(tpe: String, alsoStrip: String*): String = {
    val transforms = List(
      "scala.collection.immutable." -> "immutable.",
      "scala.collection.mutable." -> "mutable.",
      "scala.collection.generic." -> "generic.",
      "java.lang." -> "jl.",
      "scala.runtime." -> "runtime."
    ) ++ (alsoStrip map (_ -> ""))

    transforms.foldLeft(tpe) {
      case (res, (k, v)) => res.replaceAll(k, v)
    }
  }

  val typeTransforms = List(
    "java.lang." -> "",
    "scala.collection.immutable." -> "immutable.",
    "scala.collection.mutable." -> "mutable.",
    "scala.collection.generic." -> "generic."
  )
}

object TypeStrings extends TypeStrings { }