package scala.reflect.reify
package codegen

trait Util {
  self: Reifier =>

  import mirror._
  import definitions._
  import treeInfo._

  val reifyDebug = settings.Yreifydebug.value
  val reifyCopypaste = settings.Yreifycopypaste.value
  val reifyTrace = scala.tools.nsc.util.trace when reifyDebug
  object reifiedNodePrinters extends { val global: mirror.type = mirror } with tools.nsc.ast.NodePrinters with NodePrinters
  val reifiedNodeToString = reifiedNodePrinters.reifiedNodeToString

  def reifyList(xs: List[Any]): Tree =
    mkList(xs map reify)

  def reifyProduct(x: Product): Tree =
    reifyProduct(x.productPrefix, x.productIterator.toList)

  def reifyProduct(prefix: String, elements: List[Any]): Tree = {
    // reflection would be more robust, but, hey, this is a hot path
    if (prefix.startsWith("Tuple")) scalaFactoryCall(prefix, (elements map reify).toList: _*)
    else mirrorCall(prefix, (elements map reify): _*)
  }

  // helper functions

  /** Reify a case object defined in Mirror */
  def reifyMirrorObject(name: String): Tree =
    mirrorSelect(name)

  def reifyMirrorObject(x: Product): Tree =
    reifyMirrorObject(x.productPrefix)

  def call(fname: String, args: Tree*): Tree =
    Apply(termPath(fname), args.toList)

  def mirrorSelect(name: String): Tree =
    termPath(nme.MIRROR_PREFIX + name)

  def mirrorCall(name: TermName, args: Tree*): Tree =
    call("" + (nme.MIRROR_PREFIX append name), args: _*)

  def mirrorCall(name: String, args: Tree*): Tree =
    call(nme.MIRROR_PREFIX + name, args: _*)

  def mirrorFactoryCall(value: Product, args: Tree*): Tree =
    mirrorFactoryCall(value.productPrefix, args: _*)

  def mirrorFactoryCall(prefix: String, args: Tree*): Tree =
    mirrorCall(prefix, args: _*)

  def scalaFactoryCall(name: String, args: Tree*): Tree =
    call("scala." + name + ".apply", args: _*)

  def mkList(args: List[Tree]): Tree =
    scalaFactoryCall("collection.immutable.List", args: _*)

  /**
   * An (unreified) path that refers to definition with given fully qualified name
   *  @param mkName   Creator for last portion of name (either TermName or TypeName)
   */
  def path(fullname: String, mkName: String => Name): Tree = {
    val parts = fullname split "\\."
    val prefixParts = parts.init
    val lastName = mkName(parts.last)
    if (prefixParts.isEmpty) Ident(lastName)
    else {
      val prefixTree = ((Ident(prefixParts.head): Tree) /: prefixParts.tail)(Select(_, _))
      Select(prefixTree, lastName)
    }
  }

  /** An (unreified) path that refers to term definition with given fully qualified name */
  def termPath(fullname: String): Tree = path(fullname, newTermName)

  /** An (unreified) path that refers to type definition with given fully qualified name */
  def typePath(fullname: String): Tree = path(fullname, newTypeName)

  def isTough(tpe: Type) = {
    def isTough(tpe: Type) = tpe match {
      case _: RefinedType => true
      case _: ExistentialType => true
      case _: ClassInfoType => true
      case _: MethodType => true
      case _: PolyType => true
      case _ => false
    }

    tpe != null && (tpe exists isTough)
  }

  def isAnnotated(tpe: Type) = {
    def isAnnotated(tpe: Type) = tpe match {
      case _: AnnotatedType => true
      case _ => false
    }

    tpe != null && (tpe exists isAnnotated)
  }

  def origin(sym: Symbol) = {
    var origin = ""
    if (sym.owner != NoSymbol) origin += "defined by %s".format(sym.owner.name)
    if (sym.pos != NoPosition) origin += " in %s:%s:%s".format(sym.pos.source.file.name, sym.pos.line, sym.pos.column)
    if (origin == "") origin = "of unknown origin"
    origin
  }
}