/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc
package doc

import scala.collection.mutable
import compat.Platform.{EOL => LINE_SEPARATOR}


/** This class attempts to reverse engineer source code intent from compiler
 *  symbol objects.
 *
 * @author Sean McDirmid
 */
trait ModelExtractor {
  val global: Global
  import global._
  def settings: doc.Settings

  def assert(b: Boolean) {
    if (!b) throw new Error
  }

  def assert(b: Boolean, message: Any) {
    if (!b) throw new Error(message.toString)
  }

  case class Tag(tag: String, option: String, body: String)

  case class Comment(body: String, attributes: List[Tag]) {
    def decodeAttributes = {
      val map = new mutable.LinkedHashMap[String, List[(String, String)]] {
        override def default(key: String) = Nil
      }
      attributes.foreach(a => {
        map(a.tag) = map(a.tag) ::: List((a.option, a.body))
      });
      map
    }
  }
  protected def decode(sym: Symbol) =
    if (sym == definitions.ScalaObjectClass || sym == definitions.ObjectClass)
      definitions.AnyRefClass
    else sym match {
      case sym: ModuleClassSymbol => sym.sourceModule
      case sym => sym
    }

  protected def decodeComment(comment0: String): Comment = {
    val comment = { // discard outmost comment delimiters if present
      val begin = if (comment0 startsWith "/**") 3 else 0
      val end = comment0.length - (if (comment0 endsWith "*/") 2 else 0)
      comment0.substring(begin, end)
    }
    val tok = new java.util.StringTokenizer(comment, LINE_SEPARATOR)
    val buf = new StringBuilder
    type AttrDescr = (String, String, StringBuilder)
    val attributes = new collection.mutable.ListBuffer[AttrDescr]
    var attr: AttrDescr = null
    while (tok.hasMoreTokens) {
      val s = tok.nextToken.replaceFirst("\\p{Space}?\\*", "")
      val mat1 = pat1.matcher(s)
      if (mat1.matches) {
        attr = (mat1.group(1), null, new StringBuilder(mat1.group(2)))
        //if (kind != CONSTRUCTOR)
        attributes += attr
      } else {
        val mat2 = pat2.matcher(s)
        if (mat2.matches) {
          attr = (mat2.group(1), mat2.group(2), new StringBuilder(mat2.group(3)))
          //if (kind != CLASS)
          attributes += attr
        } else if (attr ne null)
          attr._3.append(s + LINE_SEPARATOR)
        else
          buf.append(s + LINE_SEPARATOR)
      }
    }
    Comment(buf.toString, attributes.toList.map({x => Tag(x._1,x._2,x._3.toString)}))
  }

  sealed abstract class Entity(val sym: Symbol) {
    private[ModelExtractor] def sym0 = sym

    override def toString = sym.toString
    def comment: Option[String] = global.comments.get(sym)
    // comments decoded, now what?
    def attributes = sym.annotations
    def decodeComment: Option[Comment] = {
      val comment0 = this.comment
      if (comment0.isEmpty) None
      else Some(ModelExtractor.this.decodeComment(comment0.get.trim))
    }
    protected def accessQualified(core: String, qual: String) = core match {
      case "public" => "" // assert(qual == null); "";
      case core => core + (if (qual == null) "" else "[" + qual + "]")
    }

    def flagsString = {
      import symtab.Flags
      //val isLocal = sym.hasFlag(Flags.LOCAL)
      val x =
        if (sym hasFlag Flags.PRIVATE) "private"
        else if (sym hasFlag Flags.PROTECTED) "protected"
        else "public"
      var string = accessQualified(x,
        if (sym hasFlag Flags.LOCAL) "this"
        else if (sym.privateWithin != null && sym.privateWithin != NoSymbol)
          sym.privateWithin.nameString
        else null
      )
      def f(flag: Int, str: String) {
        if (sym hasFlag flag) string = string + " " + str
      }
      f(Flags.IMPLICIT, "implicit")
      f(Flags.SEALED, "sealed")
      f(Flags.OVERRIDE, "override")
      f(Flags.CASE, "case")
      if (!sym.isTrait) f(Flags.ABSTRACT, "abstract")
      if (!sym.isModule) f(Flags.FINAL, "final")
      if (!sym.isTrait) f(Flags.DEFERRED, "abstract")
      string.trim
    }
    def listName = name
    def name = sym.nameString
    def fullName(sep: Char) = sym.fullNameString(sep)
    def kind: String
    def header { }
    def typeParams: List[TypeParam] = Nil
    def valueParams: List[List[ValueParam]] = Nil
    def resultType: Option[Type] = None
    def parents: Iterable[Type] = Nil
    def lo: Option[Type] = sym.info match {
      case TypeBounds(lo, hi) if decode(lo.typeSymbol) != definitions.NothingClass => Some(lo)
      case _ => None
    }
    def hi: Option[Type] = sym.info match {
      case TypeBounds(lo, hi) if decode(hi.typeSymbol) != definitions.AnyClass => Some(hi)
      case _ => None
    }
    def variance = {
      import symtab.Flags._
      if (sym hasFlag COVARIANT) "+"
      else if (sym hasFlag CONTRAVARIANT) "-"
      else ""
    }
    def overridden: Iterable[Symbol] = Nil
  }

  class ValueParam(sym: Symbol) extends Entity(sym) {
    override def resultType = Some(sym.tpe)
    //def kind = if (sym.isPublic) "val" else "";
    def kind = ""
  }

  class ConstructorParam(sym: Symbol) extends ValueParam(sym) {
    override protected def accessQualified(core: String, qual: String) = core match {
      case "public" => "val"
      case "protected" => super.accessQualified(core,qual) + " val"
      case "private" if qual == "this" => ""
      case core => super.accessQualified(core, qual)
    }
  }

  def ValueParam(sym: Symbol) = new ValueParam(sym)
  class TypeParam(sym: Symbol) extends Entity(sym) {
    def kind = ""
  }
  def TypeParam(sym: Symbol) = new TypeParam(sym)

  trait Clazz extends ClassOrObject {
    private def csym = sym.asInstanceOf[TypeSymbol]
    override def typeParams = csym.typeParams.map(TypeParam)
    override def valueParams = {
      if (constructorArgs.isEmpty) Nil
      else constructorArgs.valuesIterator.toList :: Nil
    }
    def isTrait = csym.isTrait
    override def kind = if (sym.isTrait) "trait" else "class"
  }

  trait Object extends ClassOrObject {
    override def kind = "object"
  }

  case class Package(override val sym: Symbol) extends Entity(sym) {
    override def kind = "package"
    override def name = fullName('.')
  }

  trait TopLevel extends ClassOrObject
  class TopLevelClass (sym: Symbol) extends Entity(sym) with TopLevel with Clazz
  class TopLevelObject(sym: Symbol) extends Entity(sym) with TopLevel with Object {
    override def attributes = sym.moduleClass.annotations
  }

  def compare(pathA: List[ClassOrObject], pathB: List[ClassOrObject]): Int = {
    var pA = pathA
    var pB = pathB
    while (true) {
      if (pA.isEmpty) return -1
      if (pB.isEmpty) return +1
      val diff = pA.head.name compare pB.head.name
      if (diff != 0) return diff
      pA = pA.tail
      pB = pB.tail
    }
    0
  }

  def isAccessible(sym: Symbol): Boolean = {
    import symtab.Flags._
    settings.memberaccess.value match {
      case "private"   => sym.isPublic || (sym hasFlag PROTECTED) || (sym hasFlag PRIVATE)
      case "protected" => sym.isPublic || (sym hasFlag PROTECTED)
      case "public"    => sym.isPublic
      case _           => false
    }
  }

  trait ClassOrObject extends Entity {
    def path: List[ClassOrObject] = this :: Nil
    override def listName = path map (_.name) mkString "."

    object freshParents extends mutable.LinkedHashSet[Type] {
      this ++= sym.tpe.parents
      this.toList foreach (this --= _.parents)
    }
    object constructorArgs extends mutable.LinkedHashMap[Symbol, ValueParam] {
      import symtab.Flags._
      sym.constrParamAccessors.filter(arg => ! (arg hasFlag SYNTHETIC)).foreach(arg => {
        val str = flagsToString(arg.flags)
        assert((arg hasFlag PRIVATE) && (arg hasFlag LOCAL), arg)
        val argName = arg.name.toString.trim
        val actual = sym.tpe.decls.iterator.find(e => {
          val eName = e.name.toString.trim;
          argName == eName && {
            val str = flagsToString(e.flags);
            !e.hasFlag(LOCAL);
          }
        });
        val param = actual getOrElse arg
        this(param) = new ConstructorParam(param)
      });
    }
    object decls extends mutable.LinkedHashMap[Symbol, Member] {
      sym.tpe.decls.iterator.foreach(e => {
        if (!constructorArgs.contains(e)) {
          val m = Member(e)
          if (!m.isEmpty && !this.contains(e)) this.put(e, m.get)
        }
      });
    }
    def members0(f: Symbol => Boolean) = decls.filterKeys(f).valuesIterator.toList
    def members(c: Category): Iterable[Member] = members0(c.f)
    object inherited extends mutable.LinkedHashMap[Symbol, List[Member]]() {
      override def default(tpe: Symbol) = Nil
      for (m <- sym.tpe.members if !sym.tpe.decls.iterator.contains(m) &&
          (Values.f(m) || Methods.f(m))) {
        val o = m.overridingSymbol(sym)
        if (o == NoSymbol) {
          val parent = decode(m.enclClass)
          val mo = Member(m)
          if (!mo.isEmpty) {
            this(parent) = mo.get :: this(parent)
          }
        }
      }
    }
    override def parents = freshParents
    abstract class Member(sym: Symbol) extends Entity(sym) {
      private def overriding = sym.allOverriddenSymbols
      override def comment = super.comment match {
      case ret @ Some(comment) =>
        ret
      case None =>
        val o = overriding.find(comments.contains)
        o.map(comments.apply)
      }
    }
    abstract class ValDef(sym: Symbol) extends Member(sym) {
      override def resultType = Some(resultType0)
      protected def resultType0: Type
      override def overridden: Iterable[Symbol] = {
        var ret: mutable.LinkedHashSet[Symbol] = null
        for (parent <- ClassOrObject.this.parents) {
          val sym0 = sym.overriddenSymbol(parent.typeSymbol)
          if (sym0 != NoSymbol) {
            if (ret == null) ret = new mutable.LinkedHashSet[Symbol];
            ret += sym0
          }
        }
        if (ret == null) Nil else ret
      }
    }
    case class Def(override val sym : TermSymbol) extends ValDef(sym) {
      override def resultType0 = sym.tpe.finalResultType
      override def typeParams = sym.tpe.typeParams.map(TypeParam)
      override def valueParams = methodArgumentNames.get(sym) match {
        case Some(argss) if argss.length > 1 || (!argss.isEmpty && !argss(0).isEmpty) =>
          argss map (_.map(ValueParam))
        case _ =>
          var i = 0
          val ret = for (tpe <- sym.tpe.paramTypes) yield {
            val ret = sym.newValueParameter(sym.pos, newTermName("arg" + i));
            ret setInfo tpe
            i += 1
            ValueParam(ret)
          }
          if (ret.isEmpty) Nil
          else ret :: Nil
      }
      override def kind = "def"
    }
    case class Val(override val sym: TermSymbol) extends ValDef(sym) {
      import symtab.Flags._
      def resultType0: Type = sym.tpe
      override def kind: String =
        if (sym hasFlag ACCESSOR) {
          val setterName = nme.getterToSetter(sym.name)
          val setter = sym.owner.info.decl(setterName)
          val lazyMod = if (sym hasFlag LAZY) "lazy " else ""
          lazyMod + (if (setter == NoSymbol) "val" else "var")
        } else {
          assert(sym hasFlag JAVA)
          if (sym hasFlag FINAL) "val" else "var"
        }
    }

    case class AbstractType(override val sym: Symbol) extends Member(sym) {
      override def kind = "type"
    }

    abstract class NestedClassOrObject(override val sym: Symbol) extends Member(sym) with ClassOrObject {
      override def path: List[ClassOrObject] = ClassOrObject.this.path ::: super.path
    }

    case class NestedClass(override val sym: ClassSymbol) extends NestedClassOrObject(sym) with Clazz

    case class NestedObject(override val sym: ModuleSymbol) extends NestedClassOrObject(sym) with Object {
      override def attributes = sym.moduleClass.annotations
    }

    def isVisible(sym: Symbol): Boolean = {
      import symtab.Flags._
      if (sym.isLocalClass) return false
      if (sym.isLocal) return false
      if (sym.isPrivateLocal) return false
      // the next line used to return !inIDE - now it returns true.  The underlying
      // logic being applied here is somewhat mysterious (if PRIVATE return isVisible == true?)
      // but changing it causes the docgenerator.scala test case to break, so I leave as-is.
      if (sym hasFlag PRIVATE) return true
      if (sym hasFlag SYNTHETIC) return false
      if (sym hasFlag BRIDGE) return false
      if ((sym.nameString indexOf "$") != -1) return false
      if ((sym hasFlag CASE) && sym.isMethod) return false
      true
    }

    def Member(sym: Symbol): Option[Member] = {
      import global._
      import symtab.Flags
      if (!isVisible(sym))
        None
      else if (!isAccessible(sym))
        None
      else if (sym hasFlag Flags.ACCESSOR) {
        if (sym.isSetter) return None;
        assert(sym.isGetter);
        Some[Member](new Val(sym.asInstanceOf[TermSymbol]))
      }
      else if (sym.isValue && !sym.isMethod && !sym.isModule) {
        if (!sym.hasFlag(Flags.JAVA)) {
          Console.println("SYM: " + sym + " " + sym.fullNameString('.'))
          Console.println("FLA: " + Flags.flagsToString(sym.flags))
        }
        assert(sym hasFlag Flags.JAVA)
        Some[Member](new Val(sym.asInstanceOf[TermSymbol]))
      }
      else if (sym.isValue && !sym.isModule) {
        val str = Flags.flagsToString(sym.flags)
        assert(sym.isMethod)
        Some[Member](new Def(sym.asInstanceOf[TermSymbol]))
      }
      else if (sym.isAliasType || sym.isAbstractType)
        Some(new AbstractType(sym))
      else if (sym.isClass)
        Some(new NestedClass(sym.asInstanceOf[ClassSymbol]))
      else if (sym.isModule)
        Some(new NestedObject(sym.asInstanceOf[ModuleSymbol]))
      else
        None
    }

  }
  case class Category(label: String)(g: Symbol => Boolean) {
    val f = g
    def plural = label + "s"
  }
  val Constructors = new Category("Additional Constructor")(e => e.isConstructor && !e.isPrimaryConstructor) {
    // override def plural = "Additional Constructors";
  }
  val Objects = Category("Object")(_.isModule);
  val Classes = new Category("Class")(sym => sym.isClass || (sym == definitions.AnyRefClass)) {
    override def plural = "Classes"
  }
  val Values = new Category("Value")(e => e.isValue && e.hasFlag(symtab.Flags.ACCESSOR)) {
    override def plural = "Values and Variables"
  }
  val Methods = Category("Method")(e => e.isValue && e.isMethod && !e.isConstructor && !e.hasFlag(symtab.Flags.ACCESSOR));
  val Types = Category("Type")(e => e.isAliasType || e.isAbstractType);

  val categories = Constructors :: Types :: Values :: Methods :: Classes :: Objects :: Nil;


  import java.util.regex.Pattern
  // patterns for standard tags with 1 and 2 arguments
  private val pat1 = Pattern.compile(
    "[ \t]*@(author|deprecated|owner|pre|return|see|since|todo|version|ex|note)[ \t]*(.*)")
  private val pat2 = Pattern.compile(
    "[ \t]*@(exception|param|throws)[ \t]+(\\p{Graph}*)[ \t]*(.*)")

  def sort[E <: Entity](entities: Iterable[E]): Iterable[E] = {
    val set = new collection.immutable.TreeSet[E]()(new Ordering[E] {
      def compare(eA : E, eB: E): Int = {
        if (eA eq eB) return 0;
        (eA, eB) match {
          case (eA: ClassOrObject, eB: ClassOrObject) =>
            val diff = ModelExtractor.this.compare(eA.path, eB.path)
            if (diff!= 0) return diff
          case _ =>
        }
        if (eA.getClass != eB.getClass) {
          val diff = eA.getClass.getName.compare(eB.getClass.getName)
          assert(diff != 0)
          return diff
        }
        if (!eA.sym0.isPackage) {
          val diff = eA.sym0.nameString compare eB.sym0.nameString
          if (diff != 0) return diff
        }
        val diff0 = eA.sym0.fullNameString compare eB.sym0.fullNameString
        assert(diff0 != 0)
        diff0
      }
    })
    set ++ entities
  }
}
