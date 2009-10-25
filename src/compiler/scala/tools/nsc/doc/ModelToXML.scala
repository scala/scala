/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc
package doc

import scala.xml._

/** This class has functionality to format source code models as XML blocks.
 *
 *  @author  Sean McDirmid, Stephane Micheloud
 */
trait ModelToXML extends ModelExtractor {
  import global._
  import definitions.AnyRefClass
  import DocUtil._
  // decode entity into XML.
  type Frame

  protected def urlFor(sym: Symbol)(implicit frame: Frame): String
  protected def anchor(sym: Symbol)(implicit frame: Frame): NodeSeq

  def aref(href: String, label: String)(implicit frame: Frame): NodeSeq
/*
  def link(entity: Symbol)(implicit frame: Frame): NodeSeq = {
    val url = urlFor(entity)
    // nothing to do but be verbose.
    if (url == null)
      Text(entity.owner.fullNameString('.') + '.' + entity.nameString)
    else
      aref(url, entity.nameString)
  }
*/
  def link(entity: Symbol, label: String)(implicit frame: Frame): NodeSeq = {
    val url = urlFor(entity)
    if (url == null) { // external link (handled by script.js)
      val (href, attr) =
        if (entity.isClass || (entity==AnyRefClass))
          ("", entity.owner.fullNameString('/') + '/' + entity.nameString)
        else
          ("#" + entity.nameString, entity.owner.fullNameString('/'))
      val name = entity.owner.fullNameString('.') + '.' + entity.nameString
      <a href={Utility.escape(href)} class={attr} target="contentFrame">{name}</a>;
    }
    else
      aref(url, label)
  }

  def link(entity: Symbol)(implicit frame: Frame): NodeSeq =
    link(entity, entity.nameString)

  def link(tpe: Type)(implicit frame: Frame): NodeSeq = {
    if (!tpe.typeArgs.isEmpty) {
      if (definitions.isFunctionType(tpe)) {
        val (args,r) = tpe.normalize.typeArgs.splitAt(tpe.normalize.typeArgs.length - 1);
        args.mkXML("(", ", ", ")")(link) ++ Text(" => ") ++ link(r.head);
      } else if (definitions.isRepeatedParamType(tpe)) {
        assert(tpe.typeArgs.length == 1)
        link(tpe.typeArgs(0)) ++ Text("*")
      } else if (tpe.typeSymbol == definitions.ByNameParamClass) {
        assert(tpe.typeArgs.length == 1)
        Text("=> ") ++ link(tpe.typeArgs(0))
      } else if (tpe.typeSymbol.name.toString.startsWith("Tuple") &&
                 tpe.typeSymbol.owner.name == nme.scala_.toTypeName) {
        tpe.typeArgs.mkXML("(", ", ", ")")(link)
      } else
        link(decode(tpe.typeSymbol)) ++ tpe.typeArgs.surround("[", "]")(link)
    } else tpe match {
      case PolyType(tparams,result) =>
        link(result) ++ tparams.surround("[", "]")(link)
      case RefinedType(parents,_) =>
        val parents1 =
          if ((parents.length > 1) &&
              (parents.head.typeSymbol eq definitions.ObjectClass)) parents.tail;
          else parents;
       parents1.mkXML(Text(""), <code> with </code>, Text(""))(link);
     case _ =>
       if (tpe.typeSymbol == NoSymbol) {
         throw new Error(tpe + " has no type class " + tpe.getClass)
       }
       link(decode(tpe.typeSymbol))
    }
  }

  private def printIf[T](what: Option[T], before: String, after: String)(f: T => NodeSeq): NodeSeq =
    if (what.isEmpty) Text("")
    else Text(before) ++ f(what.get) ++ Text(after)

  def bodyFor(entity: Entity)(implicit frame: Frame): NodeSeq = try {
    var seq = {entity.typeParams.surround("[", "]")(e => {
      Text(e.variance) ++ <em>{e.name}</em> ++
        {printIf(e.hi, " <: ", "")(link)} ++
        {printIf(e.lo, " >: ", "")(link)}
    })} ++ printIf(entity.hi, " <: ", "")(link) ++
           printIf(entity.lo, " >: ", "")(link);
    {entity.valueParams.foreach(xs => {
      seq = seq ++ xs.mkXML("(", ", ", ")")(arg =>
        {
          val str = arg.flagsString.trim
          if (str.length == 0) NodeSeq.Empty
          else <code>{Text(str)} </code>
        } ++
        <em>{arg.name}</em> ++ (try {

          Text(" : ") ++ link(arg.resultType.get)
        } catch {
          case e : Throwable => System.err.println("ARG " + arg + " in " + entity); throw e
        })
      );
      seq
    })};
    seq ++ {printIf(entity.resultType, " : ", "")(tpe => link(tpe))}
  } catch {
    case e => System.err.println("generating for " + entity); throw e
  }

  def extendsFor(entity: Entity)(implicit frame: Frame): NodeSeq = {
    if (entity.parents.isEmpty) NodeSeq.Empty
    else <code> extends </code>++
      entity.parents.mkXML(Text(""), <code> with </code>, Text(""))(link);
  }

  def parse(str: String): NodeSeq = {
    new SpecialNode {
      def label = "#PCDATA"
      def buildString(sb: StringBuilder): StringBuilder = {
        sb.append(str.trim)
        sb
      }
    }
  }

  def longHeader(entity: Entity)(implicit frame: Frame): NodeSeq = Group({
    anchor(entity.sym) ++ <dl>
      <dt>
        {attrsFor(entity)}
        <code>{Text(entity.flagsString)}</code>
        <code>{Text(entity.kind)}</code>
        <em>{entity.sym.nameString}</em>{bodyFor(entity)}
      </dt>
      <dd>{extendsFor(entity)}</dd>
    </dl>;
  } ++ {
    val cmnt = entity.decodeComment
    if (cmnt.isEmpty) NodeSeq.Empty
    else longComment(entity, cmnt.get)
  } ++ (entity match {
      case entity: ClassOrObject => classBody(entity)
      case _ => NodeSeq.Empty
  }) ++ {
    val overridden = entity.overridden
    if (overridden.isEmpty)
      NodeSeq.Empty
    else {
      <dl>
        <dt style="margin:10px 0 0 20px;">
          <b>Overrides</b>
        </dt>
        <dd>
        { overridden.mkXML("",", ", "")(sym => link(decode(sym.owner)) ++ Text(".") ++ link(sym))
        }
        </dd>
      </dl>
    }
  } ++ <hr/>);

  def longComment(entity: Entity, cmnt: Comment)(implicit frame: Frame): NodeSeq = {
    val attrs = <dl>{
      var seq: NodeSeq = NodeSeq.Empty
      cmnt.decodeAttributes.foreach{
      case (tag, xs) =>
        seq = seq ++ <dt style="margin:10px 0 0 20px;">
        <b>{decodeTag(tag)}</b></dt> ++ {xs.flatMap{
        case (option,body) => <dd>{
          if (option == null) NodeSeq.Empty;
          else decodeOption(tag, option);
        }{ tag match {
             case "see" => resolveSee(entity.sym, body.trim)
             case _ => parse(body)
           }}</dd>
        }}
      };
      seq
    }</dl>;
    <xml:group>
      <dl><dd>{parse(cmnt.body)}</dd></dl>
      {attrs}
    </xml:group>
  }

  /**
   * Try to be smart about @see elements. If the body looks like a link, turn it into
   * a link. If it can be resolved in the symbol table, turn it into a link to the referenced
   * entity.
   */
  private def resolveSee(owner: Symbol, body: String)(implicit frame: Frame): NodeSeq = {
    /** find a class either in the root package, in the current class or in the current package. */
    def findClass(clsName: String): Symbol = {
      try { definitions.getClass(clsName) } catch {
        case f: FatalError =>
          try { definitions.getMember(owner, clsName.toTypeName) } catch {
            case f: FatalError =>
              definitions.getMember(owner.enclosingPackage, clsName.toTypeName)
          }
      }
    }

    if (body.startsWith("http://")
        || body.startsWith("https://")
        || body.startsWith("www")) {
      // a link
      body.split(" ") match {
        case Seq(href, txt, rest @ _*) =>
          <a href={href}>{txt}{rest}</a>
        case _ =>
          <a href={body}>{body}</a>
      }
    } else try {
      // treat it like a class or member reference
      body.split("#") match {
        case Seq(clazz, member) =>
          val clazzSym = if (clazz.length == 0) owner.enclClass else findClass(clazz)
          link(definitions.getMember(clazzSym, member), body)
        case Seq(clazz, _*) =>
          link(findClass(clazz), body)
        case _ =>
          parse(body)
      }
    } catch {
      case f: FatalError =>
        log("Error resolving @see: " + f.toString)
        parse(body)
    }
  }

  def classBody(entity: ClassOrObject)(implicit from: Frame): NodeSeq =
    <xml:group>
      {categories.mkXML("","\n","")(c => shortList(entity, c)) : NodeSeq}
      {categories.mkXML("","\n","")(c =>  longList(entity, c)) : NodeSeq}
    </xml:group>;

  def longList(entity: ClassOrObject, category: Category)(implicit from: Frame): NodeSeq = {
    val xs = entity.members(category)
    if (!xs.iterator.hasNext)
      NodeSeq.Empty
    else Group(
        <table cellpadding="3" class="member-detail" summary="">
          <tr><td class="title">{Text(category.label)} Details</td></tr>
        </table>
        <div>{xs.mkXML("","\n","")(m => longHeader(m))}</div>)
  }

  def shortList(entity: ClassOrObject, category: Category)(implicit from: Frame): NodeSeq = {
    val xs = entity.members(category)
    var seq: NodeSeq = NodeSeq.Empty
    if (xs.iterator.hasNext) {
      // alphabetic
      val set = new scala.collection.immutable.TreeSet[entity.Member]()(new Ordering[entity.Member] {
        def compare(mA : entity.Member, mB: entity.Member): Int =
          if (mA eq mB) 0
          else {
            val diff = mA.name compare mB.name
            if (diff != 0) diff
            else {
              val diff0 = mA.hashCode - mB.hashCode
              assert(diff0 != 0, mA.name)
              diff0
            }
          }
      })++xs
      seq = seq ++ <table cellpadding="3" class="member" summary="">
      <tr><td colspan="2" class="title">{Text(category.label + " Summary")}</td></tr>
      {set.mkXML("","\n","")(mmbr => shortHeader(mmbr))}
      </table>
    }
    // list inherited members...if any.
    for ((tpe,members) <- entity.inherited) {
      val members0 = members.filter(m => category.f(m.sym));
      if (!members0.isEmpty) seq = seq ++ <table cellpadding="3" class="inherited" summary="">
        <tr><td colspan="2" class="title">
          {Text(category.plural + " inherited from ") ++ link(tpe)}
        </td></tr>
        <tr><td colspan="2" class="signature">
          {members0.mkXML((""), (", "), (""))(m => {
            link(decode(m.sym)) ++
              (if (m.sym.hasFlag(symtab.Flags.ABSTRACT) || m.sym.hasFlag(symtab.Flags.DEFERRED)) {
                Text(" (abstract)");
              } else NodeSeq.Empty);
           })}
        </td></tr>
      </table>
    }
    seq;
  }

  protected def decodeOption(tag: String, string: String): NodeSeq =
    <code>{Text(string + " - ")}</code>;

  protected def decodeTag(tag: String): String = tag.capitalize

  def shortHeader(entity: Entity)(implicit from: Frame): NodeSeq =
    <tr>
      <td valign="top" class="modifiers">
        <code>{Text(entity.flagsString)} {Text(entity.kind)}</code>
      </td>
      <td class="signature">
        <em>{link(decode(entity.sym))}</em>
        {bodyFor(entity) ++ extendsFor(entity)}
        {
          entity.resultType match {
            case Some(PolyType(_, ConstantType(v))) => Text(" = " + v.escapedStringValue)
            case _ => NodeSeq.Empty
          }
        }
        {
          val cmnt = entity.decodeComment
          if (cmnt.isEmpty) NodeSeq.Empty
          else shortComment(cmnt.get)
        }
      </td>
    </tr>

  import java.util.regex.Pattern
  // pattern detecting first line of comment (see ticket #224)
  private val pat = Pattern.compile("[ \t]*(/\\*)[ \t]*")

  /** Ticket #224
   *  Write the first sentence as a short summary of the method, as scaladoc
   *  automatically places it in the method summary table (and index).
   *  (see http://java.sun.com/j2se/javadoc/writingdoccomments/)
   */
  def shortComment(cmnt: Comment): NodeSeq = {
    val lines = cmnt.body split "<p>"
    val first =
      if (lines.length < 2)
        lines(0)
      else {
        val line0 = lines(0)
        val mat = pat matcher line0
        if (mat.matches()) line0 + lines(1)
        else line0
      }
    <div>{parse(first/*cmnt.body*/)}</div>
  }

  def attrsFor(entity: Entity)(implicit from: Frame): NodeSeq = {
    def attrFor(attr: AnnotationInfo): Node = {
      val buf = new StringBuilder
      val AnnotationInfo(tpe, args, nvPairs) = attr
      val name = link(decode(tpe.typeSymbol))
      if (!args.isEmpty)
        buf.append(args.mkString("(", ",", ")"))
      if (!nvPairs.isEmpty)
        for (((name, value), index) <- nvPairs.zipWithIndex) {
          if (index > 0)
            buf.append(", ")
          buf.append(name).append(" = ").append(value)
        }
      Group(name ++ Text(buf.toString))
    }
    def toGroup(x: AnnotationInfo): Node = Group(Text("@") ++ attrFor(x) ++ <br/>)
    if (entity.sym.hasFlag(symtab.Flags.CASE)) NodeSeq.Empty
    else NodeSeq fromSeq (entity.attributes map toGroup)
  }
}
