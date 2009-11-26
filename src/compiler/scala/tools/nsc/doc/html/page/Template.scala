/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html
package page

import model._

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.HashSet

class Template(tpl: DocTemplateEntity) extends HtmlPage {

  val path =
    templateToPath(tpl)

  val title = "Scaladoc for " + tpl.qualifiedName

  val headers =
    <xml:group>
		  <style type="text/css">
		    @import url({ relativeLinkTo(List("template.css", "lib")) }) screen;
		  </style>
		  <script type="text/javascript" src={ relativeLinkTo{List("template.js", "lib")} }></script>
    </xml:group>

  val valueMembers =
    (tpl.methods ::: tpl.values ::: (tpl.templates filter { tpl => tpl.isObject || tpl.isPackage })) sortWith (_.name < _.name)

  val typeMembers =
    (tpl.abstractTypes ::: tpl.aliasTypes ::: (tpl.templates filter { tpl => tpl.isTrait || tpl.isClass })) sortWith (_.name < _.name)

  val constructors = (tpl match {
    case cls: Class => cls.constructors
    case _ => Nil
  }) sortWith (_.name < _.name)

  val body =
    <body class={ if (tpl.isTrait || tpl.isClass) "type" else "value" }>

      { def ownerLinks(otl: DocTemplateEntity): NodeSeq =
          if (otl.inTemplate.isRootPackage)
            <a href={ relativeLinkTo(otl) }>{ otl.name }</a>
          else ownerLinks(otl.inTemplate) ++ <xml:group>.<a href={ relativeLinkTo(otl) }>{ otl.name }</a></xml:group>
        if (tpl.isRootPackage || tpl.inTemplate.isRootPackage)
          NodeSeq.Empty
        else
          <p id="owner">{ ownerLinks(tpl.inTemplate) }</p>
      }

      <div id="definition">
        <img src={ relativeLinkTo(List(kindToString(tpl) + "_big.png", "lib")) }/>
        <h1>{ if (tpl.isRootPackage) "root package" else tpl.name }</h1>
      </div>

      { signature(tpl, true) }

      { if (tpl.comment.isEmpty) NodeSeq.Empty else
          <div id="comment">{ commentToHtml(tpl.comment) }</div>
      }

      <div id="template">

        { if (tpl.linearization.isEmpty) NodeSeq.Empty else
            <div id="mbrsel">
              <div id="ancestors">
                <h3>Inherits</h3>
                <ol>{ tpl.linearization map { wte => <li class="in" name={ wte.qualifiedName }>{ wte.name }</li> } }</ol>
              </div>
            </div>
        }

        { if (typeMembers.isEmpty) NodeSeq.Empty else
            <div id="types" class="members">
              <h3>Type Members</h3>
              <ol>{ typeMembers map (memberToHtml(_)) }</ol>
            </div>
        }

        { if (valueMembers.isEmpty) NodeSeq.Empty else
            <div id="values" class="members">
              <h3>Value Members</h3>
              <ol>{ valueMembers map (memberToHtml(_)) }</ol>
            </div>
        }

        { if (constructors.isEmpty) NodeSeq.Empty else
            <div id="constructors" class="members">
              <h3>Instance constructors</h3>
              <ol>{ constructors map (memberToHtml(_)) }</ol>
            </div>
        }

      </div>

    </body>

  def memberToHtml(mbr: MemberEntity): NodeSeq = {
    val attributes: List[comment.Body] = Nil
    <li name={ mbr.definitionName }>
      { signature(mbr, false) }
      { val prs: List[ParameterEntity] = mbr match {
          case cls: Class if cls.isCaseClass => cls.typeParams ::: (cls.primaryConstructor map (_.valueParams.flatten)).toList.flatten
          case trt: Trait => trt.typeParams
          case dfe: Def => dfe.typeParams ::: dfe.valueParams.flatten
          case ctr: Constructor => ctr.valueParams.flatten
          case _ => Nil
        }
        def paramCommentToHtml(pr: ParameterEntity) =
          if (pr.comment.isEmpty) NodeSeq.Empty else
            <li class={ if (pr.isTypeParam) "tparam" else "param" } name={ pr.name }>{
              commentToHtml(pr.comment.get)
            }</li>
        if (prs.isEmpty) NodeSeq.Empty else
          <ol class="paramcmts">{ prs map (paramCommentToHtml(_)) }</ol>
      }
      { val fvs: List[comment.Paragraph] = mbr.visibility.toList ::: mbr.flags
        if (fvs.isEmpty) NodeSeq.Empty else
          <ol class="attributes">{ fvs map { fv => <li>{ inlineToHtml(fv.text) }</li> } }</ol>

      }
      { if (mbr.comment.isEmpty) NodeSeq.Empty else
          <div class="comment">{ commentToHtml(mbr.comment) }</div>
      }
    </li>
  }

  def kindToString(mbr: MemberEntity): String = mbr match {
    case tpl: DocTemplateEntity =>
      if (tpl.isPackage) "package" else if (tpl.isClass) "class" else if (tpl.isTrait) "trait" else "object"
    case ctor: Constructor => "new"
    case tme: MemberEntity =>
      if (tme.isDef) "def" else if (tme.isVal) "val" else if (tme.isVar) "var" else "type"
  }

  def boundsToString(hi: Option[TypeEntity], lo: Option[TypeEntity]): String = {
    def bound0(bnd: Option[TypeEntity], pre: String): String = bnd match {
      case None => ""
      case Some(tpe) => pre + typeToHtml(tpe)
    }
    bound0(hi, " <: ") + bound0(lo, " >: ")
  }

  /** name, tparams, params, result */
  def signature(mbr: MemberEntity, isSelf: Boolean): NodeSeq = {
    val inside: NodeSeq =
      <xml:group>
      <div class="kind">{ kindToString(mbr) }</div>
      <div class="symbol">
        <span class="name">{ if (mbr.isConstructor) tpl.name else mbr.name }</span>
        { def tparamsToHtml(tpss: List[TypeParam]): NodeSeq =
            if (tpss.isEmpty) NodeSeq.Empty else {
              def tparam0(tp: TypeParam): NodeSeq =
                <span name={ tp.name }>{
                  tp.variance + tp.name + boundsToString(tp.hi, tp.lo)
                }</span>
              def tparams0(tpss: List[TypeParam]): NodeSeq = (tpss: @unchecked) match {
                case tp :: Nil => tparam0(tp)
                case tp :: tps => tparam0(tp) ++ Text(",") ++ tparams0(tps)
              }
              <span class="tparams">[{ tparams0(tpss) }]</span>
            }
          mbr match {
            case trt: Trait => tparamsToHtml(trt.typeParams)
            case dfe: Def => tparamsToHtml(dfe.typeParams)
            case _ => NodeSeq.Empty
          }
        }
        { def paramsToHtml(vlsss: List[List[ValueParam]]): NodeSeq = {
            def param0(vl: ValueParam): NodeSeq =
              <span name={ vl.name }>{
                vl.name + ": " + typeToHtml(vl.resultType)
              }</span>
            def params0(vlss: List[ValueParam]): NodeSeq = vlss match {
              case Nil => NodeSeq.Empty
              case vl :: Nil => param0(vl)
              case vl :: vls => param0(vl) ++ Text(",") ++ params0(vls)
            }
            vlsss map { vlss => <span class="params">({ params0(vlss) })</span> }
          }
          mbr match {
            case cls: Class if cls.isCaseClass => paramsToHtml(cls.primaryConstructor.get.valueParams)
            case ctr: Constructor => paramsToHtml(ctr.valueParams)
            case dfe: Def => paramsToHtml(dfe.valueParams)
            case _ => NodeSeq.Empty
          }
        }
        { mbr match {
            case tpl: DocTemplateEntity if (!tpl.isPackage) =>
              tpl.parentType match {
                case Some(st) => <span class="result">extends<span>{ typeToHtml(st) }</span></span>
                case None =>NodeSeq.Empty
              }
            case tme: MemberEntity if (tme.isDef || tme.isVal || tme.isVar) =>
              <span class="result">:<span>{ typeToHtml(tme.resultType) }</span></span>
            case abt: AbstractType =>
              val b2s = boundsToString(abt.hi, abt.lo)
              if (b2s != "")
                <span class="result"><span>{ b2s }</span></span>
              else NodeSeq.Empty
            case alt: AliasType =>
              <span class="result">=<span>{ typeToHtml(alt.alias) }</span></span>
            case _ => NodeSeq.Empty
          }
        }
      </div>
        </xml:group>
    mbr match {
      case dte: DocTemplateEntity if !isSelf =>
        <a class="signature" href={ relativeLinkTo(dte) }>{ inside }</a>
      case _ =>
        <div class="signature">{ inside }</div>
    }
  }

}
