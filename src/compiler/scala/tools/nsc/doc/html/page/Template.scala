/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html
package page

import model._
import scala.xml.{ NodeSeq, Text }

class Template(tpl: DocTemplateEntity) extends HtmlPage {

  val path =
    templateToPath(tpl)

  val title =
    tpl.qualifiedName

  val headers =
    <xml:group>
      <link href={ relativeLinkTo{List("template.css", "lib")} } media="screen" type="text/css" rel="stylesheet"/>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery-ui.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("template.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("tools.tooltip.js", "lib")} }></script>
    </xml:group>

  val valueMembers =
    tpl.methods ++ tpl.values ++ tpl.templates.filter(x => x.isObject || x.isPackage) sorted

  val (absValueMembers, concValueMembers) =
    valueMembers partition (_.isAbstract)

  val typeMembers =
    tpl.abstractTypes ++ tpl.aliasTypes ++ tpl.templates.filter(x => x.isTrait || x.isClass) sorted

  val constructors = (tpl match {
    case cls: Class => (cls.constructors: List[MemberEntity]).sorted
    case _ => Nil
  })

  /* for body, there is a special case for AnyRef, otherwise AnyRef appears like a package/object
   * this problem should be fixed, this implementation is just a patch
   */
  val body = {
    <body class={ if (tpl.isTrait || tpl.isClass || tpl.qualifiedName == "scala.AnyRef") "type" else "value" } onload="windowTitle();">

      { if (tpl.isRootPackage || tpl.inTemplate.isRootPackage)
          NodeSeq.Empty
        else
          <p id="owner">{ templatesToHtml(tpl.inTemplate.toRoot.reverse.tail, xml.Text(".")) }</p>
      }

      { val templateName = if (tpl.isRootPackage) "root package" else tpl.name
        val displayName = tpl.companion match {
          case Some(companion) =>
            if (companion.visibility.isPublic && companion.inSource != None)
              <a href={relativeLinkTo(companion)} title="go to companion">{ templateName }</a>
            else templateName
          case _ =>
            templateName
        }
        <div id="definition">
          <img src={ relativeLinkTo(List(docEntityKindToBigImage(tpl), "lib")) }/>
          <h1>{ displayName }</h1>
        </div>
      }

      { signature(tpl, true) }
      { memberToCommentHtml(tpl, true) }

      <div id="template">

        <div id="mbrsel">
          <div id='textfilter'><span class='pre'/><span class='input'><input type='text' accesskey='/'/></span><span class='post'/></div>
          { if (tpl.linearizationTemplates.isEmpty) NodeSeq.Empty else
              <div id="order">
                <span class="filtertype">Ordering</span>
                <ol><li class="alpha in">Alphabetic</li><li class="inherit out">By inheritance</li></ol>
              </div>
          }
          { if (tpl.linearizationTemplates.isEmpty) NodeSeq.Empty else
              <div id="ancestors">
                <span class="filtertype">Inherited</span>
                <ol><li class="hideall">Hide All</li><li class="showall">Show all</li></ol>
                <ol id="linearization">{ (tpl :: tpl.linearizationTemplates) map { wte => <li class="in" name={ wte.qualifiedName }>{ wte.name }</li> } }</ol>
              </div>
          }
          {
            <div id="visbl">
              <span class="filtertype">Visibility</span>
              <ol><li class="public in">Public</li><li class="all out">All</li></ol>
            </div>
          }
        </div>

        { if (constructors.isEmpty) NodeSeq.Empty else
            <div id="constructors" class="members">
              <h3>Instance Constructors</h3>
              <ol>{ constructors map (memberToHtml(_)) }</ol>
            </div>
        }

        { if (typeMembers.isEmpty) NodeSeq.Empty else
            <div id="types" class="types members">
              <h3>Type Members</h3>
              <ol>{ typeMembers map (memberToHtml(_)) }</ol>
            </div>
        }

        { if (absValueMembers.isEmpty) NodeSeq.Empty else
            <div id="values" class="values members">
              <h3>Abstract Value Members</h3>
              <ol>{ absValueMembers map (memberToHtml(_)) }</ol>
            </div>
        }

        { if (concValueMembers.isEmpty) NodeSeq.Empty else
            <div id="values" class="values members">
              <h3>{ if (absValueMembers.isEmpty) "Value Members" else "Concrete Value Members" }</h3>
              <ol>{ concValueMembers map (memberToHtml(_)) }</ol>
            </div>
        }

        {
          NodeSeq fromSeq (for ((superTpl, superType) <- (tpl.linearizationTemplates zip tpl.linearizationTypes)) yield
            <div class="parent" name={ superTpl.qualifiedName }>
              <h3>Inherited from {
                if (tpl.universe.settings.useStupidTypes.value)
                  superTpl match {
                    case dtpl: DocTemplateEntity =>
                      val sig = signature(dtpl, false, true) \ "_"
                      sig
                    case tpl: TemplateEntity =>
                      tpl.name
                  }
                else
                  typeToHtml(superType, true)
              }</h3>
            </div>
          )
        }

      </div>

      <div id="tooltip" ></div>

    </body>
  }

  def boundsToString(hi: Option[TypeEntity], lo: Option[TypeEntity]): String = {
    def bound0(bnd: Option[TypeEntity], pre: String): String = bnd match {
      case None => ""
      case Some(tpe) => pre ++ tpe.toString
    }
    bound0(hi, "<:") ++ bound0(lo, ">:")
  }

  def tparamsToString(tpss: List[TypeParam]): String = {
    if (tpss.isEmpty) "" else {
      def tparam0(tp: TypeParam): String =
         tp.variance + tp.name + boundsToString(tp.hi, tp.lo)
      def tparams0(tpss: List[TypeParam]): String = (tpss: @unchecked) match {
        case tp :: Nil => tparam0(tp)
        case tp :: tps => tparam0(tp) ++ ", " ++ tparams0(tps)
      }
      "[" + tparams0(tpss) + "]"
    }
  }

  def defParamsToString(d: MemberEntity with Def): String = {
    val paramLists: List[String] =
      if (d.valueParams.isEmpty) Nil
      else d.valueParams map (ps => ps map (_.resultType.name) mkString ("(",",",")"))

    tparamsToString(d.typeParams) + paramLists.mkString
  }

  def memberToHtml(mbr: MemberEntity): NodeSeq = {
    val defParamsString = mbr match {
      case d:MemberEntity with Def => defParamsToString(d)
      case _ => ""
    }
    <li name={ mbr.definitionName } visbl={ if (mbr.visibility.isProtected) "prt" else "pub" }
      data-isabs={ mbr.isAbstract.toString }>
      <a id={ mbr.name +defParamsString +":"+ mbr.resultType.name}/>
      { signature(mbr, false) }
      { memberToCommentHtml(mbr, false) }
    </li>
  }

  def memberToCommentHtml(mbr: MemberEntity, isSelf: Boolean): NodeSeq = {
    mbr match {
      case dte: DocTemplateEntity if isSelf =>
        // comment of class itself
        <xml:group>
          <div id="comment" class="fullcomment">{ memberToCommentBodyHtml(mbr, isSelf = true) }</div>
        </xml:group>
      case dte: DocTemplateEntity if mbr.comment.isDefined =>
        // comment of inner, documented class (only short comment, full comment is on the class' own page)
        memberToInlineCommentHtml(mbr, isSelf)
      case _ =>
        // comment of non-class member or non-documentented inner class
        val commentBody = memberToCommentBodyHtml(mbr, isSelf = false)
        if (commentBody.isEmpty)
          NodeSeq.Empty
        else {
          <xml:group>
            { memberToShortCommentHtml(mbr, isSelf) }
            <div class="fullcomment">{ memberToUseCaseCommentHtml(mbr, isSelf) }{ memberToCommentBodyHtml(mbr, isSelf) }</div>
          </xml:group>
        }
    }
  }

  def memberToUseCaseCommentHtml(mbr: MemberEntity, isSelf: Boolean): NodeSeq = {
    mbr match {
      case nte: NonTemplateMemberEntity if nte.isUseCase =>
        inlineToHtml(comment.Text("[use case] "))
      case _ => NodeSeq.Empty
    }
  }

  def memberToShortCommentHtml(mbr: MemberEntity, isSelf: Boolean): NodeSeq = {
    if (mbr.comment.isEmpty)
      NodeSeq.Empty
    else
      <p class="shortcomment cmt">{ memberToUseCaseCommentHtml(mbr, isSelf) }{ inlineToHtml(mbr.comment.get.short) }</p>
  }

  def memberToInlineCommentHtml(mbr: MemberEntity, isSelf: Boolean): NodeSeq =
    <p class="comment cmt">{ inlineToHtml(mbr.comment.get.short) }</p>

  def memberToCommentBodyHtml(mbr: MemberEntity, isSelf: Boolean, isReduced: Boolean = false): NodeSeq = {
    val memberComment =
      if (mbr.comment.isEmpty) NodeSeq.Empty
      else <div class="comment cmt">{ commentToHtml(mbr.comment) }</div>

    val paramComments = {
      val prs: List[ParameterEntity] = mbr match {
        case cls: Class => cls.typeParams ::: cls.valueParams.flatten
        case trt: Trait => trt.typeParams
        case dfe: Def => dfe.typeParams ::: dfe.valueParams.flatten
        case ctr: Constructor => ctr.valueParams.flatten
        case _ => Nil
      }

      def mbrCmt = mbr.comment.get

      def paramCommentToHtml(prs: List[ParameterEntity]): NodeSeq = prs match {
        case Nil => NodeSeq.Empty

        case (tp: TypeParam) :: rest =>
          val paramEntry: NodeSeq = {
            <dt class="tparam">{ tp.name }</dt><dd class="cmt">{ bodyToHtml(mbrCmt.typeParams(tp.name)) }</dd>
          }
          paramEntry ++ paramCommentToHtml(rest)

        case (vp: ValueParam) :: rest  =>
          val paramEntry: NodeSeq = {
            <dt class="param">{ vp.name }</dt><dd class="cmt">{ bodyToHtml(mbrCmt.valueParams(vp.name)) }</dd>
          }
          paramEntry ++ paramCommentToHtml(rest)
      }

      if (mbr.comment.isEmpty) NodeSeq.Empty
      else {
        val cmtedPrs = prs filter {
          case tp: TypeParam => mbrCmt.typeParams isDefinedAt tp.name
          case vp: ValueParam => mbrCmt.valueParams isDefinedAt vp.name
        }
        if (cmtedPrs.isEmpty && mbrCmt.result.isEmpty) NodeSeq.Empty
        else {
          <dl class="paramcmts block">{
            paramCommentToHtml(cmtedPrs) ++ (
            mbrCmt.result match {
              case None => NodeSeq.Empty
              case Some(cmt) =>
                <dt>returns</dt><dd class="cmt">{ bodyToHtml(cmt) }</dd>
            })
          }</dl>
        }
      }
    }

    // --- start attributes block vals
    val attributes: Seq[scala.xml.Node] = {
      val fvs: List[comment.Paragraph] = visibility(mbr).toList ::: mbr.flags
      if (fvs.isEmpty || isReduced) NodeSeq.Empty
      else {
        <dt>Attributes</dt>
        <dd>{ fvs map { fv => { inlineToHtml(fv.text) ++ xml.Text(" ") } } }</dd>
      }
    }

    val definitionClasses: Seq[scala.xml.Node] = {
      val inDefTpls = mbr.inDefinitionTemplates
      if ((inDefTpls.tail.isEmpty && (inDefTpls.head == mbr.inTemplate)) || isReduced) NodeSeq.Empty
      else {
        <dt>Definition Classes</dt>
        <dd>{ templatesToHtml(inDefTpls, xml.Text(" → ")) }</dd>
      }
    }

    val selfType: Seq[scala.xml.Node] = mbr match {
      case dtpl: DocTemplateEntity if (isSelf && !dtpl.selfType.isEmpty && !isReduced) =>
        <dt>Self Type</dt>
        <dd>{ typeToHtml(dtpl.selfType.get, hasLinks = true) }</dd>
      case _ => NodeSeq.Empty
    }

    val annotations: Seq[scala.xml.Node] =
      if (!mbr.annotations.isEmpty) {
        <dt>Annotations</dt>
        <dd>{
            mbr.annotations.map { annot =>
              <xml:group>
                <span class="name">@{ templateToHtml(annot.annotationClass) }</span>{ argumentsToHtml(annot.arguments) }
              </xml:group>
            }
          }
        </dd>
      } else NodeSeq.Empty

    val sourceLink: Seq[scala.xml.Node] = mbr match {
      case dtpl: DocTemplateEntity if (isSelf && dtpl.sourceUrl.isDefined && dtpl.inSource.isDefined && !isReduced) =>
        val (absFile, line) = dtpl.inSource.get
        <dt>Source</dt>
        <dd>{ <a href={ dtpl.sourceUrl.get.toString }>{ Text(absFile.file.getName) }</a> }</dd>
      case _ => NodeSeq.Empty
    }

    val deprecation: Seq[scala.xml.Node] =
      if (mbr.deprecation.isEmpty || isReduced) NodeSeq.Empty
      else {
        <dt>Deprecated</dt>
        <dd class="cmt">{ bodyToHtml(mbr.deprecation.get) }</dd>
      }

    val mainComment: Seq[scala.xml.Node] = mbr.comment match {
      case Some(comment) =>
        val example =
          if(!comment.example.isEmpty && !isReduced)
            <div class="block">Example{ if (comment.example.length > 1) "s" else ""} :
                <ol>{
                val exampleXml: List[scala.xml.NodeSeq] =
                  for(example <- comment.example ) yield
                    <li class="cmt">{ bodyToHtml(example) }</li>
                exampleXml.reduceLeft(_ ++ Text(", ") ++ _)
              }</ol>
              </div>
          else NodeSeq.Empty

        val version: Seq[scala.xml.Node] =
          if(!comment.version.isEmpty && !isReduced) {
            <dt>Version</dt>
            <dd>{ for(body <- comment.version.toList) yield {bodyToHtml(body)} }</dd>
          } else NodeSeq.Empty

        val sinceVersion: Seq[scala.xml.Node] =
          if(!comment.since.isEmpty && !isReduced) {
            <dt>Since</dt>
            <dd>{ for(body <- comment.since.toList) yield {bodyToHtml(body)} }</dd>
          } else NodeSeq.Empty

        val seeAlso: Seq[scala.xml.Node] =
          if(!comment.see.isEmpty && !isReduced) {
            <dt>See also</dt>
            <dd>{
              val seeXml:List[scala.xml.NodeSeq]=(for(see <- comment.see ) yield <span class="cmt">{bodyToHtml(see)}</span> )
              seeXml.reduceLeft(_ ++ Text(", ") ++ _)
            }</dd>
          } else NodeSeq.Empty

        <xml:group> {
          example ++ version ++ sinceVersion ++ seeAlso
        } </xml:group>

      case None => NodeSeq.Empty
    }
    // end attributes block vals ---

    val attributesInfo = attributes ++ definitionClasses ++ selfType ++ annotations ++ sourceLink ++ deprecation ++ mainComment
    val attributesBlock =
      if (attributesInfo.isEmpty)
        NodeSeq.Empty
      else
        <dl class="attributes block"> { attributesInfo }</dl>

    val linearization =
      mbr match {
        case dtpl: DocTemplateEntity if isSelf && !isReduced && (dtpl.linearizationTemplates.nonEmpty || dtpl.subClasses.nonEmpty) =>
          val linearSupertypes: Seq[scala.xml.Node] =
            if (dtpl.linearizationTemplates.isEmpty) NodeSeq.Empty
            else {
              <h1>Linear Supertypes</h1>
              <p>{ typesToHtml(dtpl.linearizationTypes, hasLinks = true, sep = xml.Text(", ")) }</p>
            }

          val knownSubclasses: Seq[scala.xml.Node] =
            if (dtpl.subClasses.isEmpty) NodeSeq.Empty
            else {
              <h1>Known Subclasses</h1>
              <p>{ templatesToHtml(dtpl.subClasses, xml.Text(", ")) }</p>
            }

          <div id="superTypesDiv">
            <div class="attributes block">
              <span class="link showElement">Show linear super types and known subclasses</span>
              <span class="link hideElement">Hide linear super types and known subclasses</span>
            </div>
            <div class="superTypes hiddenContent">
              <xml:group> {
                linearSupertypes ++ knownSubclasses
              } </xml:group>
            </div>
          </div>
        case _ => NodeSeq.Empty
      }

    NodeSeq.Empty ++ memberComment ++ paramComments ++ attributesBlock ++ linearization

  }

  def kindToString(mbr: MemberEntity): String = {
    mbr match {
      case tpl: DocTemplateEntity => docEntityKindToString(tpl)
      case ctor: Constructor => "new"
      case tme: MemberEntity =>
        ( if (tme.isImplicit) "implicit " else "" ) +
        ( if (tme.isDef) "def"
          else if (tme.isVal) "val"
          else if (tme.isLazyVal) "lazy val"
          else if (tme.isVar) "var"
          else "type")
    }
  }

  def boundsToHtml(hi: Option[TypeEntity], lo: Option[TypeEntity], hasLinks: Boolean): NodeSeq = {
    def bound0(bnd: Option[TypeEntity], pre: String): NodeSeq = bnd match {
      case None => NodeSeq.Empty
      case Some(tpe) => xml.Text(pre) ++ typeToHtml(tpe, hasLinks)
    }
    bound0(lo, " >: ") ++ bound0(hi, " <: ")
  }

  def visibility(mbr: MemberEntity): Option[comment.Paragraph] = {
    import comment._
    import comment.{ Text => CText }
    mbr.visibility match {
      case PrivateInInstance() =>
        Some(Paragraph(CText("private[this]")))
      case PrivateInTemplate(owner) if (owner == mbr.inTemplate) =>
        Some(Paragraph(CText("private")))
      case PrivateInTemplate(owner) =>
        Some(Paragraph(Chain(List(CText("private["), EntityLink(owner), CText("]")))))
      case ProtectedInInstance() =>
        Some(Paragraph(CText("protected[this]")))
      case ProtectedInTemplate(owner) if (owner == mbr.inTemplate) =>
        Some(Paragraph(CText("protected")))
      case ProtectedInTemplate(owner) =>
        Some(Paragraph(Chain(List(CText("protected["), EntityLink(owner), CText("]")))))
      case Public() =>
        None
    }
  }

  /** name, tparams, params, result */
  def signature(mbr: MemberEntity, isSelf: Boolean, isReduced: Boolean = false): NodeSeq = {
    def inside(hasLinks: Boolean, nameLink: String = ""): NodeSeq =
      <xml:group>
      <span class="kind">{ kindToString(mbr) }</span>
      <span class="symbol">
        {
          val nameHtml = <span class={"name" + (if (mbr.deprecation.isDefined) " deprecated" else "") }>{ if (mbr.isConstructor) tpl.name else mbr.name }</span>
          if (!nameLink.isEmpty)
            <a href={nameLink}>{nameHtml}</a>
          else nameHtml
        }
        {
          def tparamsToHtml(mbr: Entity): NodeSeq = mbr match {
            case hk: HigherKinded =>
              val tpss = hk.typeParams
              if (tpss.isEmpty) NodeSeq.Empty else {
                def tparam0(tp: TypeParam): NodeSeq =
                  <span name={ tp.name }>{ tp.variance + tp.name }{ tparamsToHtml(tp) }{ boundsToHtml(tp.hi, tp.lo, hasLinks)}</span>
                def tparams0(tpss: List[TypeParam]): NodeSeq = (tpss: @unchecked) match {
                  case tp :: Nil => tparam0(tp)
                  case tp :: tps => tparam0(tp) ++ Text(", ") ++ tparams0(tps)
                }
                <span class="tparams">[{ tparams0(tpss) }]</span>
              }
              case _ => NodeSeq.Empty
          }
          tparamsToHtml(mbr)
        }
        { if (isReduced) NodeSeq.Empty else {
            def paramsToHtml(vlsss: List[List[ValueParam]]): NodeSeq = {
              def param0(vl: ValueParam): NodeSeq =
                // notice the }{ in the next lines, they are necessary to avoid a undesired withspace in output
                <span name={ vl.name }>{
                  Text(vl.name)
                }{ Text(": ") ++ typeToHtml(vl.resultType, hasLinks) }{
                  vl.defaultValue match {
                    case Some(v) => Text(" = ") ++ treeToHtml(v)
                    case None => NodeSeq.Empty
                  }
                }</span>

              def params0(vlss: List[ValueParam]): NodeSeq = vlss match {
                case Nil => NodeSeq.Empty
                case vl :: Nil => param0(vl)
                case vl :: vls => param0(vl) ++ Text(", ") ++ params0(vls)
              }
              def implicitCheck(vlss: List[ValueParam]): NodeSeq = vlss match {
                case vl :: vls => if(vl.isImplicit) { <span class="implicit">implicit </span> } else Text("")
                case _ => Text("")
              }
              vlsss map { vlss => <span class="params">({implicitCheck(vlss) ++ params0(vlss) })</span> }
            }
            mbr match {
              case cls: Class => paramsToHtml(cls.valueParams)
              case ctr: Constructor => paramsToHtml(ctr.valueParams)
              case dfe: Def => paramsToHtml(dfe.valueParams)
              case _ => NodeSeq.Empty
            }
          }
        }{ if (isReduced) NodeSeq.Empty else {
          mbr match {
            case tpl: DocTemplateEntity if tpl.parentType.isDefined =>
              <span class="result"> extends { typeToHtml(tpl.parentType.get, hasLinks) }</span>

            case tme: MemberEntity if (tme.isDef || tme.isVal || tme.isLazyVal || tme.isVar) =>
              <span class="result">: { typeToHtml(tme.resultType, hasLinks) }</span>

            case abt: AbstractType =>
              val b2s = boundsToHtml(abt.hi, abt.lo, hasLinks)
              if (b2s != NodeSeq.Empty)
                <span class="result">{ b2s }</span>
              else NodeSeq.Empty

            case alt: AliasType =>
              <span class="result"> = { typeToHtml(alt.alias, hasLinks) }</span>
            case _ => NodeSeq.Empty
          }
        }}
      </span>
      </xml:group>
    mbr match {
      case dte: DocTemplateEntity if !isSelf =>
        <h4 class="signature">{ inside(hasLinks = false, nameLink = relativeLinkTo(dte)) }</h4>
      case _ if isSelf =>
        <h4 id="signature" class="signature">{ inside(hasLinks = true) }</h4>
      case _ =>
        <h4 class="signature">{ inside(hasLinks = true) }</h4>
    }

  }

  /** */
  def treeToHtml(tree: TreeEntity): NodeSeq = {

    /** Makes text good looking in the html page : newlines and basic indentation,
     * You must change this function if you want to improve pretty printing of default Values
     */
    def codeStringToXml(text: String): NodeSeq = {
      var goodLookingXml: NodeSeq = NodeSeq.Empty
      var indent = 0
      for(c<-text) c match {
        case '{' => indent+=1
          goodLookingXml ++= Text("{")
        case '}' => indent-=1
          goodLookingXml ++= Text("}")
        case '\n' =>
          goodLookingXml++= <br/> ++ indentation
        case _ => goodLookingXml ++= Text(c.toString)
      }
      def indentation:NodeSeq = {
        var indentXml = NodeSeq.Empty
        for (x <- 1 to indent) indentXml ++= Text("&nbsp;&nbsp;")
        indentXml
      }
      goodLookingXml
    }

    var index = 0
    val str = tree.expression
    val length = str.length
    var myXml: NodeSeq = NodeSeq.Empty
    for( (from, (member, to)) <- tree.refEntity.toSeq) {
      if (index < from) {
        myXml ++= codeStringToXml(str.substring(index,from))
        index = from
      }
      if (index == from) {
        member match {
          case mbr: DocTemplateEntity =>
            val link = relativeLinkTo(mbr)
            myXml ++= <span class="name"><a href={link}>{str.substring(from, to)}</a></span>
          case mbr: MemberEntity =>
            val anchor = "#" + mbr.name + defParamsString(mbr) + ":" + mbr.resultType.name
            val link = relativeLinkTo(mbr.inTemplate)
            myXml ++= <span class="name"><a href={link ++ anchor}>{str.substring(from, to)}</a></span>
        }
        index = to
      }
    }
    // function used in the MemberEntity case above
    def defParamsString(mbr: Entity):String = mbr match {
      case d:MemberEntity with Def => defParamsToString(d)
      case _ => ""
    }

    if (index <= length-1)
      myXml ++= codeStringToXml(str.substring(index, length ))

    if(length < 36)
      <span class="symbol">{ myXml }</span>
    else
      <span class="defval" name={ myXml }>{ "..." }</span>
  }

  def argumentsToHtml(argss: List[ValueArgument]): NodeSeq = {
    def argumentsToHtml0(argss: List[ValueArgument]): NodeSeq = argss match {
      case Nil         => NodeSeq.Empty
      case arg :: Nil  => argumentToHtml(arg)
      case arg :: args => argumentToHtml(arg) ++ xml.Text(", ") ++ argumentsToHtml0(args)
    }
    <span class="args">({ argumentsToHtml0(argss) })</span>
  }

  def argumentToHtml(arg: ValueArgument): NodeSeq = {
    <span>
      {
        arg.parameter match {
          case Some(param) => Text(param.name + " = ")
          case None => NodeSeq.Empty
        }
      }
      { treeToHtml(arg.value) }
    </span>
  }

}
