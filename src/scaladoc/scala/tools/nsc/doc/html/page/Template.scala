/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala
package tools
package nsc
package doc
package html
package page

import base._
import base.comment._

import model._
import model.diagram._
import scala.xml.{Elem, NodeSeq, Text, UnprefixedAttribute}
import scala.language.postfixOps
import scala.collection.mutable. { Set, HashSet }

import model._
import model.diagram._
import diagram._

class Template(universe: doc.Universe, generator: DiagramGenerator, tpl: DocTemplateEntity) extends HtmlPage {

  val path =
    templateToPath(tpl)

  def title = {
    val s = universe.settings

    tpl.name +
    ( if (!s.doctitle.isDefault) " - " + s.doctitle.value else "" ) +
    ( if (!s.docversion.isDefault) (" " + s.docversion.value) else "" ) +
    " - " + tpl.qualifiedName
  }

  val headers =
    <xml:group>
      <link href={ relativeLinkTo{List("template.css", "lib")} } media="screen" type="text/css" rel="stylesheet"/>
      <link href={ relativeLinkTo{List("diagrams.css", "lib")} } media="screen" type="text/css" rel="stylesheet" id="diagrams-css" />
      <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} } id="jquery-js"></script>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery-ui.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("template.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("tools.tooltip.js", "lib")} }></script>
      { if (universe.settings.docDiagrams.value) {
      <script type="text/javascript" src={ relativeLinkTo{List("modernizr.custom.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("diagrams.js", "lib")} } id="diagrams-js"></script>
      } else NodeSeq.Empty }
      <script type="text/javascript">
         if(top === self) {{
            var url = '{ val p = templateToPath(tpl); "../" * (p.size - 1) + "index.html" }';
            var hash = '{ val p = templateToPath(tpl); (p.tail.reverse ::: List(p.head.replace(".html", ""))).mkString(".") }';
            var anchor = window.location.hash;
            var anchor_opt = '';
            if (anchor.length { scala.xml.Unparsed(">=") /* unless we use Unparsed, it gets escaped and crashes the script */ } 1)
              anchor_opt = '@' + anchor.substring(1);
            window.location.href = url + '#' + hash + anchor_opt;
         }}
   	  </script>
    </xml:group>

  val valueMembers =
    tpl.methods ++ tpl.values ++ tpl.templates.filter(x => x.isObject || x.isPackage) sorted

  val (absValueMembers, nonAbsValueMembers) =
    valueMembers partition (_.isAbstract)

  val (deprValueMembers, nonDeprValueMembers) =
    nonAbsValueMembers partition (_.deprecation.isDefined)

  val (concValueMembers, shadowedImplicitMembers) =
    nonDeprValueMembers partition (!_.isShadowedOrAmbiguousImplicit)

  val typeMembers =
    tpl.abstractTypes ++ tpl.aliasTypes ++ tpl.templates.filter(x => x.isTrait || x.isClass) sorted (implicitly[Ordering[MemberEntity]])

  val constructors = (tpl match {
    case cls: Class => (cls.constructors: List[MemberEntity]).sorted
    case _ => Nil
  })

  /* for body, there is a special case for AnyRef, otherwise AnyRef appears
   * like a package/object this problem should be fixed, this implementation
   * is just a patch. */
  val body = {
    val templateName = if (tpl.isRootPackage) "root package" else tpl.name
    val displayName = tpl.companion match {
      case Some(companion) if (companion.visibility.isPublic && companion.inSource != None) =>
        <a href={relativeLinkTo(companion)} title={docEntityKindToCompanionTitle(tpl)}>{ templateName }</a>
      case _ =>
        templateName
    }
    val owner = {
      if (tpl.isRootPackage || tpl.inTemplate.isRootPackage)
        NodeSeq.Empty
      else
        <p id="owner">{ templatesToHtml(tpl.inTemplate.toRoot.reverse.tail, scala.xml.Text(".")) }</p>
    }

    <body class={ if (tpl.isType) "type" else "value" }>
      <div id="definition">
        {
          val (src, alt) = docEntityKindToBigImage(tpl)

          tpl.companion match {
            case Some(companion) if (companion.visibility.isPublic && companion.inSource != None) =>
              <a href={relativeLinkTo(companion)} title={docEntityKindToCompanionTitle(tpl)}><img alt={alt} src={ relativeLinkTo(List(src, "lib")) }/></a>
            case _ =>
              <img alt={alt} src={ relativeLinkTo(List(src, "lib")) }/>
        }}
        { owner }
        <h1>{ displayName }</h1>{
          if (tpl.isPackage) NodeSeq.Empty else <h3>{companionAndPackage(tpl)}</h3>
        }{ permalink(tpl) }
      </div>

      { signature(tpl, isSelf = true) }
      { memberToCommentHtml(tpl, tpl.inTemplate, isSelf = true) }

      <div id="mbrsel">
        <div id='textfilter'><span class='pre'/><span class='input'><input id='mbrsel-input' type='text' accesskey='/'/></span><span class='post'/></div>
        { if (tpl.linearizationTemplates.isEmpty && tpl.conversions.isEmpty && (!universe.settings.docGroups.value || (tpl.members.map(_.group).distinct.length == 1)))
            NodeSeq.Empty
          else
            <div id="order">
              <span class="filtertype">Ordering</span>
              <ol>
                {
                  if (!universe.settings.docGroups.value || (tpl.members.map(_.group).distinct.length == 1))
                    NodeSeq.Empty
                  else
                    <li class="group out"><span>Grouped</span></li>
                }
                <li class="alpha in"><span>Alphabetic</span></li>
                {
                  if (tpl.linearizationTemplates.isEmpty && tpl.conversions.isEmpty)
                    NodeSeq.Empty
                  else
                    <li class="inherit out"><span>By Inheritance</span></li>
                }
              </ol>
            </div>
        }
        { if (tpl.linearizationTemplates.isEmpty && tpl.conversions.isEmpty) NodeSeq.Empty else
          {
            if (!tpl.linearizationTemplates.isEmpty)
              <div id="ancestors">
                <span class="filtertype">Inherited<br/>
                </span>
                <ol id="linearization">
                  { (tpl :: tpl.linearizationTemplates).map(wte => <li class="in" name={ wte.qualifiedName }><span>{ wte.name }</span></li>) }
                </ol>
              </div>
            else NodeSeq.Empty
          } ++ {
            if (!tpl.conversions.isEmpty)
              <div id="ancestors">
                <span class="filtertype">Implicitly<br/>
                </span>
                <ol id="implicits"> {
                  tpl.conversions.map { conv =>
                    val name = conv.conversionQualifiedName
                    val hide = universe.settings.hiddenImplicits(name)
                    <li class="in" name={ name } data-hidden={ hide.toString }><span>{ "by " + conv.conversionShortName }</span></li>
                  }
                }
                </ol>
              </div>
            else NodeSeq.Empty
          } ++
          <div id="ancestors">
            <span class="filtertype"></span>
            <ol>
              <li class="hideall out"><span>Hide All</span></li>
              <li class="showall in"><span>Show All</span></li>
            </ol>
          </div>
        }
        {
          <div id="visbl">
            <span class="filtertype">Visibility</span>
            <ol><li class="public in"><span>Public</span></li><li class="all out"><span>All</span></li></ol>
          </div>
        }
      </div>

      <div id="template">
        <div id="allMembers">
        { if (constructors.isEmpty) NodeSeq.Empty else
            <div id="constructors" class="members">
              <h3>Instance Constructors</h3>
              <ol>{ constructors map (memberToHtml(_, tpl)) }</ol>
            </div>
        }

        { if (typeMembers.isEmpty) NodeSeq.Empty else
            <div id="types" class="types members">
              <h3>Type Members</h3>
              <ol>{ typeMembers map (memberToHtml(_, tpl)) }</ol>
            </div>
        }

        { if (absValueMembers.isEmpty) NodeSeq.Empty else
            <div id="values" class="values members">
              <h3>Abstract Value Members</h3>
              <ol>{ absValueMembers map (memberToHtml(_, tpl)) }</ol>
            </div>
        }

        { if (concValueMembers.isEmpty) NodeSeq.Empty else
            <div id="values" class="values members">
              <h3>{ if (absValueMembers.isEmpty) "Value Members" else "Concrete Value Members" }</h3>
              <ol>{ concValueMembers map (memberToHtml(_, tpl)) }</ol>
            </div>
        }

        { if (shadowedImplicitMembers.isEmpty) NodeSeq.Empty else
            <div id="values" class="values members">
              <h3>Shadowed Implicit Value Members</h3>
              <ol>{ shadowedImplicitMembers map (memberToHtml(_, tpl)) }</ol>
            </div>
        }

        { if (deprValueMembers.isEmpty) NodeSeq.Empty else
            <div id="values" class="values members">
              <h3>Deprecated Value Members</h3>
              <ol>{ deprValueMembers map (memberToHtml(_, tpl)) }</ol>
            </div>
        }
        </div>

        <div id="inheritedMembers">
        {
          // linearization
          NodeSeq fromSeq (for ((superTpl, superType) <- (tpl.linearizationTemplates zip tpl.linearizationTypes)) yield
            <div class="parent" name={ superTpl.qualifiedName }>
              <h3>Inherited from {
                typeToHtmlWithStupidTypes(tpl, superTpl, superType)
              }</h3>
            </div>
          )
        }
        {
          // implicitly inherited
          NodeSeq fromSeq (for (conversion <- (tpl.conversions)) yield
            <div class="conversion" name={ conversion.conversionQualifiedName }>
              <h3>Inherited by implicit conversion { conversion.conversionShortName } from
                { typeToHtml(tpl.resultType, hasLinks = true) } to { typeToHtml(conversion.targetType, hasLinks = true) }
              </h3>
            </div>
          )
        }
        </div>

        <div id="groupedMembers">
        {
          val allGroups = tpl.members.map(_.group).distinct
          val orderedGroups = allGroups.map(group => (tpl.groupPriority(group), group)).sorted.map(_._2)
          // linearization
          NodeSeq fromSeq (for (group <- orderedGroups) yield
            <div class="group" name={ group }>
              <h3>{ tpl.groupName(group) }</h3>
              {
                tpl.groupDescription(group) match {
                  case Some(body) => <div class="comment cmt">{ bodyToHtml(body) }</div>
                  case _ => NodeSeq.Empty
                }
              }
            </div>
          )
        }
        </div>

      </div>

      <div id="tooltip" ></div>

      {
        if (Set("epfl", "EPFL").contains(tpl.universe.settings.docfooter.value))
          <div id="footer">Scala programming documentation. Copyright (c) 2003-2017 <a href="http://www.epfl.ch" target="_top">EPFL</a>, with contributions from <a href="http://www.lightbend.com" target="_top">Lightbend</a>.</div>
        else
          <div id="footer"> { tpl.universe.settings.docfooter.value } </div>
      }


    </body>
  }

  def memberToHtml(mbr: MemberEntity, inTpl: DocTemplateEntity): NodeSeq = {
    val memberComment = memberToCommentHtml(mbr, inTpl, isSelf = false)
    <li name={ mbr.definitionName } visbl={ if (mbr.visibility.isProtected) "prt" else "pub" }
      data-isabs={ mbr.isAbstract.toString }
      fullComment={ if(memberComment.filter(_.label=="div").isEmpty) "no" else "yes" }
      group={ mbr.group }>
      <a id={ mbr.signature }/>
      <a id={ mbr.signatureCompat }/>
      { signature(mbr, isSelf = false) }
      { memberComment }
    </li>
  }

  def memberToCommentHtml(mbr: MemberEntity, inTpl: DocTemplateEntity, isSelf: Boolean): NodeSeq = {
    mbr match {
      case dte: DocTemplateEntity if isSelf =>
        // comment of class itself
        <xml:group>
          <div id="comment" class="fullcommenttop">{ memberToCommentBodyHtml(mbr, inTpl, isSelf = true) }</div>
        </xml:group>
      case _ =>
        // comment of non-class member or non-documentented inner class
        val commentBody = memberToCommentBodyHtml(mbr, inTpl, isSelf = false)
        if (commentBody.isEmpty)
          NodeSeq.Empty
        else {
          val shortComment = memberToShortCommentHtml(mbr, isSelf)
          val longComment = memberToUseCaseCommentHtml(mbr, isSelf) ++ memberToCommentBodyHtml(mbr, inTpl, isSelf)

          val includedLongComment = if (shortComment.text.trim == longComment.text.trim)
            NodeSeq.Empty
          else
            <div class="fullcomment">{ longComment }</div>

          shortComment ++ includedLongComment
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

  def memberToShortCommentHtml(mbr: MemberEntity, isSelf: Boolean): NodeSeq =
    mbr.comment.fold(NodeSeq.Empty) { comment =>
      <p class="shortcomment cmt">{ memberToUseCaseCommentHtml(mbr, isSelf) }{ inlineToHtml(comment.short) }</p>
    }

  def memberToInlineCommentHtml(mbr: MemberEntity, isSelf: Boolean): NodeSeq =
    <p class="comment cmt">{ inlineToHtml(mbr.comment.get.short) }</p>

  def memberToCommentBodyHtml(mbr: MemberEntity, inTpl: DocTemplateEntity, isSelf: Boolean, isReduced: Boolean = false): NodeSeq = {
    val s = universe.settings

    val memberComment =
      if (mbr.comment.isEmpty) NodeSeq.Empty
      else <div class="comment cmt">{ commentToHtml(mbr.comment) }</div>

    val authorComment =
      if (! s.docAuthor || mbr.comment.isEmpty ||
        mbr.comment.isDefined && mbr.comment.get.authors.isEmpty) NodeSeq.Empty
      else <div class="comment cmt">
        {if (mbr.comment.get.authors.size > 1) <h6>Authors:</h6> else <h6>Author:</h6>}
        { mbr.comment.get.authors map bodyToHtml}
      </div>

    val paramComments = {
      val prs: List[ParameterEntity] = mbr match {
        case cls: Class => cls.typeParams ::: cls.valueParams.flatten
        case trt: Trait => trt.typeParams
        case dfe: Def => dfe.typeParams ::: dfe.valueParams.flatten
        case ctr: Constructor => ctr.valueParams.flatten
        case _ => Nil
      }

      def paramCommentToHtml(prs: List[ParameterEntity], comment: Comment): NodeSeq = prs match {

        case (tp: TypeParam) :: rest =>
          val paramEntry: NodeSeq = {
            <dt class="tparam">{ tp.name }</dt><dd class="cmt">{ bodyToHtml(comment.typeParams(tp.name)) }</dd>
          }
          paramEntry ++ paramCommentToHtml(rest, comment)

        case (vp: ValueParam) :: rest  =>
          val paramEntry: NodeSeq = {
            <dt class="param">{ vp.name }</dt><dd class="cmt">{ bodyToHtml(comment.valueParams(vp.name)) }</dd>
          }
          paramEntry ++ paramCommentToHtml(rest, comment)

        case _ =>
          NodeSeq.Empty
      }

      mbr.comment.fold(NodeSeq.Empty) { comment =>
        val cmtedPrs = prs filter {
          case tp: TypeParam => comment.typeParams isDefinedAt tp.name
          case vp: ValueParam => comment.valueParams isDefinedAt vp.name
        }
        if (cmtedPrs.isEmpty && comment.result.isEmpty) NodeSeq.Empty
        else {
          <dl class="paramcmts block">{
            paramCommentToHtml(cmtedPrs, comment) ++ (
            comment.result match {
              case None => NodeSeq.Empty
              case Some(cmt) =>
                <dt>returns</dt><dd class="cmt">{ bodyToHtml(cmt) }</dd>
            })
          }</dl>
        }
      }
    }

    val implicitInformation = mbr.byConversion match {
      case Some(conv) =>
        <dt class="implicit">Implicit information</dt> ++
        {
          val targetType = typeToHtml(conv.targetType, hasLinks = true)
          val conversionMethod = conv.convertorMethod match {
            case Left(member) => Text(member.name)
            case Right(name)  => Text(name)
          }

          // strip off the package object endings, they make things harder to follow
          val conversionOwnerQualifiedNane = conv.convertorOwner.qualifiedName.stripSuffix(".package")
          val conversionOwner = templateToHtml(conv.convertorOwner, conversionOwnerQualifiedNane)

          val constraintText = conv.constraints match {
            case Nil =>
              NodeSeq.Empty
            case List(constraint) =>
              scala.xml.Text("This conversion will take place only if ") ++ constraintToHtml(constraint) ++ scala.xml.Text(".")
            case List(constraint1, constraint2) =>
              scala.xml.Text("This conversion will take place only if ") ++ constraintToHtml(constraint1) ++
                scala.xml.Text(" and at the same time ") ++ constraintToHtml(constraint2) ++ scala.xml.Text(".")
            case constraints =>
              <br/> ++ "This conversion will take place only if all of the following constraints are met:" ++ <br/> ++ {
                var index = 0
                constraints map { constraint => scala.xml.Text({ index += 1; index } + ". ") ++ constraintToHtml(constraint) ++ <br/> }
              }
          }

          <dd>
            This member is added by an implicit conversion from { typeToHtml(inTpl.resultType, hasLinks = true) } to
            { targetType } performed by method { conversionMethod } in { conversionOwner }.
            { constraintText }
          </dd>
        } ++ {
          if (mbr.isShadowedOrAmbiguousImplicit) {
            // These are the members that are shadowing or ambiguating the current implicit
            // see ImplicitMemberShadowing trait for more information
            val shadowingSuggestion = {
              val params = mbr match {
                case d: Def => d.valueParams map (_ map (_ name) mkString("(", ", ", ")")) mkString
                case _      => "" // no parameters
              }
              <br/> ++ scala.xml.Text("To access this member you can use a ") ++
              <a href="http://stackoverflow.com/questions/2087250/what-is-the-purpose-of-type-ascription-in-scala"
                target="_blank">type ascription</a> ++ scala.xml.Text(":") ++
              <br/> ++ <div class="cmt"><pre>{"(" + Template.lowerFirstLetter(tpl.name) + ": " + conv.targetType.name + ")." + mbr.name + params }</pre></div>
            }

            val shadowingWarning: NodeSeq =
              if (mbr.isShadowedImplicit)
                  scala.xml.Text("This implicitly inherited member is shadowed by one or more members in this " +
                  "class.") ++ shadowingSuggestion
              else if (mbr.isAmbiguousImplicit)
                  scala.xml.Text("This implicitly inherited member is ambiguous. One or more implicitly " +
                  "inherited members have similar signatures, so calling this member may produce an ambiguous " +
                  "implicit conversion compiler error.") ++ shadowingSuggestion
              else NodeSeq.Empty

            <dt class="implicit">Shadowing</dt> ++
            <dd>{ shadowingWarning }</dd>

          } else NodeSeq.Empty
        }
      case _ =>
        NodeSeq.Empty
    }

    // --- start attributes block vals
    val attributes: NodeSeq = {
      val fvs: List[comment.Paragraph] = visibility(mbr).toList
      if (fvs.isEmpty || isReduced) NodeSeq.Empty
      else {
        <dt>Attributes</dt>
        <dd>{ fvs map { fv => { inlineToHtml(fv.text) ++ scala.xml.Text(" ") } } }</dd>
      }
    }

    val definitionClasses: NodeSeq = {
      val inDefTpls = mbr.inDefinitionTemplates
      if ((inDefTpls.tail.isEmpty && (inDefTpls.head == inTpl)) || isReduced) NodeSeq.Empty
      else {
        <dt>Definition Classes</dt>
        <dd>{ templatesToHtml(inDefTpls, scala.xml.Text(" → ")) }</dd>
      }
    }

    val fullSignature: NodeSeq = {
      mbr match {
        case nte: NonTemplateMemberEntity if nte.isUseCase =>
          <div class="full-signature-block toggleContainer">
            <span class="toggle">Full Signature</span>
            <div class="hiddenContent full-signature-usecase">{ signature(nte.useCaseOf.get,isSelf = true) }</div>
          </div>
        case _ => NodeSeq.Empty
      }
    }

    val selfType: NodeSeq = mbr match {
      case dtpl: DocTemplateEntity if (isSelf && !dtpl.selfType.isEmpty && !isReduced) =>
        <dt>Self Type</dt>
        <dd>{ typeToHtml(dtpl.selfType.get, hasLinks = true) }</dd>
      case _ => NodeSeq.Empty
    }

    val annotations: NodeSeq = {
      // A list of annotations which don't show their arguments, e. g. because they are shown separately.
      val annotationsWithHiddenArguments = List("deprecated", "Deprecated", "migration")

      def showArguments(annotation: Annotation) =
        !(annotationsWithHiddenArguments.contains(annotation.qualifiedName))

      if (!mbr.annotations.isEmpty) {
        <dt>Annotations</dt>
        <dd>{
            mbr.annotations.map { annot =>
              <xml:group>
                <span class="name">@{ templateToHtml(annot.annotationClass) }</span>{
                  if (showArguments(annot)) argumentsToHtml(annot.arguments) else NodeSeq.Empty
                }
              </xml:group>
            }
          }
        </dd>
      } else NodeSeq.Empty
    }

    val sourceLink: NodeSeq = mbr match {
      case dtpl: DocTemplateEntity if (isSelf && dtpl.sourceUrl.isDefined && dtpl.inSource.isDefined && !isReduced) =>
        val (absFile, _) = dtpl.inSource.get
        <dt>Source</dt>
        <dd>{ <a href={ dtpl.sourceUrl.get.toString } target="_blank">{ Text(absFile.file.getName) }</a> }</dd>
      case _ => NodeSeq.Empty
    }

    val deprecation: NodeSeq =
      mbr.deprecation match {
        case Some(deprecation) if !isReduced =>
          <dt>Deprecated</dt>
          <dd class="cmt">{ bodyToHtml(deprecation) }</dd>
        case _ => NodeSeq.Empty
      }

    val migration: NodeSeq =
      mbr.migration match {
        case Some(migration) if !isReduced =>
          <dt>Migration</dt>
          <dd class="cmt">{ bodyToHtml(migration) }</dd>
        case _ => NodeSeq.Empty
      }

    val mainComment: NodeSeq = mbr.comment match {
      case Some(comment) if (! isReduced) =>
        def orEmpty[T](it: Iterable[T])(gen:  =>NodeSeq): NodeSeq =
          if (it.isEmpty) NodeSeq.Empty else gen

        val example =
          orEmpty(comment.example) {
            <div class="block">Example{ if (comment.example.length > 1) "s" else ""}:
               <ol>{
                 val exampleXml: List[NodeSeq] = for (ex <- comment.example) yield
                   <li class="cmt">{ bodyToHtml(ex) }</li>
                 exampleXml.reduceLeft(_ ++ Text(", ") ++ _)
              }</ol>
            </div>
	  }

        val version: NodeSeq =
          orEmpty(comment.version) {
            <dt>Version</dt>
            <dd>{ for(body <- comment.version.toList) yield bodyToHtml(body) }</dd>
          }

        val sinceVersion: NodeSeq =
          orEmpty(comment.since) {
            <dt>Since</dt>
            <dd>{ for(body <- comment.since.toList) yield bodyToHtml(body) }</dd>
          }

        val note: NodeSeq =
          orEmpty(comment.note) {
            <dt>Note</dt>
            <dd>{
              val noteXml: List[NodeSeq] =  for(note <- comment.note ) yield <span class="cmt">{bodyToHtml(note)}</span>
              noteXml.reduceLeft(_ ++ Text(", ") ++ _)
            }</dd>
          }

        val seeAlso: NodeSeq =
          orEmpty(comment.see) {
            <dt>See also</dt>
            <dd>{
              val seeXml: List[NodeSeq] = for(see <- comment.see ) yield <span class="cmt">{bodyToHtml(see)}</span>
              seeXml.reduceLeft(_ ++ _)
            }</dd>
          }

        val exceptions: NodeSeq =
          orEmpty(comment.throws) {
            <dt>Exceptions thrown</dt>
            <dd>{
              val exceptionsXml: List[NodeSeq] =
                for((name, body) <- comment.throws.toList.sortBy(_._1) ) yield
                  <span class="cmt">{bodyToHtml(body)}</span>
              exceptionsXml.reduceLeft(_ ++ Text("") ++ _)
            }</dd>
          }

        val todo: NodeSeq =
          orEmpty(comment.todo) {
            <dt>To do</dt>
            <dd>{
              val todoXml: List[NodeSeq] = (for(todo <- comment.todo ) yield <span class="cmt">{bodyToHtml(todo)}</span> )
              todoXml.reduceLeft(_ ++ Text(", ") ++ _)
            }</dd>
          }

        example ++ version ++ sinceVersion ++ exceptions ++ todo ++ note ++ seeAlso

      case _ => NodeSeq.Empty
    }
    // end attributes block vals ---

    val attributesInfo = implicitInformation ++ attributes ++ definitionClasses ++ fullSignature ++ selfType ++ annotations ++ deprecation ++ migration ++ sourceLink ++ mainComment
    val attributesBlock =
      if (attributesInfo.isEmpty)
        NodeSeq.Empty
      else
        <dl class="attributes block"> { attributesInfo }</dl>

    val linearization = mbr match {
      case dtpl: DocTemplateEntity if isSelf && !isReduced && dtpl.linearizationTemplates.nonEmpty =>
        <div class="toggleContainer block">
          <span class="toggle">Linear Supertypes</span>
          <div class="superTypes hiddenContent">{
            typesToHtml(dtpl.linearizationTypes, hasLinks = true, sep = scala.xml.Text(", "))
          }</div>
        </div>
      case _ => NodeSeq.Empty
    }

    val subclasses = mbr match {
      case dtpl: DocTemplateEntity if isSelf && !isReduced =>
        val subs: Set[DocTemplateEntity] = HashSet.empty
        def transitive(dtpl: DocTemplateEntity) {
          for (sub <- dtpl.directSubClasses if !(subs contains sub)) {
            subs add sub
            transitive(sub)
          }
        }
        transitive(dtpl)
        if (subs.nonEmpty)
          <div class="toggleContainer block">
            <span class="toggle">Known Subclasses</span>
            <div class="subClasses hiddenContent">{
              templatesToHtml(subs.toList.sortBy(_.name), scala.xml.Text(", "))
            }</div>
          </div>
        else NodeSeq.Empty
      case _ => NodeSeq.Empty
    }

    def createDiagram(f: DocTemplateEntity => Option[Diagram], description: String, id: String): NodeSeq =
      if (s.docDiagrams.value) mbr match {
        case dtpl: DocTemplateEntity if isSelf && !isReduced =>
          val diagram = f(dtpl)
          if (diagram.isDefined) {
            val diagramSvg = generator.generate(diagram.get, tpl, this)
            if (diagramSvg != NodeSeq.Empty) {
              <div class="toggleContainer block diagram-container" id={ id + "-container"}>
                <span class="toggle diagram-link">{ description }</span>
                <div class="diagram" id={ id }>{
                  diagramSvg
                }</div>
              </div>
            } else NodeSeq.Empty
          } else NodeSeq.Empty
        case _ => NodeSeq.Empty
      } else NodeSeq.Empty // diagrams not generated

    val typeHierarchy = createDiagram(_.inheritanceDiagram, "Type Hierarchy", "inheritance-diagram")
    val contentHierarchy = createDiagram(_.contentDiagram, "Content Hierarchy", "content-diagram")

    memberComment ++ authorComment ++ paramComments ++ attributesBlock ++ linearization ++ subclasses ++ typeHierarchy ++ contentHierarchy
  }

  def boundsToHtml(hi: Option[TypeEntity], lo: Option[TypeEntity], hasLinks: Boolean): NodeSeq = {
    def bound0(bnd: Option[TypeEntity], pre: String): NodeSeq = bnd match {
      case None => NodeSeq.Empty
      case Some(tpe) => scala.xml.Text(pre) ++ typeToHtml(tpe, hasLinks)
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
        Some(Paragraph(Chain(List(CText("private["), EntityLink(comment.Text(owner.qualifiedName), LinkToTpl(owner)), CText("]")))))
      case ProtectedInInstance() =>
        Some(Paragraph(CText("protected[this]")))
      case ProtectedInTemplate(owner) if (owner == mbr.inTemplate) =>
        Some(Paragraph(CText("protected")))
      case ProtectedInTemplate(owner) =>
        Some(Paragraph(Chain(List(CText("protected["), EntityLink(comment.Text(owner.qualifiedName), LinkToTpl(owner)), CText("]")))))
      case Public() =>
        None
    }
  }

  /** name, tparams, params, result */
  def signature(mbr: MemberEntity, isSelf: Boolean, isReduced: Boolean = false): NodeSeq = {

    def inside(hasLinks: Boolean, nameLink: String = ""): NodeSeq =
      <xml:group>
      <span class="modifier_kind">
        <span class="modifier">{ mbr.flags.map(flag => inlineToHtml(flag.text) ++ scala.xml.Text(" ")) }</span>
        <span class="kind">{ kindToString(mbr) }</span>
      </span>
      <span class="symbol">
        {
          val nameClass =
            if (mbr.isImplicitlyInherited)
              if (mbr.isShadowedOrAmbiguousImplicit)
                "implicit shadowed"
              else
                "implicit"
            else
              "name"

          val nameHtml = {
            val value = if (mbr.isConstructor) tpl.name else mbr.name
            val span = if (mbr.deprecation.isDefined)
              <span class={ nameClass + " deprecated"} title={"Deprecated: "+bodyToStr(mbr.deprecation.get)}>{ value }</span>
            else
              <span class={ nameClass }>{ value }</span>
            val encoded = scala.reflect.NameTransformer.encode(value)
            if (encoded != value) {
              span % new UnprefixedAttribute("title",
                                             "gt4s: " + encoded +
                                             span.attribute("title").map(
                                               node => ". " + node
                                             ).getOrElse(""),
                                             scala.xml.Null)
            } else {
              span
            }
          }
          if (!nameLink.isEmpty)
            <a href={nameLink}>{nameHtml}</a>
          else nameHtml
        }{
          def tparamsToHtml(mbr: Any): NodeSeq = mbr match {
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
        }{
          if (isReduced) NodeSeq.Empty else {
            def paramsToHtml(vlsss: List[List[ValueParam]]): NodeSeq = {
              def param0(vl: ValueParam): NodeSeq =
                // notice the }{ in the next lines, they are necessary to avoid an undesired whitespace in output
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
            case tme: MemberEntity if (tme.isDef || tme.isVal || tme.isLazyVal || tme.isVar) =>
              <span class="result">: { typeToHtml(tme.resultType, hasLinks) }</span>

            case abt: MemberEntity with AbstractType =>
              val b2s = boundsToHtml(abt.hi, abt.lo, hasLinks)
              if (b2s != NodeSeq.Empty)
                <span class="result">{ b2s }</span>
              else NodeSeq.Empty

            case alt: MemberEntity with AliasType =>
              <span class="result"> = { typeToHtml(alt.alias, hasLinks) }</span>

            case tpl: MemberTemplateEntity if !tpl.parentTypes.isEmpty =>
              <span class="result"> extends { typeToHtml(tpl.parentTypes.map(_._2), hasLinks) }</span>

            case _ => NodeSeq.Empty
          }
        }}
      </span>
      </xml:group>
    mbr match {
      case dte: DocTemplateEntity if !isSelf =>
        <h4 class="signature">{ inside(hasLinks = true, nameLink = relativeLinkTo(dte)) }</h4> ++ permalink(dte, isSelf)
      case _ if isSelf =>
        <h4 id="signature" class="signature">{ inside(hasLinks = true) }</h4>
      case _ =>
        <h4 class="signature">{ inside(hasLinks = true) }</h4> ++ permalink(mbr)
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
      for (c <- text) c match {
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
    for ((from, (member, to)) <- tree.refEntity.toSeq) {
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
            val anchor = "#" + mbr.signature
            val link = relativeLinkTo(mbr.inTemplate)
            myXml ++= <span class="name"><a href={link ++ anchor}>{str.substring(from, to)}</a></span>
        }
        index = to
      }
    }

    if (index <= length-1)
      myXml ++= codeStringToXml(str.substring(index, length ))

    if (length < 36)
      <span class="symbol">{ myXml }</span>
    else
      <span class="defval" name={ myXml }>{ "..." }</span>
  }

  private def argumentsToHtml(argss: List[ValueArgument]): NodeSeq = {
    def argumentsToHtml0(argss: List[ValueArgument]): NodeSeq = argss match {
      case Nil         => NodeSeq.Empty
      case arg :: Nil  => argumentToHtml(arg)
      case arg :: args => argumentToHtml(arg) ++ scala.xml.Text(", ") ++ argumentsToHtml0(args)
    }
    <span class="args">({ argumentsToHtml0(argss) })</span>
  }

  private def argumentToHtml(arg: ValueArgument): NodeSeq = {
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

  private def bodyToStr(body: comment.Body): String =
    body.blocks flatMap (blockToStr(_)) mkString ""

  private def blockToStr(block: comment.Block): String = block match {
    case comment.Paragraph(in) => inlineToStr(in)
    case _ => block.toString
  }

  private def inlineToStr(inl: comment.Inline): String = inl match {
    case comment.Chain(items) => items flatMap (inlineToStr(_)) mkString ""
    case comment.Italic(in) => inlineToStr(in)
    case comment.Bold(in) => inlineToStr(in)
    case comment.Underline(in) => inlineToStr(in)
    case comment.Monospace(in) => inlineToStr(in)
    case comment.Text(text) => text
    case comment.Summary(in) => inlineToStr(in)
    case _ => inl.toString
  }

  private def typeToHtmlWithStupidTypes(tpl: TemplateEntity, superTpl: TemplateEntity, superType: TypeEntity): NodeSeq =
    if (tpl.universe.settings.useStupidTypes.value)
      superTpl match {
        case dtpl: DocTemplateEntity =>
          val sig = signature(dtpl, isSelf = false, isReduced = true) \ "_"
          sig
        case tpl: TemplateEntity =>
          Text(tpl.name)
      }
  else
    typeToHtml(superType, hasLinks = true)

  private def constraintToHtml(constraint: Constraint): NodeSeq = constraint match {
    case ktcc: KnownTypeClassConstraint =>
      scala.xml.Text(ktcc.typeExplanation(ktcc.typeParamName) + " (" + ktcc.typeParamName + ": ") ++
        templateToHtml(ktcc.typeClassEntity) ++ scala.xml.Text(")")
    case tcc: TypeClassConstraint =>
      scala.xml.Text(tcc.typeParamName + " is ") ++
        <a href="http://stackoverflow.com/questions/2982276/what-is-a-context-bound-in-scala" target="_blank">
        context-bounded</a> ++ scala.xml.Text(" by " + tcc.typeClassEntity.qualifiedName + " (" + tcc.typeParamName + ": ") ++
        templateToHtml(tcc.typeClassEntity) ++ scala.xml.Text(")")
    case impl: ImplicitInScopeConstraint =>
      scala.xml.Text("an implicit value of type ") ++ typeToHtml(impl.implicitType, hasLinks = true) ++ scala.xml.Text(" is in scope")
    case eq: EqualTypeParamConstraint =>
      scala.xml.Text(eq.typeParamName + " is " + eq.rhs.name + " (" + eq.typeParamName + " =:= ") ++
        typeToHtml(eq.rhs, hasLinks = true) ++ scala.xml.Text(")")
    case bt: BoundedTypeParamConstraint =>
      scala.xml.Text(bt.typeParamName + " is a superclass of " + bt.lowerBound.name + " and a subclass of " +
        bt.upperBound.name + " (" + bt.typeParamName + " >: ") ++
        typeToHtml(bt.lowerBound, hasLinks = true) ++ scala.xml.Text(" <: ") ++
        typeToHtml(bt.upperBound, hasLinks = true) ++ scala.xml.Text(")")
    case lb: LowerBoundedTypeParamConstraint =>
      scala.xml.Text(lb.typeParamName + " is a superclass of " + lb.lowerBound.name + " (" + lb.typeParamName + " >: ") ++
        typeToHtml(lb.lowerBound, hasLinks = true) ++ scala.xml.Text(")")
    case ub: UpperBoundedTypeParamConstraint =>
      scala.xml.Text(ub.typeParamName + " is a subclass of " + ub.upperBound.name + " (" + ub.typeParamName + " <: ") ++
        typeToHtml(ub.upperBound, hasLinks = true) ++ scala.xml.Text(")")
  }
}

object Template {
  /* Vlad: Lesson learned the hard way: don't put any stateful code that references the model here,
   * it won't be garbage collected and you'll end up filling the heap with garbage */

  def lowerFirstLetter(s: String) = if (s.length >= 1) s.substring(0,1).toLowerCase() + s.substring(1) else s
}
