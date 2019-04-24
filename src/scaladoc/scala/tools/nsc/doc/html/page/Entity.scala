/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package doc
package html
package page

import base._
import base.comment._
import model._
import model.diagram._
import page.diagram._

import scala.collection.mutable
import scala.reflect.internal.Reporter

trait EntityPage extends HtmlPage {
  import HtmlTags._

  def universe: doc.Universe
  def generator: DiagramGenerator
  def tpl: DocTemplateEntity
  def docletReporter: Reporter

  override val path = templateToPath(tpl)

  def title = {
    val s = universe.settings
    ( if (!s.doctitle.isDefault) s.doctitle.value + " " else "" ) +
    ( if (!s.docversion.isDefault) s.docversion.value else "" ) +
    ( if ((!s.doctitle.isDefault || !s.docversion.isDefault) && tpl.qualifiedName != "_root_") " - " + tpl.qualifiedName else "" )
  }

  def headers: Elems = {
    def extScript(str: String) = Script(`type` = "text/javascript", src = str)
    def libScript(value: String) = extScript(relativeLinkTo(List(value, "lib")))
    val canonicalSetting = universe.settings.docCanonicalBaseUrl
    val canonicalLink =  if (canonicalSetting.isSetByUser) {
      val canonicalUrl =
        if (canonicalSetting.value.endsWith("/")) canonicalSetting.value
        else canonicalSetting.value + "/"
      List(HtmlTags.Link(href = canonicalUrl + Page.relativeLinkTo(List("."), path), rel = "canonical"))
    } else Nil
    canonicalLink ++ List(
      HtmlTags.Link(href = relativeLinkTo(List("index.css", "lib")), media = "screen", `type` = "text/css", rel = "stylesheet"),
    HtmlTags.Link(href = relativeLinkTo(List("template.css", "lib")), media = "screen", `type` = "text/css", rel = "stylesheet"),
    HtmlTags.Link(href = relativeLinkTo(List("diagrams.css", "lib")), media = "screen", `type` = "text/css", rel = "stylesheet", id = "diagrams-css"),
    libScript("jquery.js"),
    libScript("index.js"),
    extScript(relativeLinkTo(List("index.js"))),
    libScript("scheduler.js"),
    libScript("template.js"),
    libScript("tools.tooltip.js")) ++
    ((if (!universe.settings.docDiagrams.value) Nil
     else (List(
         extScript("https://d3js.org/d3.v4.js"),
         extScript("https://cdn.jsdelivr.net/npm/graphlib-dot@0.6.2/dist/graphlib-dot.min.js"),
         extScript("https://cdnjs.cloudflare.com/ajax/libs/dagre-d3/0.6.1/dagre-d3.min.js")))) :+
    Script(`type` = "text/javascript", elems =
      Txt("/* this variable can be used by the JS to determine the path to the root document */\n" +
          s"""var toRoot = '${ val p = templateToPath(tpl); "../" * (p.size - 1) }';""")))
  }



  def body =
    HtmlTags.Body (
      search ::
      Div(id = "search-results", elems =
             Div(id = "search-progress", elems =
                 Div(id = "progress-fill")
                ) ::
               Div(id="results-content", elems =
                 Div(id="entity-results") ::
                 Div(id="member-results")) :: NoElems
      ) ::
      Div(id="content-scroll-container", style="-webkit-overflow-scrolling: touch;", elems =
        Div(id="content-container", style="-webkit-overflow-scrolling: touch;", elems =
          Div(id="subpackage-spacer", elems =
            Div(id="packages", elems =
              H(1, Txt("Packages")) ::
              Ul(elems = {
                    def entityToUl(mbr: TemplateEntity with MemberEntity, indentation: Int): Elems =
                      if (mbr.isObject && hasCompanion(mbr))
                        NoElems
                      else
                        Li(`class`= s"current-entities indented$indentation", elems =
                          (mbr match {
                            case dtpl: DocTemplateEntity =>
                              dtpl.companion.fold(Span(`class`= "separator"): Elem) { c: DocTemplateEntity =>
                                A(`class`= "object", href=relativeLinkTo(c), title= memberToShortCommentTitleTag(c))
                              }
                            case _                       => Span(`class`= "separator")
                          }) :: Txt(" ") ::
                          A(`class`= mbr.kind, href=relativeLinkTo(mbr), title=memberToShortCommentTitleTag(mbr)) ::
                          A(href=relativeLinkTo(mbr), title=memberToShortCommentTitleTag(mbr), elems= Txt(mbr.name)) :: NoElems
                        )

                    // Get path from root
                    val rootToParentLis: Elems = tpl.toRoot
                                          .tail
                                          .reverse
                                          .zipWithIndex
                                          .flatMap { case (pack, ind) =>
                                            memberToHtml(pack, tpl, indentation = ind, isParent = (pack eq tpl.toRoot.tail.head))
                                          }

                    val parent = tpl.toRoot match {
                      case _ :: parent :: _ if !parent.isRootPackage => Some(parent)
                      case _                                         => None
                    }

                    val parentSub = parent.fold(Seq[TemplateEntity with MemberEntity](tpl)) { p =>
                      p.templates.filter(_.isPackage).sortBy(_.name)
                    }

                    // If current entity is a package, take its containing entities - otherwise take parent's containing entities
                    val currentPackageTpls =
                      if (tpl.isPackage) tpl.templates
                      else parent.fold(Seq.empty[TemplateEntity with MemberEntity])(p => p.templates)

                    val (subsToTpl, subsAfterTpl) = parentSub.partition(_.name <= tpl.name)

                    val subsToTplLis = subsToTpl.toList.flatMap(memberToHtml(_, tpl, indentation = rootToParentLis.length))
                    val subsAfterTplLis = subsAfterTpl.toList.flatMap(memberToHtml(_, tpl, indentation = rootToParentLis.length))
                    val currEntityLis = currentPackageTpls
                                        .filter(x => !x.isPackage && (x.isTrait || x.isClass || x.isAbstractType || x.isObject))
                                        .sortBy(_.name)
                                        .toList.flatMap(entityToUl(_, (if (tpl.isPackage) 0 else -1) + rootToParentLis.length))
                    val currSubLis = tpl.templates
                                     .filter(_.isPackage)
                                     .sortBy(_.name)
                                     .flatMap(memberToHtml(_, tpl, indentation = rootToParentLis.length + 1))

                    if (subsToTpl.isEmpty && !tpl.isPackage) // current Entity is not a package, show packages before entity listing
                      rootToParentLis ++ subsToTplLis ++ subsAfterTplLis ++ currSubLis ++ currEntityLis
                    else
                      rootToParentLis ++ subsToTplLis ++ currSubLis ++ currEntityLis ++ subsAfterTplLis
                  }
                )
             )
           ) ::
           Div(id="content", elems = content
           ) :: NoElems
         )
       )
     )


  def search =
    Div(id="search", elems=
        Span(id= "doc-title", elems= Txt(universe.settings.doctitle.value) :: Span(id= "doc-version", elems= Txt(universe.settings.docversion.value))) ::
        Txt(" ") ::
        Span(`class`= "close-results", elems= Span(`class`="left", elems= Txt("<")) :: Txt(" Back")) ::
        Div(id="textfilter", elems=
          Span(`class`= "input", elems=
            Input(autocapitalize="none", placeholder="Search", id="index-input", `type`="text", accesskey="/") ::
            I(`class`= "clear material-icons", elems=Txt("\uE14C")) ::
            I(id="search-icon", `class`= "material-icons", elems=Txt("\uE8B6"))
          )
         ) :: NoElems
       )

  val valueMembers =
    (tpl.methods ++ tpl.values ++ tpl.templates.filter(x => x.isObject)).sorted

  val (absValueMembers, nonAbsValueMembers) =
    valueMembers partition (_.isAbstract)

  val (deprValueMembers, nonDeprValueMembers) =
    nonAbsValueMembers partition (_.deprecation.isDefined)

  val (concValueMembers, shadowedImplicitMembers) =
    nonDeprValueMembers partition (!_.isShadowedOrAmbiguousImplicit)

  val allTypeMembers =
    tpl.abstractTypes ++ tpl.aliasTypes ++ tpl.templates.filter(x => x.isTrait || x.isClass) sorted (implicitly[Ordering[MemberEntity]])

  val (deprTypeMembers, typeMembers) = allTypeMembers partition (_.deprecation.isDefined)

  val packageMembers = tpl.templates.filter(x => x.isPackage) sorted (implicitly[Ordering[MemberEntity]])

  val constructors = (tpl match {
    case cls: Class => (cls.constructors: List[MemberEntity]).sorted
    case _ => Nil
  })

  /* for body, there is a special case for AnyRef, otherwise AnyRef appears
   * like a package/object this problem should be fixed, this implementation
   * is just a patch. */
  val content = {
    val templateName = Txt(if (tpl.isRootPackage) "root package " else tpl.name)
    val displayName: Elems = tpl.companion match {
      case Some(companion) if (companion.visibility.isPublic && companion.inSource.isDefined) =>
        A(href= relativeLinkTo(companion), title= docEntityKindToCompanionTitle(tpl), elems=templateName)
      case _ =>
        templateName
    }
    val owner: Elems = {
      if (tpl.isRootPackage || tpl.inTemplate.isRootPackage)
        NoElems
      else
        P(id= "owner", elems= templatesToHtml(tpl.inTemplate.toRoot.reverse.tail, Txt(".")))
    }


    val definition: Elems =
      List(Div(id="definition", elems=
          {val imageClass = docEntityImageClass(tpl)

          tpl.companion match {
            case Some(companion) if (companion.visibility.isPublic && companion.inSource.isDefined) =>
              A(href= relativeLinkTo(companion), title= docEntityKindToCompanionTitle(tpl), elems= Div(`class`= s"big-circle $imageClass", elems=Txt(imageClass.substring(0,1))))
            case _ =>
              Div(`class`= s"big-circle $imageClass", elems=Txt(imageClass.substring(0,1)))
          }} ::
        owner ++
        H(1, displayName ++ permalink(tpl)) ++
        {if (tpl.isPackage) NoElems else H(3, companionAndPackage(tpl)) :: NoElems }
      ))

    val memberSel: Elems =
      if (valueMembers.forall(_.kind == "package")) NoElems
      else List(Div(id="mbrsel", elems=
        Div(`class`="toggle") ::
        Div(id="memberfilter", elems=
          I(`class`="material-icons arrow", elems= Txt("\uE037")) ::
          Span(`class`="input", elems=
            Input(id="mbrsel-input", placeholder="Filter all members", `type`="text", accesskey="/")
          ) ::
          I(`class`="clear material-icons", elems=Txt("\uE14C"))
        ) ::
        Div(id="filterby", elems=
          Div(id="order", elems=
            Span(`class`="filtertype", elems=Txt("Ordering")) ::
            Ol(elems=
              {
                if (!universe.settings.docGroups.value || tpl.members.map(_.group).distinct.forall(_ == ModelFactory.defaultGroup))
                  NoElems
                else
                  Li(`class`="group out", elems=Span(elems=Txt("Grouped"))) :: NoElems
              } ++
              (Li(`class`="alpha in", elems=Span(elems=Txt("Alphabetic"))) ::
              {
                if (tpl.linearizationTemplates.isEmpty && tpl.conversions.isEmpty)
                  NoElems
                else
                  Li(`class`="inherit out", elems=Span(elems=Txt("By Inheritance"))) :: NoElems
              })
            )
          ) ++ (
          if (tpl.linearizationTemplates.isEmpty && tpl.conversions.isEmpty) NoElems else {
            (if (tpl.linearizationTemplates.isEmpty) NoElems else
              Div(`class`="ancestors", elems=
                Span(`class`="filtertype", elems=Txt("Inherited") :: Br :: NoElems) ::
                Ol(id="linearization", elems=
                  { (tpl :: tpl.linearizationTemplates).map(wte => Li(`class`="in", name= wte.qualifiedName, elems=Span(elems= Txt(wte.name)))) }
                )
              ) :: NoElems) ++
            (if (tpl.conversions.isEmpty) NoElems else
              Div(`class`="ancestors", elems=
                Span(`class`="filtertype", elems=Txt("Implicitly") :: Br :: NoElems) ::
                Ol(id="implicits", elems= {
                  tpl.conversions.map { conv =>
                    val name = conv.conversionQualifiedName
                    val hide = universe.settings.hiddenImplicits(name)
                    Li(`class`="in", name= name, `data-hidden`= hide.toString, elems= Span(elems= Txt("by " + conv.conversionShortName)))
                  }
                }
                )
              ) :: NoElems) ++ List(
            Div(`class`="ancestors", elems=
              Span(`class`="filtertype") ::
              Ol(elems=
                Li(`class`="hideall out", elems= Span(elems=Txt("Hide All"))) ::
                Li(`class`="showall in", elems= Span(elems=Txt("Show All")))
              )
            ))
          }) ++ List(
          Div(id="visbl", elems=
              Span(`class`="filtertype", elems=Txt("Visibility")) ::
              Ol(elems=
                Li(`class`="public in", elems= Span(elems=Txt("Public"))) :: Li(`class`="all out", elems= Span(elems=Txt("All"))))
          ))
        )
      ))

    val template: Elems = List(
     Div(id="template", elems= List(
        Div(id="allMembers", elems=
             memsDiv("package members", "Package Members", packageMembers, "packages")
          ++ memsDiv("members", "Instance Constructors", constructors, "constructors")
          ++ memsDiv("types members", "Type Members", typeMembers, "types")
          ++ memsDiv("types members", "Deprecated Type Members", deprTypeMembers)
          ++ memsDiv("values members", "Abstract Value Members", absValueMembers)
          ++ memsDiv("values members", if (absValueMembers.isEmpty) "Value Members" else "Concrete Value Members", concValueMembers)
          ++ memsDiv("values members", "Shadowed Implicit Value Members", shadowedImplicitMembers)
          ++ memsDiv("values members", "Deprecated Value Members", deprValueMembers)),
        Div(id="inheritedMembers", elems=
          // linearization
          (for ((superTpl, superType) <- tpl.linearizationTemplates zip tpl.linearizationTypes) yield
            Div(`class`="parent", name= superTpl.qualifiedName, elems=
              H(3, elems=Txt("Inherited from ") ::
                typeToHtml(superType, hasLinks = true)
               ))) ++
          // implicitly inherited
          (for (conversion <- tpl.conversions) yield
            Div(`class`="conversion", name= conversion.conversionQualifiedName, elems=
              H(3, elems=Txt(s"Inherited by implicit conversion ${conversion.conversionShortName} from") ::
                         typeToHtml(tpl.resultType, hasLinks = true) ++ (Txt(" to ") :: typeToHtml(conversion.targetType, hasLinks = true))
              )
            ))),
        Div(id="groupedMembers", elems= {
          val allGroups = tpl.members.map(_.group).distinct
          val orderedGroups = allGroups.map(group => (tpl.groupPriority(group), group)).sorted.map(_._2)
          // linearization
          for (group <- orderedGroups) yield
            Div(`class` = "group", name = group, elems =
              H(3, Txt(tpl.groupName(group))) :: (
              tpl.groupDescription(group) match {
                case Some(body) => Div(`class`="comment cmt", elems= bodyToHtml(body)) :: NoElems
                case _          => NoElems
              })
            )
        }))
      ))

    val postamble =
      List(Div(id = "tooltip"),
           if (Set("epfl", "EPFL").contains(tpl.universe.settings.docfooter.value))
             Div(id = "footer", elems = Txt("Scala programming documentation. Copyright (c) 2002-2019 ") :: A(href = "http://www.epfl.ch", target = "_top", elems = Txt("EPFL")) :: Txt(", with contributions from ") :: A(href = "http://www.lightbend.com", target = "_top", elems = Txt("Lightbend")) :: Txt("."))
           else
             Div(id = "footer", elems = Txt(tpl.universe.settings.docfooter.value)))

    HtmlTags.Body(`class`= tpl.kind + (if (tpl.isType) " type" else " value"), elems=
      definition ++ signature(tpl, isSelf = true) ++ memberToCommentHtml(tpl, tpl.inTemplate, isSelf = true) ++ memberSel ++ template ++ postamble
    )
  }

  def memsDiv(cls: String, header: String, mems: List[MemberEntity], name: String = null) =
    if (mems.isEmpty) NoElems
    else List(Div(id= name, `class`= cls, elems= List(H(3, Txt(header)), Ol(elems= mems flatMap (memberToHtml(_, tpl))))))

  def memberToHtml(
    mbr:         MemberEntity,
    inTpl:       DocTemplateEntity,
    isParent:    Boolean = false,
    indentation: Int = 0
  ): Elems = {
    // Sometimes it's same, do we need signatureCompat still?
    val sig = if (mbr.signature == mbr.signatureCompat) {
      A(id= mbr.signature) :: NoElems
    } else {
      A(id= mbr.signature) :: A(id= mbr.signatureCompat) :: NoElems
    }

    val memberComment = memberToCommentHtml(mbr, inTpl, isSelf = false)
    Li(name= mbr.definitionName, visbl= if (mbr.visibility.isProtected) "prt" else "pub",
      `class`= s"indented$indentation " + (if (mbr eq inTpl) "current" else ""),
      `data-isabs`= mbr.isAbstract.toString,
      fullComment= if(!memberComment.exists(_.tagName == "div")) "no" else "yes",
      group= mbr.group, elems=
      { sig } ++
      (Txt(" ") :: { signature (mbr, isSelf = false) }) ++
      { memberComment }
      )
  }

  def memberToCommentHtml(mbr: MemberEntity, inTpl: DocTemplateEntity, isSelf: Boolean): Elems = {
    mbr match {
      // comment of class itself
      case dte: DocTemplateEntity if isSelf =>
        Div(id="comment", `class`="fullcommenttop", elems= memberToCommentBodyHtml(dte, inTpl, isSelf = true))
      case _ =>
        // comment of non-class member or non-documented inner class
        val commentBody = memberToCommentBodyHtml(mbr, inTpl, isSelf = false)
        if (commentBody.isEmpty)
          NoElems
        else {
          val shortComment = memberToShortCommentHtml(mbr, isSelf)
          val longComment = memberToUseCaseCommentHtml(mbr, isSelf) ++ memberToCommentBodyHtml(mbr, inTpl, isSelf)

          val includedLongComment =
            if (textOf(shortComment) == textOf(longComment)) NoElems
            else Div(`class`="fullcomment", elems= longComment) :: NoElems

          shortComment ++ includedLongComment
        }
    }
  }

  def memberToUseCaseCommentHtml(mbr: MemberEntity, isSelf: Boolean): Elems = {
    mbr match {
      case nte: NonTemplateMemberEntity if nte.isUseCase =>
        inlineToHtml(comment.Text("[use case] "))
      case _ => NoElems
    }
  }

  def memberToShortCommentHtml(mbr: MemberEntity, isSelf: Boolean): Elems =
    mbr.comment.toList.flatMap { comment =>
      P(`class`="shortcomment cmt", elems= memberToUseCaseCommentHtml(mbr, isSelf) ++ inlineToHtml(comment.short) )
    }

  def memberToShortCommentTitleTag(mbr: MemberEntity): String =
    mbr.comment.fold("")(comment => Page.inlineToStrForTitleTag(comment.short))

  def memberToCommentBodyHtml(mbr: MemberEntity, inTpl: DocTemplateEntity, isSelf: Boolean, isReduced: Boolean = false): Elems = {
    val s = universe.settings

    val memberComment =
      if (mbr.comment.isEmpty) NoElems
      else Div(`class`="comment cmt", elems= commentToHtml(mbr.comment)) :: NoElems

    val authorComment =
      if (! s.docAuthor || mbr.comment.isEmpty ||
        mbr.comment.isDefined && mbr.comment.get.authors.isEmpty) NoElems
      else Div(`class`= "comment cmt", elems=
        H(6, Txt(if (mbr.comment.get.authors.size > 1) "Authors:" else "Author:" )) ::
          mbr.comment.get.authors.flatMap(bodyToHtml)
        )  :: NoElems

    val paramComments = {
      val prs: List[ParameterEntity] = mbr match {
        case cls: Class => cls.typeParams ::: cls.valueParams.flatten
        case trt: Trait => trt.typeParams
        case dfe: Def => dfe.typeParams ::: dfe.valueParams.flatten
        case ctr: Constructor => ctr.valueParams.flatten
        case _ => Nil
      }

      def paramCommentToHtml(prs: List[ParameterEntity], comment: Comment): Elems = prs match {

        case (tp: TypeParam) :: rest =>
          val paramEntry: Elems = {
            Dt(`class`= "tparam", elems=Txt(tp.name)) :: Dd(`class`= "cmt", elems= bodyToHtml(comment.typeParams(tp.name)) )
          }
          paramEntry ++ paramCommentToHtml(rest, comment)

        case (vp: ValueParam) :: rest  =>
          val paramEntry: Elems = {
            Dt(`class`= "param", elems=Txt(vp.name)) :: Dd(`class`= "cmt", elems= bodyToHtml(comment.valueParams(vp.name)) )
          }
          paramEntry ++ paramCommentToHtml(rest, comment)

        case _ =>
          NoElems
      }

      mbr.comment.fold(NoElems) { comment =>
        val cmtedPrs = prs filter {
          case tp: TypeParam => comment.typeParams isDefinedAt tp.name
          case vp: ValueParam => comment.valueParams isDefinedAt vp.name
        }
        if (cmtedPrs.isEmpty && comment.result.isEmpty) NoElems
        else {
          Dl(`class`= "paramcmts block", elems =
            paramCommentToHtml(cmtedPrs, comment) ++ (
            comment.result match {
              case None => NoElems
              case Some(cmt) =>
                Dt(elems=Txt("returns")) :: Dd(`class`="cmt", elems=bodyToHtml(cmt))
            }))
        }
      }
    }

    val implicitInformation = mbr.byConversion match {
      case Some(conv) =>
        Dt(`class`= "implicit", elems= Txt("Implicit")) ++
        {
          val targetType = typeToHtml(conv.targetType, hasLinks = true)
          val conversionMethod = conv.convertorMethod match {
            case Left(member) => member.name
            case Right(name)  => name
          }

          // strip off the package object endings, they make things harder to follow
          val conversionOwnerQualifiedName = conv.convertorOwner.qualifiedName.stripSuffix(".package")
          val conversionOwner = templateToHtml(conv.convertorOwner, conversionOwnerQualifiedName)

          val constraintText = conv.constraints match {
            case Nil =>
              NoElems
            case List(constraint) =>
              Txt("This conversion will take place only if ") ++ constraintToHtml(constraint) ++ Txt(".")
            case List(constraint1, constraint2) =>
              Txt("This conversion will take place only if ") ++ constraintToHtml(constraint1) ++
                Txt(" and at the same time ") ++ constraintToHtml(constraint2) ++ Txt(".")
            case constraints =>
              Br :: Txt("This conversion will take place only if all of the following constraints are met:") :: Br :: {
                var index = 0
                constraints flatMap { constraint => Txt({ index += 1; index } + ". ") :: (constraintToHtml(constraint) :+ Br) }
              }
          }

          Dd(elems=
            (Txt("This member is added by an implicit conversion from ") :: (typeToHtml(inTpl.resultType, hasLinks = true) :+ Txt(" to"))) ++
            targetType ++ (Txt(" performed by method ") :: (Txt(conversionMethod+" in ") :: (conversionOwner :+ Txt(".")))) ++
            constraintText
          )
        } ++ {
          if (mbr.isShadowedOrAmbiguousImplicit) {
            // These are the members that are shadowing or ambiguating the current implicit
            // see ImplicitMemberShadowing trait for more information
            val shadowingSuggestion = {
              val params = mbr match {
                case d: Def => d.valueParams.map(_.map(_.name).mkString("(", ", ", ")")).mkString
                case _      => "" // no parameters
              }
              Br ++ Txt("To access this member you can use a ") ++
              A(href="http://stackoverflow.com/questions/2087250/what-is-the-purpose-of-type-ascription-in-scala",
                target="_blank", elems= Txt("type ascription")) ++ Txt(":") ++
              Br ++ Div(`class`="cmt", elems=Pre(Txt(s"(${EntityPage.lowerFirstLetter(tpl.name)}: ${conv.targetType.name}).${mbr.name}$params")))
            }

            val shadowingWarning: Elems =
              if (mbr.isShadowedImplicit)
                  Txt("This implicitly inherited member is shadowed by one or more members in this " +
                  "class.") ++ shadowingSuggestion
              else if (mbr.isAmbiguousImplicit)
                  Txt("This implicitly inherited member is ambiguous. One or more implicitly " +
                  "inherited members have similar signatures, so calling this member may produce an ambiguous " +
                  "implicit conversion compiler error.") ++ shadowingSuggestion
              else NoElems

            Dt(`class`="implicit", elems=Txt("Shadowing")) ::
            Dd(elems= shadowingWarning) :: NoElems

          } else NoElems
        }
      case _ =>
        NoElems
    }

    def dt(s: String) = Dt(elems=Txt(s))

    // --- start attributes block vals
    val attributes: Elems = {
      val fvs: List[comment.Paragraph] = visibility(mbr).toList
      if (fvs.isEmpty || isReduced) NoElems
      else {
        dt("Attributes") ::
        Dd(elems=  fvs flatMap { fv => { inlineToHtml(fv.text) :+ Txt(" ") } }  ) :: NoElems
      }
    }

    val definitionClasses: Elems = {
      val inDefTpls = mbr.inDefinitionTemplates
      if ((inDefTpls.tail.isEmpty && (inDefTpls.head == inTpl)) || isReduced) NoElems
      else {
        dt("Definition Classes") ::
        Dd(elems=  templatesToHtml(inDefTpls, Txt(" → "))  ) :: NoElems
      }
    }

    val fullSignature: Elems = {
      mbr match {
        case nte: NonTemplateMemberEntity if nte.isUseCase =>
          Div(`class`= "full-signature-block toggleContainer", elems=
            Span(`class`= "toggle", elems=
              I(`class`= "material-icons", elems=Txt("\uE037")) ::
              Txt("Full Signature") :: NoElems
            ) ::
            Div(`class`= "hiddenContent full-signature-usecase", elems= signature(nte.useCaseOf.get,isSelf = true))
          )
        case _ => NoElems
      }
    }

    val selfType: Elems = mbr match {
      case dtpl: DocTemplateEntity if (isSelf && dtpl.selfType.isDefined && !isReduced) =>
        dt("Self Type") ::
        Dd(elems=  typeToHtml(dtpl.selfType.get, hasLinks = true)  ) :: NoElems
      case _ => NoElems
    }

    val annotations: Elems = {
      // A list of annotations which don't show their arguments, e. g. because they are shown separately.
      val annotationsWithHiddenArguments = List("deprecated", "Deprecated", "migration")

      def showArguments(annotation: Annotation) =
        !(annotationsWithHiddenArguments.contains(annotation.qualifiedName))

      if (mbr.annotations.nonEmpty) {
        dt("Annotations") ::
        Dd(elems =
             mbr.annotations.flatMap { annot =>
               Span(`class` = "name", elems = Txt("@") :: templateToHtml(annot.annotationClass)) :: ((
               if (showArguments(annot)) argumentsToHtml(annot.arguments) else NoElems) :+ Txt(" "))
             }
          ) :: NoElems
      } else NoElems
    }

    val sourceLink: Elems = mbr match {
      case dtpl: DocTemplateEntity if (isSelf && dtpl.sourceUrl.isDefined && dtpl.inSource.isDefined && !isReduced) =>
        val (absFile, _) = dtpl.inSource.get
        dt("Source") ::
        Dd(elems=  A(href= dtpl.sourceUrl.get.toString, target="_blank", elems= Txt(absFile.file.getName) )  ) :: NoElems
      case _ => NoElems
    }

    val deprecations: Elems =
      mbr.deprecation match {
        case Some(deprecation) if !isReduced =>
          dt("Deprecated") ::
          Dd(`class`= "cmt", elems=  bodyToHtml(deprecation)  ) :: NoElems
        case _ => NoElems
      }

    val migrations: Elems =
      mbr.migration match {
        case Some(migration) if !isReduced =>
          dt("Migration") ::
          Dd(`class`= "cmt", elems=  bodyToHtml(migration)  ) :: NoElems
        case _ => NoElems
      }

    val mainComment: Elems = mbr.comment match {
      case Some(comment) if (! isReduced) =>
        def orEmpty[T](it: Iterable[T])(gen:  =>Elems): Elems =
          if (it.isEmpty) NoElems else gen

        val example =
          orEmpty(comment.example) {
            Div(`class`="block", elems= Txt(s"Example${if (comment.example.lengthIs > 1) "s" else ""}:") ::
               Ol(elems = {
                 val exampleXml: List[Elems] = for (ex <- comment.example) yield
                   Li(`class`= "cmt", elems= bodyToHtml(ex)) :: NoElems
                 exampleXml.reduceLeft(_ ++ Txt(", ") ++ _)
               })
            )
          }

        val version: Elems =
          orEmpty(comment.version) {
            dt("Version") ::
            Dd(elems= comment.version.toList.flatMap(bodyToHtml)) :: NoElems
          }

        val sinceVersion: Elems =
          orEmpty(comment.since) {
            dt("Since") ::
            Dd(elems= comment.since.toList.flatMap(bodyToHtml) ) :: NoElems
          }

        val note: Elems =
          orEmpty(comment.note) {
            dt("Note") ::
            Dd(elems= {
              val noteXml: List[Elems] =  for(note <- comment.note ) yield Span(`class`= "cmt", elems= bodyToHtml(note)) :: NoElems
              noteXml.reduceLeft(_ ++ Txt(", ") ++ _)
            })
          }

        val seeAlso: Elems =
          orEmpty(comment.see) {
            dt("See also") ::
            Dd(elems= {
              val seeXml: List[Elems] = for(see <- comment.see ) yield Span(`class`= "cmt", elems= bodyToHtml(see)) :: NoElems
              seeXml.reduceLeft(_ ++ _)
            })
          }

        val exceptions: Elems =
          orEmpty(comment.throws) {
            dt("Exceptions thrown") ::
            Dd(elems= {
              val exceptionsXml: List[Elems] =
                for((name, body) <- comment.throws.toList.sortBy(_._1) ) yield
                  Span(`class`= "cmt", elems= bodyToHtml(body)) :: NoElems
              exceptionsXml.reduceLeft(_ ++ Txt("") ++ _)
            })
          }

        val todo: Elems =
          orEmpty(comment.todo) {
            dt("To do") ::
            Dd(elems= {
              val todoXml: List[Elems] = for(todo <- comment.todo ) yield Span(`class`= "cmt", elems= bodyToHtml(todo)) :: NoElems
              todoXml.reduceLeft(_ ++ _)
            })
          }

        example ++ version ++ sinceVersion ++ exceptions ++ todo ++ note ++ seeAlso

      case _ => NoElems
    }
    // end attributes block vals ---

    val attributesInfo = implicitInformation ++ attributes ++ definitionClasses ++ fullSignature ++ selfType ++ annotations ++ deprecations ++ migrations ++ sourceLink ++ mainComment
    val attributesBlock =
      if (attributesInfo.isEmpty)
        NoElems
      else
        Dl(`class`= "attributes block", elems= attributesInfo ) :: NoElems

    val linearization = mbr match {
      case dtpl: DocTemplateEntity if isSelf && !isReduced && dtpl.linearizationTemplates.nonEmpty =>
        Div(`class` = "toggleContainer", elems =
          Div(`class` = "toggle block", elems =
            Span(elems =
              Txt("Linear Supertypes")
            ) ::
              Div(`class` = "superTypes hiddenContent", elems =
                typesToHtml(dtpl.linearizationTypes, hasLinks = true, sep = Txt(", "))
              )
          )) :: NoElems
      case _ => NoElems
    }

    val subclasses = mbr match {
      case dtpl: DocTemplateEntity if isSelf && !isReduced =>
        val subs = mutable.HashSet.empty[DocTemplateEntity]

        def transitive(dtpl: DocTemplateEntity): Unit = {
          for (sub <- dtpl.directSubClasses if !(subs contains sub)) {
            subs add sub
            transitive(sub)
          }
        }

        transitive(dtpl)
        if (subs.nonEmpty)
          Div(`class` = "toggleContainer", elems =
            Div(`class` = "toggle block", elems =
              Span(elems =
                Txt("Known Subclasses")
              ) ::
                Div(`class` = "subClasses hiddenContent", elems =
                  templatesToHtml(subs.toList.sorted(Entity.EntityOrdering), Txt(", "))
                )
            )) :: NoElems
        else NoElems
      case _ => NoElems
    }

    def diagramDiv(description: String, diagId: String)(diagramSvg: Elems): Elems =
      Div(`class`= "toggleContainer block diagram-container", id=diagId+"-container", elems= List(
        Span(`class`= "toggle diagram-link", elems= Txt(description)),
        Div(`class`= "diagram hiddenContent", id= diagId, elems= diagramSvg))) :: NoElems

    def ifDiags(genDiag: DocTemplateEntity => Option[Diagram])(embedDiagSvg: Elems => Elems): Elems =
      mbr match {
        case dtpl: DocTemplateEntity if s.docDiagrams.value && isSelf && !isReduced =>
          genDiag(dtpl).map(diag => embedDiagSvg(generator.generate(diag, tpl, this))).getOrElse(NoElems)
        case _ => NoElems
      }

    val typeHierarchy = ifDiags(_.inheritanceDiagram)(diagramDiv("Type Hierarchy", "inheritance-diagram"))
    val contentHierarchy = ifDiags(_.contentDiagram)(diagramDiv("Content Hierarchy", "content-diagram"))

    memberComment ++ authorComment ++ paramComments ++ attributesBlock ++ linearization ++ subclasses ++ typeHierarchy ++ contentHierarchy
  }

  def boundsToHtml(hi: Option[TypeEntity], lo: Option[TypeEntity], hasLinks: Boolean): Elems = {
    def bound0(bnd: Option[TypeEntity], pre: String): Elems = bnd match {
      case None => NoElems
      case Some(tpe) => Txt(pre) ++ typeToHtml(tpe, hasLinks)
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
  def signature(mbr: MemberEntity, isSelf: Boolean, isReduced: Boolean = false): Elems = {

    def inside(hasLinks: Boolean, nameLink: String = ""): Elems =
      Span(`class`= "modifier_kind", elems=
        Span(`class`= "modifier", elems= mbr.flags.flatMap(flag => inlineToHtml(flag.text) :+ Txt(" "))) ::
        Txt(" ") :: Span(`class`= "kind", elems= Txt(kindToString(mbr))) :: NoElems
      ) :: Txt(" ") ::
      Span(`class`="symbol", elems=
        {
          val nameClass =
            if (mbr.isImplicitlyInherited)
              if (mbr.isShadowedOrAmbiguousImplicit)
                "implicit shadowed"
              else
                "implicit"
            else
              "name"

          val nameHtml: Elem = {
            val value = if (mbr.isConstructor) tpl.name else mbr.name
            val (cls, titleDepr) = if (mbr.deprecation.isDefined)
              (nameClass + " deprecated", "Deprecated: "+bodyToStr(mbr.deprecation.get))
            else
              (nameClass, null)
            val encoded = scala.reflect.NameTransformer.encode(value)
            val title = if (encoded != value) {
              "gt4s: " + encoded + (if (titleDepr == null) "" else ". " + titleDepr)
            } else {
              titleDepr
            }
            Span(`class`= cls, title= title, elems=Txt(value))
          }
          if (!nameLink.isEmpty)
            A(title= memberToShortCommentTitleTag(mbr), href= nameLink, elems= nameHtml)
          else nameHtml
        } :: {
          def tparamsToHtml(mbr: Any): Elems = mbr match {
            case hk: HigherKinded =>
              val tpss = hk.typeParams
              if (tpss.isEmpty) NoElems else {
                def tparam0(tp: TypeParam): Elems =
                  Span(name= tp.name, elems= Txt(tp.variance + tp.name) :: tparamsToHtml(tp) ++ boundsToHtml(tp.hi, tp.lo, hasLinks))
                def tparams0(tpss: List[TypeParam]): Elems = (tpss: @unchecked) match {
                  case tp :: Nil => tparam0(tp)
                  case tp :: tps => tparam0(tp) ++ Txt(", ") ++ tparams0(tps)
                }
                Span(`class`= "tparams", elems= Txt("[") :: (tparams0(tpss) :+ Txt("]")))
              }
            case _ => NoElems
          }
          tparamsToHtml(mbr)
        } ++ {
        if (isReduced) NoElems else {
            def paramsToHtml(vlsss: List[List[ValueParam]]): Elems = {
              def param0(vl: ValueParam): Elems =
                // notice the }{ in the next lines, they are necessary to avoid an undesired whitespace in output
                Span (name= vl.name, elems=
                  Txt(vl.name) ::
                { Txt(": ") ++ typeToHtml(vl.resultType, hasLinks) } ++
                  (vl.defaultValue match {
                    case Some(v) => Txt(" = ") :: treeToHtml(v)
                    case None => NoElems
                  })
                )

              def params0(vlss: List[ValueParam]): Elems = vlss match {
                case Nil => NoElems
                case vl :: Nil => param0(vl)
                case vl :: vls => param0(vl) ++ Txt(", ") ++ params0(vls)
              }
              def implicitCheck(vlss: List[ValueParam]): Elems = vlss match {
                case vl :: vls => if(vl.isImplicit) { Span(`class`= "implicit", elems= Txt("implicit ")) } else Txt("")
                case _ => Txt("")
              }
              vlsss map { vlss => Span(`class`= "params", elems = Txt("(") :: implicitCheck(vlss) ++ params0(vlss) ++ Txt(")")) }
            }
            mbr match {
              case cls: Class => paramsToHtml(cls.valueParams)
              case ctr: Constructor => paramsToHtml(ctr.valueParams)
              case dfe: Def => paramsToHtml(dfe.valueParams)
              case _ => NoElems
            }
          }
        } ++ {if (isReduced) NoElems else {
          mbr match {
            case tme: MemberEntity if (tme.isDef || tme.isVal || tme.isLazyVal || tme.isVar) =>
              Span(`class`= "result", elems= Txt(": ") :: typeToHtml(tme.resultType, hasLinks) )

            case abt: MemberEntity with AbstractType =>
              val b2s = boundsToHtml(abt.hi, abt.lo, hasLinks)
              if (b2s != NoElems)
                Span(`class`= "result", elems= b2s )
              else NoElems

            case alt: MemberEntity with AliasType =>
              Span(`class`= "result alias", elems= Txt(" = ") :: typeToHtml(alt.alias, hasLinks) )

            case tpl: MemberTemplateEntity if tpl.parentTypes.nonEmpty =>
              Span(`class`= "result", elems= Txt(" extends ") :: typeToHtml(tpl.parentTypes.map(_._2), hasLinks) )

            case _ => NoElems
          }
        }}
      )

    mbr match {
      case dte: DocTemplateEntity if !isSelf =>
        permalink(dte, isSelf) :: Txt(" ") ++ { inside(hasLinks = true, nameLink = relativeLinkTo(dte)) }
      case _ if isSelf =>
        H(4, id="signature", `class`= "signature", elems= inside(hasLinks = true))
      case _ =>
        permalink(mbr) :: Txt(" ") ++ { inside(hasLinks = true) }
    }

  }

  /** */
  def treeToHtml(tree: TreeEntity): Elems = {

    /** Makes text good looking in the html page : newlines and basic indentation,
     * You must change this function if you want to improve pretty printing of default Values
     */
    def codeStringToXml(text: String): Elems = {
      var goodLookingXml: Elems = NoElems
      var indent = 0
      for (c <- text) c match {
        case '{' => indent+=1
          goodLookingXml ++= Txt("{")
        case '}' => indent-=1
          goodLookingXml ++= Txt("}")
        case '\n' =>
          goodLookingXml++= Br ++ indentation
        case _ => goodLookingXml ++= Txt(c.toString)
      }
      def indentation:Elems = {
        var indentXml = NoElems
        for (_ <- 1 to indent) indentXml ++= Txt("  ") // TODO: &nbsp;&nbsp;
        indentXml
      }
      goodLookingXml
    }

    var index = 0
    val str = tree.expression
    val length = str.length
    var myXml: Elems = NoElems
    for ((from, (member, to)) <- tree.refEntity.toSeq) {
      if (index < from) {
        myXml ++= codeStringToXml(str.substring(index,from))
        index = from
      }
      if (index == from) {
        member match {
          case mbr: DocTemplateEntity =>
            val link = relativeLinkTo(mbr)
            myXml ++= Span(`class`="name", elems= A(href=link, elems= Txt(str.substring(from, to))))
          case mbr: MemberEntity =>
            val anchor = "#" + mbr.signature
            val link = relativeLinkTo(mbr.inTemplate)
            myXml ++= Span(`class`="name", elems= A(href=link + anchor, elems= Txt(str.substring(from, to))))
        }
        index = to
      }
    }

    if (index <= length-1)
      myXml ++= codeStringToXml(str.substring(index, length ))

    if (length < 36)
      Span(`class`= "symbol", elems= myXml )
    else
      Span(`class`= "defval", elems= myXml ) // was buggy: <span class="defval" name={ myXml }>{ "..." }</span> -- TODO: handle overflow in CSS (as in #search > span#doc-title > span#doc-version )
  }

  private def argumentsToHtml(argss: List[ValueArgument]): Elems = {
    def argumentsToHtml0(argss: List[ValueArgument]): Elems = argss match {
      case Nil         => NoElems
      case arg :: Nil  => argumentToHtml(arg)
      case arg :: args => argumentToHtml(arg) ++ Txt(", ") ++ argumentsToHtml0(args)
    }
    Span(`class`= "args", elems= (Txt("(") :: argumentsToHtml0(argss)) :+ Txt(")"))
  }

  private def argumentToHtml(arg: ValueArgument): Elems = {
    Span(elems=
      arg.parameter match {
        case Some(param) => Txt(param.name + " = ") :: treeToHtml(arg.value)
        case None        => treeToHtml(arg.value)
      }
    )
  }

  private def bodyToStr(body: comment.Body): String =
    body.blocks flatMap blockToStr mkString ""

  private def blockToStr(block: comment.Block): String = block match {
    case comment.Paragraph(in) => Page.inlineToStr(in)
    case _ => block.toString
  }

  private def constraintToHtml(constraint: Constraint): Elems = constraint match {
    case ktcc: KnownTypeClassConstraint =>
      Txt(ktcc.typeExplanation(ktcc.typeParamName) + " (" + ktcc.typeParamName + ": ") ++
        templateToHtml(ktcc.typeClassEntity) ++ Txt(")")
    case tcc: TypeClassConstraint =>
      Txt(tcc.typeParamName + " is ") ++
        A(href="http://stackoverflow.com/questions/2982276/what-is-a-context-bound-in-scala", target="_blank", elems=
        Txt("context-bounded")) ++ Txt(" by " + tcc.typeClassEntity.qualifiedName + " (" + tcc.typeParamName + ": ") ++
        templateToHtml(tcc.typeClassEntity) ++ Txt(")")
    case impl: ImplicitInScopeConstraint =>
      Txt("an implicit value of type ") ++ typeToHtml(impl.implicitType, hasLinks = true) ++ Txt(" is in scope")
    case eq: EqualTypeParamConstraint =>
      Txt(eq.typeParamName + " is " + eq.rhs.name + " (" + eq.typeParamName + " =:= ") ++
        typeToHtml(eq.rhs, hasLinks = true) ++ Txt(")")
    case bt: BoundedTypeParamConstraint =>
      Txt(bt.typeParamName + " is a superclass of " + bt.lowerBound.name + " and a subclass of " +
        bt.upperBound.name + " (" + bt.typeParamName + " >: ") ++
        typeToHtml(bt.lowerBound, hasLinks = true) ++ Txt(" <: ") ++
        typeToHtml(bt.upperBound, hasLinks = true) ++ Txt(")")
    case lb: LowerBoundedTypeParamConstraint =>
      Txt(lb.typeParamName + " is a superclass of " + lb.lowerBound.name + " (" + lb.typeParamName + " >: ") ++
        typeToHtml(lb.lowerBound, hasLinks = true) ++ Txt(")")
    case ub: UpperBoundedTypeParamConstraint =>
      Txt(ub.typeParamName + " is a subclass of " + ub.upperBound.name + " (" + ub.typeParamName + " <: ") ++
        typeToHtml(ub.upperBound, hasLinks = true) ++ Txt(")")
  }
}

object EntityPage {
  def apply(
    uni: doc.Universe,
    gen: DiagramGenerator,
    docTpl: DocTemplateEntity,
    rep: Reporter
  ): EntityPage = new EntityPage {
    def universe = uni
    def generator = gen
    def tpl = docTpl
    def docletReporter = rep
  }

  /* Vlad: Lesson learned the hard way: don't put any stateful code that references the model here,
   * it won't be garbage collected and you'll end up filling the heap with garbage */
  def lowerFirstLetter(s: String) = if (s.length >= 1) s.substring(0,1).toLowerCase() + s.substring(1) else s
}
