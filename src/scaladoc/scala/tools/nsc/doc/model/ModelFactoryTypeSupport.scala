/* NSC -- new Scala compiler -- Copyright 2007-2013 LAMP/EPFL */

package scala.tools.nsc
package doc
package model

import base._
import diagram._

import scala.collection._

/** This trait extracts all required information for documentation from compilation units */
trait ModelFactoryTypeSupport {
  thisFactory: ModelFactory
               with ModelFactoryImplicitSupport
               with ModelFactoryTypeSupport
               with DiagramFactory
               with CommentFactory
               with TreeFactory
               with MemberLookup =>

  import global._
  import definitions.{ ObjectClass, AnyClass, AnyRefClass }

  protected val typeCache = new mutable.LinkedHashMap[Type, TypeEntity]

  /** */
  def makeType(aType: Type, inTpl: TemplateImpl): TypeEntity = {
    def createTypeEntity = new TypeEntity {
      private var nameBuffer = new StringBuilder
      private var refBuffer = new immutable.TreeMap[Int, (LinkTo, Int)]
      private def appendTypes0(types: List[Type], sep: String): Unit = types match {
        case Nil =>
        case tp :: Nil =>
          appendType0(tp)
        case tp :: tps =>
          appendType0(tp)
          nameBuffer append sep
          appendTypes0(tps, sep)
      }

      private def appendType0(tpe: Type): Unit = tpe match {
        /* Type refs */
        case tp: TypeRef if definitions.isFunctionTypeDirect(tp) =>
          val args = tp.typeArgs
          nameBuffer append '('
          appendTypes0(args.init, ", ")
          nameBuffer append ") ⇒ "
          appendType0(args.last)
        case tp: TypeRef if definitions.isScalaRepeatedParamType(tp) =>
          appendType0(tp.args.head)
          nameBuffer append '*'
        case tp: TypeRef if definitions.isByNameParamType(tp) =>
          nameBuffer append "⇒ "
          appendType0(tp.args.head)
        case tp: TypeRef if definitions.isTupleTypeDirect(tp) =>
          val args = tp.typeArgs
          nameBuffer append '('
          appendTypes0(args, ", ")
          nameBuffer append ')'
        case TypeRef(pre, aSym, targs) =>
          val preSym = pre.widen.typeSymbol

          // SI-3314/SI-4888: Classes, Traits and Types can be inherited from a template to another:
          // class Enum { abstract class Value }
          // class Day extends Enum { object Mon extends Value /*...*/ }
          // ===> in such cases we have two options:
          // (0) if there's no inheritance taking place (Enum#Value) we can link to the template directly
          // (1) if we generate the doc template for Day, we can link to the correct member
          // (2) If the symbol comes from an external library for which we know the documentation URL, point to it.
          // (3) if we don't generate the doc template, we should at least indicate the correct prefix in the tooltip
          val bSym = normalizeTemplate(aSym)
          val owner =
            if ((preSym != NoSymbol) &&                  /* it needs a prefix */
                (preSym != bSym.owner) &&                /* prefix is different from owner */
                (aSym == bSym))                          /* normalization doesn't play tricks on us */
              preSym
            else
              bSym.owner

          val link =
            findTemplateMaybe(bSym) match {
              case Some(bTpl) if owner == bSym.owner =>
                // (0) the owner's class is linked AND has a template - lovely
                bTpl match {
                  case dtpl: DocTemplateEntity => new LinkToTpl(dtpl)
                  case _ => new Tooltip(bTpl.qualifiedName)
                }
              case _ =>
                val oTpl = findTemplateMaybe(owner)
                (oTpl, oTpl flatMap (findMember(bSym, _))) match {
                  case (Some(oTpl), Some(bMbr)) =>
                    // (1) the owner's class
                    LinkToMember(bMbr, oTpl)
                  case _ =>
                    val name = makeQualifiedName(bSym)
                    if (!bSym.owner.hasPackageFlag)
                      Tooltip(name)
                    else
                      findExternalLink(bSym, name).getOrElse (
                        // (3) if we couldn't find neither the owner nor external URL to link to, show a tooltip with the qualified name
                        Tooltip(name)
                      )
                }
            }

          // SI-4360 Showing prefixes when necessary
          // We check whether there's any directly accessible type with the same name in the current template OR if the
          // type is inherited from one template to another. There may be multiple symbols with the same name in scope,
          // but we won't show the prefix if our symbol is among them, only if *it's not* -- that's equal to showing
          // the prefix only for ambiguous references, not for overloaded ones.
          def needsPrefix: Boolean = {
            if ((owner != bSym.owner || preSym.isRefinementClass) && (normalizeTemplate(owner) != inTpl.sym))
              return true
            // don't get tricked into prefixing method type params and existentials:
            // I tried several tricks BUT adding the method for which I'm creating the type => that simply won't scale,
            // as ValueParams are independent of their parent member, and I really don't want to add this information to
            // all terms, as we're already over the allowed memory footprint
            if (aSym.isTypeParameterOrSkolem || aSym.isExistentiallyBound /* existential or existential skolem */)
              return false

            for (tpl <- inTpl.sym.ownerChain) {
              tpl.info.member(bSym.name) match {
                case NoSymbol =>
                  // No syms with that name, look further inside the owner chain
                case sym =>
                  // Symbol found -- either the correct symbol, another one OR an overloaded alternative
                  if (sym == bSym)
                    return false
                  else sym.info match {
                    case OverloadedType(owner, alternatives) =>
                      return alternatives.contains(bSym)
                    case _ =>
                      return true
                  }
              }
            }
            // if it's not found in the owner chain, we can safely leave out the prefix
            false
          }

          val prefix =
            if (!settings.docNoPrefixes && needsPrefix && (bSym != AnyRefClass /* which we normalize */)) {
              if (!owner.isRefinementClass) {
                val qName = makeQualifiedName(owner, Some(inTpl.sym))
                if (qName != "") qName + "." else ""
              }
              else {
                nameBuffer append "("
                appendType0(pre)
                nameBuffer append ")#"
                "" // we already appended the prefix
              }
            } else ""

          //DEBUGGING:
          //if (makeQualifiedName(bSym) == "pack1.A") println("needsPrefix(" + bSym + ", " + owner + ", " + inTpl.qualifiedName + ") => " + needsPrefix + "  and prefix=" + prefix)

          val name = prefix + bSym.nameString
          val pos0 = nameBuffer.length
          refBuffer += pos0 -> ((link, name.length))
          nameBuffer append name

          if (!targs.isEmpty) {
            nameBuffer append '['
            appendTypes0(targs, ", ")
            nameBuffer append ']'
          }
        /* Refined types */
        case RefinedType(parents, defs) =>
          val ignoreParents = Set[Symbol](AnyClass, ObjectClass)
          val filtParents = parents filterNot (x => ignoreParents(x.typeSymbol)) match {
            case Nil    => parents
            case ps     => ps
          }
          appendTypes0(filtParents, " with ")
          // XXX Still todo: properly printing refinements.
          // Since I didn't know how to go about displaying a multi-line type, I went with
          // printing single method refinements (which should be the most common) and printing
          // the number of members if there are more.
          defs.toList match {
            case Nil      => ()
            case x :: Nil => nameBuffer append (" { " + x.defString + " }")
            case xs       => nameBuffer append (" { ... /* %d definitions in type refinement */ }" format xs.size)
          }
        /* Eval-by-name types */
        case NullaryMethodType(result) =>
          nameBuffer append '⇒'
          appendType0(result)

        /* Polymorphic types */
        case PolyType(tparams, result) => assert(tparams.nonEmpty)
          def typeParamsToString(tps: List[Symbol]): String = if (tps.isEmpty) "" else
            tps.map{tparam =>
              tparam.varianceString + tparam.name + typeParamsToString(tparam.typeParams)
            }.mkString("[", ", ", "]")
          nameBuffer append typeParamsToString(tparams)
          appendType0(result)

        case et@ExistentialType(quantified, underlying) =>

          def appendInfoStringReduced(sym: Symbol, tp: Type): Unit = {
            if (sym.isType && !sym.isAliasType && !sym.isClass) {
                tp match {
                  case PolyType(tparams, _) =>
                    nameBuffer append "["
                    appendTypes0(tparams.map(_.tpe), ", ")
                    nameBuffer append "]"
                  case _ =>
                }
                tp.resultType match {
                  case rt @ TypeBounds(_, _) =>
                    appendType0(rt)
                  case rt                    =>
                    nameBuffer append " <: "
                    appendType0(rt)
                }
            } else {
              // fallback to the Symbol infoString
              nameBuffer append sym.infoString(tp)
            }
          }

          def appendClauses = {
            nameBuffer append " forSome {"
            var first = true
            for (sym <- quantified) {
              if (!first) { nameBuffer append ", " } else first = false
              if (sym.isSingletonExistential) {
                nameBuffer append "val "
                nameBuffer append tpnme.dropSingletonName(sym.name)
                nameBuffer append ": "
                appendType0(dropSingletonType(sym.info.bounds.hi))
              } else {
                if (sym.flagString != "") nameBuffer append (sym.flagString + " ")
                if (sym.keyString != "") nameBuffer append (sym.keyString + " ")
                nameBuffer append sym.varianceString
                nameBuffer append sym.nameString
                appendInfoStringReduced(sym, sym.info)
              }
            }
            nameBuffer append "}"
          }

          underlying match {
            case TypeRef(pre, sym, args) if et.isRepresentableWithWildcards =>
              appendType0(typeRef(pre, sym, Nil))
              nameBuffer append "["
              var first = true
              val qset = quantified.toSet
              for (arg <- args) {
                if (!first) { nameBuffer append ", " } else first = false
                arg match {
                  case TypeRef(_, sym, _) if (qset contains sym) =>
                    nameBuffer append "_"
                    appendInfoStringReduced(sym, sym.info)
                  case arg =>
                    appendType0(arg)
                }
              }
              nameBuffer append "]"
            case MethodType(_, _) | NullaryMethodType(_) | PolyType(_, _) =>
              nameBuffer append "("
              appendType0(underlying)
              nameBuffer append ")"
              appendClauses
            case _ =>
              appendType0(underlying)
              appendClauses
          }

        case tb@TypeBounds(lo, hi) =>
          if (tb.lo != TypeBounds.empty.lo) {
            nameBuffer append " >: "
            appendType0(lo)
          }
          if (tb.hi != TypeBounds.empty.hi) {
            nameBuffer append " <: "
            appendType0(hi)
          }
        // case tpen: ThisType | SingleType | SuperType =>
        //   if (tpen.isInstanceOf[ThisType] && tpen.asInstanceOf[ThisType].sym.isEffectiveRoot) {
        //     appendType0 typeRef(NoPrefix, sym, Nil)
        //   } else {
        //     val underlying =
        //     val pre = underlying.typeSymbol.skipPackageObject
        //     if (pre.isOmittablePrefix) pre.fullName + ".type"
        //     else prefixString + "type"
        case tpen@ThisType(sym) =>
          appendType0(typeRef(NoPrefix, sym, Nil))
          nameBuffer append ".this"
          if (!tpen.underlying.typeSymbol.skipPackageObject.isOmittablePrefix) nameBuffer append ".type"
        case tpen@SuperType(thistpe, supertpe) =>
          nameBuffer append "super["
          appendType0(supertpe)
          nameBuffer append "]"
        case tpen@SingleType(pre, sym) =>
          appendType0(typeRef(pre, sym, Nil))
          if (!tpen.underlying.typeSymbol.skipPackageObject.isOmittablePrefix) nameBuffer append ".type"
        case tpen =>
          nameBuffer append tpen.toString
      }
      appendType0(aType)
      val refEntity = refBuffer
      val name = optimize(nameBuffer.toString)
      nameBuffer = null
    }

    // SI-4360: Entity caching depends on both the type AND the template it's in, as the prefixes might change for the
    // same type based on the template the type is shown in.
    if (settings.docNoPrefixes)
      typeCache.getOrElseUpdate(aType, createTypeEntity)
    else createTypeEntity
  }
}
