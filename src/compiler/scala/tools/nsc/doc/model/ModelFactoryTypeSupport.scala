/* NSC -- new Scala compiler -- Copyright 2007-2011 LAMP/EPFL */

package scala.tools.nsc
package doc
package model

import comment._

import diagram._

import scala.collection._
import scala.util.matching.Regex

import symtab.Flags

import io._

import model.{ RootPackage => RootPackageEntity }

/** This trait extracts all required information for documentation from compilation units */
trait ModelFactoryTypeSupport {
  thisFactory: ModelFactory
               with ModelFactoryImplicitSupport
               with ModelFactoryTypeSupport
               with DiagramFactory
               with CommentFactory
               with TreeFactory =>

  import global._
  import definitions.{ ObjectClass, NothingClass, AnyClass, AnyValClass, AnyRefClass }
  import rootMirror.{ RootPackage, RootClass, EmptyPackage }

  protected var typeCache = new mutable.LinkedHashMap[(Type, TemplateImpl), TypeEntity]
  protected var typeCacheNoPrefix = new mutable.LinkedHashMap[Type, TypeEntity]

  /** */
  def makeType(aType: Type, inTpl: TemplateImpl): TypeEntity = {
    def templatePackage = closestPackage(inTpl.sym)

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
        case tp: TypeRef if definitions.isFunctionType(tp) =>
          val args = tp.normalize.typeArgs
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
        case tp: TypeRef if definitions.isTupleType(tp) =>
          val args = tp.normalize.typeArgs
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
          // (2) if we don't generate the doc template, we should at least indicate the correct prefix in the tooltip
          val bSym = normalizeTemplate(aSym)
          val owner =
            if ((preSym != NoSymbol) &&                  /* it needs a prefix */
                (preSym != bSym.owner) &&                /* prefix is different from owner */
                // ((preSym != inTpl.sym) &&             /* prevent prefixes from being shown inside the defining class */
                // (preSym != inTpl.sym.moduleClass)) && /* or object */
                (aSym == bSym))                          /* normalization doesn't play tricks on us */
              preSym
            else
              bSym.owner

          val bTpl = findTemplateMaybe(bSym)
          val link =
            if (owner == bSym.owner && bTpl.isDefined)
              // (0) the owner's class is linked AND has a template - lovely
              LinkToTpl(bTpl.get)
            else {
              val oTpl = findTemplateMaybe(owner)
              val bMbr = oTpl.map(findMember(bSym, _))
              if (oTpl.isDefined && bMbr.isDefined && bMbr.get.isDefined)
                // (1) the owner's class
                LinkToMember(bMbr.get.get, oTpl.get) //ugh
              else
                // (2) if we still couldn't find the owner, make a noDocTemplate for everything (in the correct owner!)
                LinkToTpl(makeTemplate(bSym, Some(makeTemplate(owner))))
            }

          // SI-4360 Showing prefixes when necessary
          // We check whether there's any directly accessible type with the same name in the current template OR if the
          // type is inherited from one template to another. There may be multiple symbols with the same name in scope,
          // but we won't show the prefix if our symbol is among them, only if *it's not* -- that's equal to showing
          // the prefix only for ambiguous references, not for overloaded ones.
          def needsPrefix: Boolean = {
            if (owner != bSym.owner && (normalizeTemplate(owner) != inTpl.sym))
              return true

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
            if (!settings.docNoPrefixes.value && needsPrefix && (bSym != AnyRefClass /* which we normalize */)) {
              val qualifiedName = makeQualifiedName(owner, Some(inTpl.sym))
              if (qualifiedName != "") qualifiedName + "." else ""
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
//          throw new Error("Polymorphic type '" + tpe + "' cannot be printed as a type")
          def typeParamsToString(tps: List[Symbol]): String = if (tps.isEmpty) "" else
            tps.map{tparam =>
              tparam.varianceString + tparam.name + typeParamsToString(tparam.typeParams)
            }.mkString("[", ", ", "]")
          nameBuffer append typeParamsToString(tparams)
          appendType0(result)
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
    val cached =
      if (!settings.docNoPrefixes.value)
        typeCache.get((aType, inTpl))
      else
        typeCacheNoPrefix.get(aType)

    cached match {
      case Some(typeEntity) => typeEntity
      case None =>
        val typeEntity = createTypeEntity
        if (!settings.docNoPrefixes.value)
          typeCache += (aType, inTpl) -> typeEntity
        else
          typeCacheNoPrefix += aType -> typeEntity
        typeEntity
    }
  }
}