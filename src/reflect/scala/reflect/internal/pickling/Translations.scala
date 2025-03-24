/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal
package pickling

import PickleFormat._
import scala.annotation.tailrec
import util.shortClassOfInstance

trait Translations {
  self: SymbolTable =>

  def isTreeSymbolPickled(code: Int): Boolean = (code: @annotation.switch) match {
    case PACKAGEtree | CLASStree | MODULEtree | VALDEFtree | DEFDEFtree | TYPEDEFtree | LABELtree => true
    case IMPORTtree | TEMPLATEtree | BINDtree | FUNCTIONtree | RETURNtree                         => true
    case APPLYDYNAMICtree | SUPERtree | THIStree | SELECTtree | IDENTtree                         => true
    case _                                                                                        => false
  }
  /** This method should be equivalent to tree.hasSymbolField, but that method
   *  doesn't do us any good when we're unpickling because we need to know based
   *  on the Int tag - the tree doesn't exist yet. Thus, this method is documentation only.
   */
  def isTreeSymbolPickled(tree: Tree): Boolean = isTreeSymbolPickled(picklerSubTag(tree))

  // The ad hoc pattern matching of tuples out of AnyRefs is a
  // truly terrible idea. It reaches the height of its powers in
  // combination with scala's insistence on helpfully tupling
  // multiple arguments passed to a single-arg AnyRef.
  final def picklerTag(ref: AnyRef): Int = ref match {
    case tp: Type                       => picklerTag(tp)
    case sym: Symbol                    => picklerTag(sym)
    case const: Constant                => LITERAL + const.tag
    case _: Tree                        => TREE           // its sub tag more precisely identifies it
    case _: TermName                    => TERMname
    case _: TypeName                    => TYPEname
    case _: ArrayAnnotArg               => ANNOTARGARRAY  // an array of annotation arguments
    case _: AnnotationInfo              => ANNOTINFO      // annotations on types (not linked to a symbol)
    case (_: Symbol, _: AnnotationInfo) => SYMANNOT       // symbol annotations, i.e. on terms
    case (_: Symbol, _: List[_])        => CHILDREN       // the direct subclasses of a sealed symbol
    case _: Modifiers                   => MODIFIERS
    case _                              => throw new IllegalStateException(s"unpicklable entry ${shortClassOfInstance(ref)} $ref")
  }

  /** Local symbols only. The assessment of locality depends
   *  on convoluted conditions which depends in part on the root
   *  symbol being pickled, so it cannot be reproduced here.
   *  The pickler tags at stake are EXTMODCLASSref and EXTref.
   *  Those tags are never produced here - such symbols must be
   *  excluded prior to calling this method.
   */
  def picklerTag(sym: Symbol): Int = sym match {
    case NoSymbol                            => NONEsym
    case _: ClassSymbol                      => CLASSsym
    case _: TypeSymbol if sym.isAbstractType => TYPEsym
    case _: TypeSymbol                       => ALIASsym
    case _: TermSymbol if sym.isModule       => MODULEsym
    case _: TermSymbol                       => VALsym
    case x                                   => throw new MatchError(x)
  }

  @tailrec
  final def picklerTag(tpe: Type): Int = tpe match {
    case NoType                        => NOtpe
    case NoPrefix                      => NOPREFIXtpe
    case _: ThisType                   => THIStpe
    case _: SingleType                 => SINGLEtpe
    case _: SuperType                  => SUPERtpe
    case _: ConstantType               => CONSTANTtpe
    case _: TypeBounds                 => TYPEBOUNDStpe
    case _: TypeRef                    => TYPEREFtpe
    case _: RefinedType                => REFINEDtpe
    case _: ClassInfoType              => CLASSINFOtpe
    case _: MethodType                 => METHODtpe
    case _: PolyType                   => POLYtpe
    case _: NullaryMethodType          => POLYtpe  // bad juju, distinct ints are not at a premium!
    case _: ExistentialType            => EXISTENTIALtpe
    case StaticallyAnnotatedType(_, _) => ANNOTATEDtpe
    case _: AnnotatedType              => picklerTag(tpe.underlying)
    case x                             => throw new MatchError(x)
  }

  def picklerSubTag(tree: Tree): Int = tree match {
    case EmptyTree              => EMPTYtree
    case _: PackageDef          => PACKAGEtree
    case _: ClassDef            => CLASStree
    case _: ModuleDef           => MODULEtree
    case _: ValDef              => VALDEFtree
    case _: DefDef              => DEFDEFtree
    case _: TypeDef             => TYPEDEFtree
    case _: LabelDef            => LABELtree
    case _: Import              => IMPORTtree
    // case _: DocDef              => DOCDEFtree
    case _: Template            => TEMPLATEtree
    case _: Block               => BLOCKtree
    case _: CaseDef             => CASEtree
    case _: Alternative         => ALTERNATIVEtree
    case _: Star                => STARtree
    case _: Bind                => BINDtree
    case _: UnApply             => UNAPPLYtree
    case _: ArrayValue          => ARRAYVALUEtree
    case _: Function            => FUNCTIONtree
    case _: Assign              => ASSIGNtree
    case _: If                  => IFtree
    case _: Match               => MATCHtree
    case _: Return              => RETURNtree
    case _: Try                 => TREtree     // TREtree?
    case _: Throw               => THROWtree
    case _: New                 => NEWtree
    case _: Typed               => TYPEDtree
    case _: TypeApply           => TYPEAPPLYtree
    case _: Apply               => APPLYtree
    case _: ApplyDynamic        => APPLYDYNAMICtree
    case _: Super               => SUPERtree
    case _: This                => THIStree
    case _: Select              => SELECTtree
    case _: Ident               => IDENTtree
    case _: Literal             => LITERALtree
    case _: TypeTree            => TYPEtree
    case _: Annotated           => ANNOTATEDtree
    case _: SingletonTypeTree   => SINGLETONTYPEtree
    case _: SelectFromTypeTree  => SELECTFROMTYPEtree
    case _: CompoundTypeTree    => COMPOUNDTYPEtree
    case _: AppliedTypeTree     => APPLIEDTYPEtree
    case _: TypeBoundsTree      => TYPEBOUNDStree
    case _: ExistentialTypeTree => EXISTENTIALTYPEtree
    case x                      => throw new MatchError(x)
  }
}

