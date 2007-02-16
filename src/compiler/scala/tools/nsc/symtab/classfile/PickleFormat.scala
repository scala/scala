/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

/** This object provides constants for pickling attributes.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object PickleFormat {

/***************************************************
 * Symbol table attribute format:
 *   Symtab         = nentries_Nat {Entry}
 *   Entry          = 1 TERMNAME len_Nat NameInfo
 *                  | 2 TYPENAME len_Nat NameInfo
 *                  | 3 NONEsym len_Nat
 *                  | 4 TYPEsym len_Nat SymbolInfo
 *                  | 5 ALIASsym len_Nat SymbolInfo
 *                  | 6 CLASSsym len_Nat SymbolInfo [thistype_Ref]
 *                  | 7 MODULEsym len_Nat SymbolInfo
 *                  | 8 VALsym len_Nat SymbolInfo [alias_Ref]
 *                  | 9 EXTref len_Nat name_Ref [owner_Ref]
 *                  | 10 EXTMODCLASSref len_Nat name_Ref [owner_Ref]
 *                  | 11 NOtpe len_Nat
 *                  | 12 NOPREFIXtpe len_Nat
 *                  | 13 THIStpe len_Nat sym_Ref
 *                  | 14 SINGLEtpe len_Nat type_Ref sym_Ref
 *                  | 15 CONSTANTtpe len_Nat type_Ref constant_Ref
 *                  | 16 TYPEREFtpe len_Nat type_Ref sym_Ref {targ_Ref}
 *                  | 17 TYPEBOUNDStpe len_Nat tpe_Ref tpe_Ref
 *                  | 18 REFINEDtpe len_Nat classsym_Ref {tpe_Ref}
 *                  | 19 CLASSINFOtpe len_Nat classsym_Ref {tpe_Ref}
 *                  | 20 METHODtpe len_Nat tpe_Ref {tpe_Ref}
 *                  | 21 POLYTtpe len_Nat tpe_Ref {sym_Ref}
 *                  | 22 IMPLICITMETHODtpe len_Nat tpe_Ref {tpe_Ref}
 *                  | 24 LITERALunit len_Nat
 *                  | 25 LITERALboolean len_Nat value_Long
 *                  | 26 LITERALbyte len_Nat value_Long
 *                  | 27 LITERALshort len_Nat value_Long
 *                  | 28 LITERALchar len_Nat value_Long
 *                  | 29 LITERALint len_Nat value_Long
 *                  | 30 LITERALlong len_Nat value_Long
 *                  | 31 LITERALfloat len_Nat value_Long
 *                  | 32 LITERALdouble len_Nat value_Long
 *                  | 33 LITERALstring len_Nat name_Ref
 *                  | 34 LITERALnull len_Nat
 *                  | 35 LITERALclass len_Nat type_Ref
 *                  | 40 ATTRIBUTE len_Nat sym_Ref info_Ref {constant_Ref} {nameRef constantRef}
 *                  | 41 CHILDREN len_Nat sym_Ref {sym_Ref}
 *                  | 68 PosTYPEsym len_Nat pos_Nat SymbolInfo
 *                  | 69 PosALIASsym len_Nat pos_Nat SymbolInfo
 *                  | 70 PosCLASSsym len_Nat pos_Nat SymbolInfo [thistype_Ref]
 *                  | 71 PosMODULEsym len_Nat pos_Nat SymbolInfo
 *                  | 72 PosVALsym len_Nat pos_Nat SymbolInfo [alias_Ref]
 *   SymbolInfo     = name_Ref owner_Ref flags_Nat [privateWithin_Ref] info_Ref
 *   NameInfo       = <character sequence of length len_Nat in Utf8 format>
 *   NumInfo        = <len_Nat-byte signed number in big endian format>
 *   Ref            = Nat
 *
 *   len is remaining length after `len'.
 */
  val MajorVersion = 4
  val MinorVersion = 0

  final val TERMname = 1
  final val TYPEname = 2
  final val NONEsym = 3
  final val TYPEsym = 4
  final val ALIASsym = 5
  final val CLASSsym = 6
  final val MODULEsym = 7
  final val VALsym = 8
  final val EXTref = 9
  final val EXTMODCLASSref = 10
  final val NOtpe = 11
  final val NOPREFIXtpe = 12
  final val THIStpe = 13
  final val SINGLEtpe = 14
  final val CONSTANTtpe = 15
  final val TYPEREFtpe = 16
  final val TYPEBOUNDStpe = 17
  final val REFINEDtpe = 18
  final val CLASSINFOtpe = 19
  final val METHODtpe = 20
  final val POLYtpe = 21
  final val IMPLICITMETHODtpe = 22
  final val LITERAL = 23   // base line for literals
  final val LITERALunit = 24
  final val LITERALboolean = 25
  final val LITERALbyte = 26
  final val LITERALshort = 27
  final val LITERALchar = 28
  final val LITERALint = 29
  final val LITERALlong = 30
  final val LITERALfloat = 31
  final val LITERALdouble = 32
  final val LITERALstring = 33
  final val LITERALnull = 34
  final val LITERALclass = 35
  final val ATTRIBUTE = 40
  final val CHILDREN = 41
  final val firstSymTag = NONEsym
  final val lastSymTag = VALsym
  final val lastExtSymTag = EXTMODCLASSref
  final val firstTypeTag = NOtpe
  final val lastTypeTag = POLYtpe
  final val PosOffset = 64

  final val PosTYPEsym  = PosOffset + TYPEsym
  final val PosALIASsym = PosOffset + ALIASsym
  final val PosCLASSsym = PosOffset + CLASSsym
  final val PosMODULEsym = PosOffset + MODULEsym
  final val PosVALsym = PosOffset + VALsym
}
