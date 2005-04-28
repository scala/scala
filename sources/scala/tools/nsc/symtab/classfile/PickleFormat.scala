/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab.classfile;

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
 *                  | 8 VALsym len_Nat SymbolInfo
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
 *                  | 35 LITERALzero len_Nat
 *   SymbolInfo     = name_Ref owner_Ref flags_Nat info_Ref
 *   NameInfo       = <character sequence of length len_Nat in Utf8 format>
 *   NumInfo        = <len_Nat-byte signed number in big endian format>
 *   Ref            = Nat
 *
 *   len is remaining length after `len'.
 */
  val TERMname = 1;
  val TYPEname = 2;
  val NONEsym = 3;
  val TYPEsym = 4;
  val ALIASsym = 5;
  val CLASSsym = 6;
  val MODULEsym = 7;
  val VALsym = 8;
  val EXTref = 9;
  val EXTMODCLASSref = 10;
  val NOtpe = 11;
  val NOPREFIXtpe = 12;
  val THIStpe = 13;
  val SINGLEtpe = 14;
  val CONSTANTtpe = 15;
  val TYPEREFtpe = 16;
  val TYPEBOUNDStpe = 17;
  val REFINEDtpe = 18;
  val CLASSINFOtpe = 19;
  val METHODtpe = 20;
  val POLYtpe = 21;
  val IMPLICITMETHODtpe = 22;
  val LITERALunit = 24;
  val LITERALboolean = 25;
  val LITERALbyte = 26;
  val LITERALshort = 27;
  val LITERALchar = 28;
  val LITERALint = 29;
  val LITERALlong = 30;
  val LITERALfloat = 31;
  val LITERALdouble = 32;
  val LITERALstring = 33;
  val LITERALnull = 34;
  val LITERALzero = 35;

  val firstSymTag = NONEsym;
  val lastSymTag = VALsym;
  val firstTypeTag = NOtpe;
  val lastTypeTag = POLYtpe;
}
