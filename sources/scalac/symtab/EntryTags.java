/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id: EntryTags.java

package scalac.symtab;

public interface EntryTags {

/***************************************************
 * Symbol table attribute format:
 *   Symtab         = nentries_Nat {Entry}
 *   Entry          = 1 TERMNAME len_Nat NameInfo
 *                  | 2 TYPENAME len_Nat NameInfo
 *                  | 3 NUMBER lenNat NumInfo
 *                  | 4 NONEsym len_Nat
 *                  | 5 TYPEsym len_Nat SymbolInfo lobound_Ref
 *                  | 6 ALIASsym len_Nat SymbolInfo constrsym_Ref
 *                  | 7 CLASSsym len_Nat SymbolInfo thistype_Ref constrsym_Ref
 *                  | 8 VALsym len_Nat SymbolInfo [classsym_Ref]
 *                  | 9 EXTref len_Nat name_Ref [owner_Ref]
 *                  | 10 EXTMODCLASSref len_Nat name_Ref [owner_Ref]
 *                  | 11 NOtpe len_Nat
 *                  | 12 THIStpe len_Nat sym_Ref
 *                  | 13 SINGLEtpe len_Nat type_Ref sym_Ref
 *                  | 14 CONSTANTtpe len_Nat type_Ref value_Ref
 *                  | 15 TYPEREFtpe len_Nat type_Ref sym_Ref {targ_Ref}
 *                  | 16 COMPOUNDtpe len_Nat classsym_Ref {tpe_Ref}
 *                  | 17 METHODtpe len_Nat tpe_Ref {tpe_Ref}
 *                  | 18 POLYTtpe len_Nat tpe_Ref {sym_Ref}
 *                  | 19 OVERLOADEDtpe len_Nat {sym_Ref} {tpe_Ref}
 *                  | 22 FLAGGEDtpe len_Nat flags_Nat tpe_Ref
 *   SymbolInfo     = name_Ref owner_Ref flags_Nat info_Ref
 *   NameInfo       = <character sequence of length len_Nat in Utf8 format>
 *   NumInfo        = <len_Nat-byte signed number in big endian format>
 *   Ref            = Nat
 *
 *   len is remaining length after `len'.
 */

    int TERMname = 1,
	TYPEname = 2,
	NUMBER = 3,
	NONEsym = 4,
	TYPEsym = 5,
	ALIASsym = 6,
	CLASSsym = 7,
	VALsym = 8,
	EXTref = 9,
	EXTMODCLASSref = 10,
	NOtpe = 11,
	THIStpe = 12,
	SINGLEtpe = 13,
	CONSTANTtpe = 14,
	TYPEREFtpe = 15,
	COMPOUNDtpe = 16,
	METHODtpe = 17,
	POLYtpe = 18,
	OVERLOADEDtpe = 19,
	UNBOXEDtpe = 20,
        UNBOXEDARRAYtpe = 21,
	FLAGGEDtpe = 22,
	ERRORtpe = 23;

    int firstSymTag = NONEsym, lastSymTag = VALsym;
    int firstTypeTag = NOtpe, lastTypeTag = FLAGGEDtpe;

// flag encodings

    int REPEATEDflag = 4, DEFflag = 8;
}
