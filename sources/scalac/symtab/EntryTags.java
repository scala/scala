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
 *                  | 3 NONEsym len_Nat
 *                  | 4 TYPEsym len_Nat SymbolInfo lobound_Ref
 *                  | 5 ALIASsym len_Nat SymbolInfo
 *                  | 6 CLASSsym len_Nat SymbolInfo thistype_Ref constrsym_Ref
 *                  | 7 VALsym len_Nat SymbolInfo [classsym_Ref]
 *                  | 8 EXTref len_Nat name_Ref [owner_Ref]
 *                  | 9 EXTMODCLASSref len_Nat name_Ref [owner_Ref]
 *                  | 10 NOtpe len_Nat
 *                  | 11 THIStpe len_Nat sym_Ref
 *                  | 12 SINGLEtpe len_Nat type_Ref sym_Ref
 *                  | 13 TYPEREFtpe len_Nat type_Ref sym_Ref {targ_Ref}
 *                  | 14 COMPOUNDtpe len_Nat classsym_Ref {tpe_Ref}
 *                  | 15 METHODtpe len_Nat tpe_Ref {tpe_Ref}
 *                  | 16 POLYTtpe len_Nat tpe_Ref {sym_Ref}
 *                  | 17 OVERLOADEDtpe len_Nat {sym_Ref} {tpe_Ref}
 *                  | 20 FLAGGEDtpe len_Nat flags_Nat tpe_Ref
 *   SymbolInfo     = name_Ref owner_Ref flags_Nat info_Ref
 *   NameInfo       = <character sequence of length len_Nat in Utf8 format>
 *   Ref            = Nat
 *
 *   len is remaining length after `len'.
 */

    int TERMname = 1,
	TYPEname = 2,
	NONEsym = 3,
	TYPEsym = 4,
	ALIASsym = 5,
	CLASSsym = 6,
	VALsym = 7,
	EXTref = 8,
	EXTMODCLASSref = 9,
	NOtpe = 10,
	THIStpe = 11,
	SINGLEtpe = 12,
	TYPEREFtpe = 13,
	COMPOUNDtpe = 14,
	METHODtpe = 15,
	POLYtpe = 16,
	OVERLOADEDtpe = 17,
	UNBOXEDtpe = 18,
        UNBOXEDARRAYtpe = 19,
	FLAGGEDtpe = 20,
	ERRORtpe = 21;

    int firstSymTag = NONEsym, lastSymTag = VALsym;
    int firstTypeTag = NOtpe, lastTypeTag = FLAGGEDtpe;

// flag encodings

    int COVARflag = 1,
	CONTRAVARflag = 2,
	REPEATEDflag = 4,
	DEFflag = 8;
}
