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
 *                  | 2 CONSTRNAME len_Nat NameInfo
 *                  | 3 TYPENAME len_Nat NameInfo
 *                  | 4 NONEsym len_Nat
 *                  | 5 TYPEsym len_Nat SymbolInfo lobound_Ref
 *                  | 6 ALIASsym len_Nat SymbolInfo
 *                  | 7 CLASSsym len_Nat SymbolInfo thistype_Ref constrsym_Ref
 *                  | 8 VALsym len_Nat SymbolInfo [classsym_Ref]
 *                  | 9 EXTsym len_Nat name_Ref [owner_Ref]
 *                  | 10 EXTMODCLASSsym len_Nat name_Ref [owner_Ref]
 *                  | 11 NOtpe len_Nat
 *                  | 12 THIStpe len_Nat sym_Ref
 *                  | 13 SINGLEtpe len_Nat type_Ref sym_Ref
 *                  | 14 TYPEREFtpe len_Nat type_Ref sym_Ref {targ_Ref}
 *                  | 15 COMPOUNDtpe len_Nat classsym_Ref {tpe_Ref}
 *                  | 16 METHODtpe len_Nat tpe_Ref {tpe_Ref}
 *                  | 17 POLYTtpe len_Nat tpe_Ref {sym_Ref}
 *                  | 18 OVERLOADEDtpe len_Nat {sym_Ref} {tpe_Ref}
 *                  | 21 FLAGGEDtype len_Nat flags_Nat tpe_Ref
 *   SymbolInfo     = name_Ref owner_Ref flags_Nat info_Ref
 *   NameInfo       = <character sequence of length len_Nat in Utf8 format>
 *   Ref            = Nat
 *
 *   len is remaining length after `len'.
 */

    int TERMname = 1,
	CONSTRname = 2,
	TYPEname = 3,
	NONEsym = 4,
	TYPEsym = 5,
	ALIASsym = 6,
	CLASSsym = 7,
	VALsym = 8,
	EXTsym = 9,
	EXTMODCLASSsym = 10,
	NOtpe = 11,
	THIStpe = 12,
	SINGLEtpe = 13,
	TYPEREFtpe = 14,
	COMPOUNDtpe = 15,
	METHODtpe = 16,
	POLYtpe = 17,
	OVERLOADEDtpe = 18,
	UNBOXEDtpe = 19,
        UNBOXEDARRAYtpe = 20,
	FLAGGEDtpe = 21,
	ERRORtpe = 22;

    int firstSymTag = NONEsym, lastSymTag = VALsym;
    int firstTypeTag = NOtpe, lastTypeTag = FLAGGEDtpe;

// flag encodings

    int COVARflag = 1,
	CONTRAVARflag = 2,
	REPEATEDflag = 4,
	DEFflag = 8;
}
