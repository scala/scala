/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id: EntryTags.java

package scalac.symtab;

public interface EntryTags {

    int TERMname = 1,
	CONSTRname = 2,
	TYPEname = 3,
	NONEsym = 4,
	TYPEsym = 5,
	ALIASsym = 6,
	CLASSsym = 7,
	MODULEsym = 8,
	VALsym = 9,
	EXTsym = 10,
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
	ERRORtpe = 21;

    int firstSymTag = NONEsym, lastSymTag = VALsym;
    int firstTypeTag = NOtpe, lastTypeTag = UNBOXEDARRAYtpe;
}
