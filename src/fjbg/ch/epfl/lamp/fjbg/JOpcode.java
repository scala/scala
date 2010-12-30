/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Definition of opcodes for the JVM.
 *
 * @author Michel Schinz, Thomas Friedli
 * @version 1.0
 */

public class JOpcode {
    public final String name;
    public final int code;

    // The following attributes can be (statically) unknown for some
    // instructions, and are therefore not public. To know their value,
    // functions have to be used (see JCodeIterator).
    protected final int size;
    protected final JType[] producedDataTypes;
    protected final JType[] consumedDataTypes;
    protected final int jumpKind;
    protected final int successorCount;

    protected final static int UNKNOWN = Integer.MIN_VALUE;

    protected final static int JMP_NONE             = 0;
    protected final static int JMP_NEXT             = 1;
    protected final static int JMP_ALWAYS_S2_OFFSET = 2;
    protected final static int JMP_ALWAYS_S4_OFFSET = 3;
    protected final static int JMP_MAYBE_S2_OFFSET  = 4;
    protected final static int JMP_TABLE            = 5;
    protected final static int JMP_LOOKUP           = 6;

    protected final static JType[] NO_DATA = new JType[0];

    protected final static JType[] INT_TYPE =
        new JType[] { JType.INT };
    protected final static JType[] FLOAT_TYPE =
        new JType[] { JType.FLOAT };
    protected final static JType[] LONG_TYPE =
        new JType[] { JType.LONG };
    protected final static JType[] DOUBLE_TYPE =
        new JType[] { JType.DOUBLE };
    protected final static JType[] OBJECT_REF_TYPE =
        new JType[] { JObjectType.JAVA_LANG_OBJECT };
    protected final static JType[] ARRAY_REF_TYPE =
        new JType[] { new JArrayType(JType.VOID) };
    protected final static JType[] REFERENCE_TYPE =
        new JType[] { JType.REFERENCE };
    protected final static JType[] ADDRESS_TYPE =
        new JType[] { JType.ADDRESS };
    protected final static JType[] UNKNOWN_TYPE =
        new JType[] { JType.UNKNOWN };

    /// Instruction codes
    public final static int cNOP             = 0;
    public final static int cACONST_NULL     = 1;
    public final static int cICONST_M1       = 2;
    public final static int cICONST_0        = 3;
    public final static int cICONST_1        = 4;
    public final static int cICONST_2        = 5;
    public final static int cICONST_3        = 6;
    public final static int cICONST_4        = 7;
    public final static int cICONST_5        = 8;
    public final static int cLCONST_0        = 9;
    public final static int cLCONST_1        = 10;
    public final static int cFCONST_0        = 11;
    public final static int cFCONST_1        = 12;
    public final static int cFCONST_2        = 13;
    public final static int cDCONST_0        = 14;
    public final static int cDCONST_1        = 15;
    public final static int cBIPUSH          = 16;
    public final static int cSIPUSH          = 17;
    public final static int cLDC             = 18;
    public final static int cLDC_W           = 19;
    public final static int cLDC2_W          = 20;
    public final static int cILOAD           = 21;
    public final static int cLLOAD           = 22;
    public final static int cFLOAD           = 23;
    public final static int cDLOAD           = 24;
    public final static int cALOAD           = 25;
    public final static int cILOAD_0         = 26;
    public final static int cILOAD_1         = 27;
    public final static int cILOAD_2         = 28;
    public final static int cILOAD_3         = 29;
    public final static int cLLOAD_0         = 30;
    public final static int cLLOAD_1         = 31;
    public final static int cLLOAD_2         = 32;
    public final static int cLLOAD_3         = 33;
    public final static int cFLOAD_0         = 34;
    public final static int cFLOAD_1         = 35;
    public final static int cFLOAD_2         = 36;
    public final static int cFLOAD_3         = 37;
    public final static int cDLOAD_0         = 38;
    public final static int cDLOAD_1         = 39;
    public final static int cDLOAD_2         = 40;
    public final static int cDLOAD_3         = 41;
    public final static int cALOAD_0         = 42;
    public final static int cALOAD_1         = 43;
    public final static int cALOAD_2         = 44;
    public final static int cALOAD_3         = 45;
    public final static int cIALOAD          = 46;
    public final static int cLALOAD          = 47;
    public final static int cFALOAD          = 48;
    public final static int cDALOAD          = 49;
    public final static int cAALOAD          = 50;
    public final static int cBALOAD          = 51;
    public final static int cCALOAD          = 52;
    public final static int cSALOAD          = 53;
    public final static int cISTORE          = 54;
    public final static int cLSTORE          = 55;
    public final static int cFSTORE          = 56;
    public final static int cDSTORE          = 57;
    public final static int cASTORE          = 58;
    public final static int cISTORE_0        = 59;
    public final static int cISTORE_1        = 60;
    public final static int cISTORE_2        = 61;
    public final static int cISTORE_3        = 62;
    public final static int cLSTORE_0        = 63;
    public final static int cLSTORE_1        = 64;
    public final static int cLSTORE_2        = 65;
    public final static int cLSTORE_3        = 66;
    public final static int cFSTORE_0        = 67;
    public final static int cFSTORE_1        = 68;
    public final static int cFSTORE_2        = 69;
    public final static int cFSTORE_3        = 70;
    public final static int cDSTORE_0        = 71;
    public final static int cDSTORE_1        = 72;
    public final static int cDSTORE_2        = 73;
    public final static int cDSTORE_3        = 74;
    public final static int cASTORE_0        = 75;
    public final static int cASTORE_1        = 76;
    public final static int cASTORE_2        = 77;
    public final static int cASTORE_3        = 78;
    public final static int cIASTORE         = 79;
    public final static int cLASTORE         = 80;
    public final static int cFASTORE         = 81;
    public final static int cDASTORE         = 82;
    public final static int cAASTORE         = 83;
    public final static int cBASTORE         = 84;
    public final static int cCASTORE         = 85;
    public final static int cSASTORE         = 86;
    public final static int cPOP             = 87;
    public final static int cPOP2            = 88;
    public final static int cDUP             = 89;
    public final static int cDUP_X1          = 90;
    public final static int cDUP_X2          = 91;
    public final static int cDUP2            = 92;
    public final static int cDUP2_X1         = 93;
    public final static int cDUP2_X2         = 94;
    public final static int cSWAP            = 95;
    public final static int cIADD            = 96;
    public final static int cLADD            = 97;
    public final static int cFADD            = 98;
    public final static int cDADD            = 99;
    public final static int cISUB            = 100;
    public final static int cLSUB            = 101;
    public final static int cFSUB            = 102;
    public final static int cDSUB            = 103;
    public final static int cIMUL            = 104;
    public final static int cLMUL            = 105;
    public final static int cFMUL            = 106;
    public final static int cDMUL            = 107;
    public final static int cIDIV            = 108;
    public final static int cLDIV            = 109;
    public final static int cFDIV            = 110;
    public final static int cDDIV            = 111;
    public final static int cIREM            = 112;
    public final static int cLREM            = 113;
    public final static int cFREM            = 114;
    public final static int cDREM            = 115;
    public final static int cINEG            = 116;
    public final static int cLNEG            = 117;
    public final static int cFNEG            = 118;
    public final static int cDNEG            = 119;
    public final static int cISHL            = 120;
    public final static int cLSHL            = 121;
    public final static int cISHR            = 122;
    public final static int cLSHR            = 123;
    public final static int cIUSHR           = 124;
    public final static int cLUSHR           = 125;
    public final static int cIAND            = 126;
    public final static int cLAND            = 127;
    public final static int cIOR             = 128;
    public final static int cLOR             = 129;
    public final static int cIXOR            = 130;
    public final static int cLXOR            = 131;
    public final static int cIINC            = 132;
    public final static int cI2L             = 133;
    public final static int cI2F             = 134;
    public final static int cI2D             = 135;
    public final static int cL2I             = 136;
    public final static int cL2F             = 137;
    public final static int cL2D             = 138;
    public final static int cF2I             = 139;
    public final static int cF2L             = 140;
    public final static int cF2D             = 141;
    public final static int cD2I             = 142;
    public final static int cD2L             = 143;
    public final static int cD2F             = 144;
    public final static int cI2B             = 145;
    public final static int cI2C             = 146;
    public final static int cI2S             = 147;
    public final static int cLCMP            = 148;
    public final static int cFCMPL           = 149;
    public final static int cFCMPG           = 150;
    public final static int cDCMPL           = 151;
    public final static int cDCMPG           = 152;
    public final static int cIFEQ            = 153;
    public final static int cIFNE            = 154;
    public final static int cIFLT            = 155;
    public final static int cIFGE            = 156;
    public final static int cIFGT            = 157;
    public final static int cIFLE            = 158;
    public final static int cIF_ICMPEQ       = 159;
    public final static int cIF_ICMPNE       = 160;
    public final static int cIF_ICMPLT       = 161;
    public final static int cIF_ICMPGE       = 162;
    public final static int cIF_ICMPGT       = 163;
    public final static int cIF_ICMPLE       = 164;
    public final static int cIF_ACMPEQ       = 165;
    public final static int cIF_ACMPNE       = 166;
    public final static int cGOTO            = 167;
    public final static int cJSR             = 168;
    public final static int cRET             = 169;
    public final static int cTABLESWITCH     = 170;
    public final static int cLOOKUPSWITCH    = 171;
    public final static int cIRETURN         = 172;
    public final static int cLRETURN         = 173;
    public final static int cFRETURN         = 174;
    public final static int cDRETURN         = 175;
    public final static int cARETURN         = 176;
    public final static int cRETURN          = 177;
    public final static int cGETSTATIC       = 178;
    public final static int cPUTSTATIC       = 179;
    public final static int cGETFIELD        = 180;
    public final static int cPUTFIELD        = 181;
    public final static int cINVOKEVIRTUAL   = 182;
    public final static int cINVOKESPECIAL   = 183;
    public final static int cINVOKESTATIC    = 184;
    public final static int cINVOKEINTERFACE = 185;
    public final static int cNEW             = 187;
    public final static int cNEWARRAY        = 188;
    public final static int cANEWARRAY       = 189;
    public final static int cARRAYLENGTH     = 190;
    public final static int cATHROW          = 191;
    public final static int cCHECKCAST       = 192;
    public final static int cINSTANCEOF      = 193;
    public final static int cMONITORENTER    = 194;
    public final static int cMONITOREXIT     = 195;
    public final static int cWIDE            = 196;
    public final static int cMULTIANEWARRAY  = 197;
    public final static int cIFNULL          = 198;
    public final static int cIFNONNULL       = 199;
    public final static int cGOTO_W          = 200;
    public final static int cJSR_W           = 201;

    // Objects representing instructions
    public final static JOpcode NOP =
        new JOpcode("NOP", cNOP, 1, NO_DATA, NO_DATA, JMP_NEXT);
    public final static JOpcode ACONST_NULL = new JOpcode("ACONST_NULL",
                                                          cACONST_NULL,
                                                          1,
                                                          REFERENCE_TYPE,
                                                          NO_DATA,
                                                          JMP_NEXT);
    public final static JOpcode ICONST_M1 =
        new JOpcode("ICONST_M1", cICONST_M1, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ICONST_0 =
        new JOpcode("ICONST_0", cICONST_0, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ICONST_1 =
        new JOpcode("ICONST_1", cICONST_1, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ICONST_2 =
        new JOpcode("ICONST_2", cICONST_2, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ICONST_3 =
        new JOpcode("ICONST_3", cICONST_3, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ICONST_4 =
        new JOpcode("ICONST_4", cICONST_4, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ICONST_5 =
        new JOpcode("ICONST_5", cICONST_5, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LCONST_0 =
        new JOpcode("LCONST_0", cLCONST_0, 1, LONG_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LCONST_1 =
        new JOpcode("LCONST_1", cLCONST_1, 1, LONG_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FCONST_0 =
        new JOpcode("FCONST_0", cFCONST_0, 1, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FCONST_1 =
        new JOpcode("FCONST_1", cFCONST_1, 1, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FCONST_2 =
        new JOpcode("FCONST_2", cFCONST_2, 1, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode DCONST_0 =
        new JOpcode("DCONST_0", cDCONST_0, 1, DOUBLE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode DCONST_1 =
        new JOpcode("DCONST_1", cDCONST_1, 1, DOUBLE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode BIPUSH =
        new JOpcode("BIPUSH", cBIPUSH, 2, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode SIPUSH =
        new JOpcode("SIPUSH", cSIPUSH, 3, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LDC =
        new JOpcode("LDC", cLDC, 2, UNKNOWN_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LDC_W =
        new JOpcode("LDC_W", cLDC_W, 3, UNKNOWN_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LDC2_W =
        new JOpcode("LDC2_W", cLDC2_W, 3, UNKNOWN_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ILOAD =
        new JOpcode("ILOAD", cILOAD, 2, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LLOAD =
        new JOpcode("LLOAD", cLLOAD, 2, LONG_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FLOAD =
        new JOpcode("FLOAD", cFLOAD, 2, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode DLOAD =
        new JOpcode("DLOAD", cDLOAD, 2, DOUBLE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ALOAD =
        new JOpcode("ALOAD", cALOAD, 2, REFERENCE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ILOAD_0 =
        new JOpcode("ILOAD_0", cILOAD_0, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ILOAD_1 =
        new JOpcode("ILOAD_1", cILOAD_1, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ILOAD_2 =
        new JOpcode("ILOAD_2", cILOAD_2, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ILOAD_3 =
        new JOpcode("ILOAD_3", cILOAD_3, 1, INT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LLOAD_0 =
        new JOpcode("LLOAD_0", cLLOAD_0, 1, LONG_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LLOAD_1 =
        new JOpcode("LLOAD_1", cLLOAD_1, 1, LONG_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LLOAD_2 =
        new JOpcode("LLOAD_2", cLLOAD_2, 1, LONG_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode LLOAD_3 =
        new JOpcode("LLOAD_3", cLLOAD_3, 1, LONG_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FLOAD_0 =
        new JOpcode("FLOAD_0", cFLOAD_0, 1, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FLOAD_1 =
        new JOpcode("FLOAD_1", cFLOAD_1, 1, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FLOAD_2 =
        new JOpcode("FLOAD_2", cFLOAD_2, 1, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode FLOAD_3 =
        new JOpcode("FLOAD_3", cFLOAD_3, 1, FLOAT_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode DLOAD_0 =
        new JOpcode("DLOAD_0", cDLOAD_0, 1, DOUBLE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode DLOAD_1 =
        new JOpcode("DLOAD_1", cDLOAD_1, 1, DOUBLE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode DLOAD_2 =
        new JOpcode("DLOAD_2", cDLOAD_2, 1, DOUBLE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode DLOAD_3 =
        new JOpcode("DLOAD_3", cDLOAD_3, 1, DOUBLE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ALOAD_0 =
        new JOpcode("ALOAD_0", cALOAD_0, 1, REFERENCE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ALOAD_1 =
        new JOpcode("ALOAD_1", cALOAD_1, 1, REFERENCE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ALOAD_2 =
        new JOpcode("ALOAD_2", cALOAD_2, 1, REFERENCE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode ALOAD_3 =
        new JOpcode("ALOAD_3", cALOAD_3, 1, REFERENCE_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode IALOAD =
        new JOpcode("IALOAD",
                    cIALOAD,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, JArrayType.INT},
                    JMP_NEXT);
    public final static JOpcode LALOAD =
        new JOpcode("LALOAD",
                    cLALOAD,
                    1,
                    LONG_TYPE,
                    new JType[] {JType.INT, JArrayType.LONG},
                    JMP_NEXT);
    public final static JOpcode FALOAD =
        new JOpcode("FALOAD",
                    cFALOAD,
                    1,
                    FLOAT_TYPE,
                    new JType[] {JType.INT, JArrayType.FLOAT},
                    JMP_NEXT);
    public final static JOpcode DALOAD =
        new JOpcode("DALOAD",
                    cDALOAD,
                    1,
                    DOUBLE_TYPE,
                    new JType[] {JType.INT, JArrayType.DOUBLE},
                    JMP_NEXT);
    public final static JOpcode AALOAD =
        new JOpcode("AALOAD",
                    cAALOAD,
                    1,
                    REFERENCE_TYPE,
                    new JType[] {JType.INT, JArrayType.REFERENCE},
                    JMP_NEXT);
    public final static JOpcode BALOAD =
        new JOpcode("BALOAD",
                    cBALOAD,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, new JArrayType(JType.UNKNOWN)},
                    JMP_NEXT);
    public final static JOpcode CALOAD =
        new JOpcode("CALOAD",
                    cCALOAD,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, JArrayType.CHAR},
                    JMP_NEXT);
    public final static JOpcode SALOAD =
        new JOpcode("SALOAD",
                    cSALOAD,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, JArrayType.SHORT},
                    JMP_NEXT);
    public final static JOpcode ISTORE =
        new JOpcode("ISTORE", cISTORE, 2, NO_DATA, INT_TYPE, JMP_NEXT);
    public final static JOpcode LSTORE =
        new JOpcode("LSTORE", cLSTORE, 2, NO_DATA, LONG_TYPE, JMP_NEXT);
    public final static JOpcode FSTORE =
        new JOpcode("FSTORE", cFSTORE, 2, NO_DATA, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode DSTORE =
        new JOpcode("DSTORE", cDSTORE, 2, NO_DATA, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode ASTORE =
        new JOpcode("ASTORE", cASTORE, 2, NO_DATA, REFERENCE_TYPE, JMP_NEXT);
    public final static JOpcode ISTORE_0 =
        new JOpcode("ISTORE_0", cISTORE_0, 1, NO_DATA, INT_TYPE, JMP_NEXT);
    public final static JOpcode ISTORE_1 =
        new JOpcode("ISTORE_1", cISTORE_1, 1, NO_DATA, INT_TYPE, JMP_NEXT);
    public final static JOpcode ISTORE_2 =
        new JOpcode("ISTORE_2", cISTORE_2, 1, NO_DATA, INT_TYPE, JMP_NEXT);
    public final static JOpcode ISTORE_3 =
        new JOpcode("ISTORE_3", cISTORE_3, 1, NO_DATA, INT_TYPE, JMP_NEXT);
    public final static JOpcode LSTORE_0 =
        new JOpcode("LSTORE_0", cLSTORE_0, 1, NO_DATA, LONG_TYPE, JMP_NEXT);
    public final static JOpcode LSTORE_1 =
        new JOpcode("LSTORE_1", cLSTORE_1, 1, NO_DATA, LONG_TYPE, JMP_NEXT);
    public final static JOpcode LSTORE_2 =
        new JOpcode("LSTORE_2", cLSTORE_2, 1, NO_DATA, LONG_TYPE, JMP_NEXT);
    public final static JOpcode LSTORE_3 =
        new JOpcode("LSTORE_3", cLSTORE_3, 1, NO_DATA, LONG_TYPE, JMP_NEXT);
    public final static JOpcode FSTORE_0 =
        new JOpcode("FSTORE_0", cFSTORE_0, 1, NO_DATA, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode FSTORE_1 =
        new JOpcode("FSTORE_1", cFSTORE_1, 1, NO_DATA, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode FSTORE_2 =
        new JOpcode("FSTORE_2", cFSTORE_2, 1, NO_DATA, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode FSTORE_3 =
        new JOpcode("FSTORE_3", cFSTORE_3, 1, NO_DATA, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode DSTORE_0 =
        new JOpcode("DSTORE_0", cDSTORE_0, 1, NO_DATA, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode DSTORE_1 =
        new JOpcode("DSTORE_1", cDSTORE_1, 1, NO_DATA, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode DSTORE_2 =
        new JOpcode("DSTORE_2", cDSTORE_2, 1, NO_DATA, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode DSTORE_3 =
        new JOpcode("DSTORE_3", cDSTORE_3, 1, NO_DATA, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode ASTORE_0 = new JOpcode("ASTORE_0",
                                                       cASTORE_0,
                                                       1,
                                                       NO_DATA,
                                                       REFERENCE_TYPE,
                                                       JMP_NEXT);
    public final static JOpcode ASTORE_1 = new JOpcode("ASTORE_1",
                                                       cASTORE_1,
                                                       1,
                                                       NO_DATA,
                                                       REFERENCE_TYPE,
                                                       JMP_NEXT);
    public final static JOpcode ASTORE_2 = new JOpcode("ASTORE_2",
                                                       cASTORE_2,
                                                       1,
                                                       NO_DATA,
                                                       REFERENCE_TYPE,
                                                       JMP_NEXT);
    public final static JOpcode ASTORE_3 = new JOpcode("ASTORE_3",
                                                       cASTORE_3,
                                                       1,
                                                       NO_DATA,
                                                       REFERENCE_TYPE,
                                                       JMP_NEXT);
    public final static JOpcode IASTORE =
        new JOpcode("IASTORE",
                    cIASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.INT,
                                  JType.INT,
                                  JArrayType.INT},
                    JMP_NEXT);
    public final static JOpcode LASTORE =
        new JOpcode("LASTORE",
                    cLASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.LONG,
                                  JType.INT,
                                  JArrayType.LONG},
                    JMP_NEXT);
    public final static JOpcode FASTORE =
        new JOpcode("FASTORE",
                    cFASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.FLOAT,
                                  JType.INT,
                                  JArrayType.FLOAT},
                    JMP_NEXT);
    public final static JOpcode DASTORE =
        new JOpcode("DASTORE",
                    cDASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.DOUBLE,
                                  JType.INT,
                                  JArrayType.DOUBLE},
                    JMP_NEXT);
    public final static JOpcode AASTORE =
        new JOpcode("AASTORE",
                    cAASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.REFERENCE,
                                  JType.INT,
                                  JArrayType.REFERENCE},
                    JMP_NEXT);
    public final static JOpcode BASTORE =
        new JOpcode("BASTORE",
                    cBASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.INT,
                                  JType.INT,
                                  new JArrayType(JType.UNKNOWN)},
                    JMP_NEXT);
    public final static JOpcode CASTORE =
        new JOpcode("CASTORE",
                    cCASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.INT,
                                  JType.INT,
                                  JArrayType.CHAR},
                    JMP_NEXT);
    public final static JOpcode SASTORE =
        new JOpcode("SASTORE",
                    cSASTORE,
                    1,
                    NO_DATA,
                    new JType[] { JType.INT,
                                  JType.INT,
                                  JArrayType.SHORT},
                    JMP_NEXT);
    public final static JOpcode POP =
        new JOpcode("POP", cPOP, 1, NO_DATA, UNKNOWN_TYPE, JMP_NEXT);
    public final static JOpcode POP2 =
        new JOpcode("POP2", cPOP2, 1, NO_DATA, UNKNOWN_TYPE, JMP_NEXT);
    public final static JOpcode DUP =
        new JOpcode("DUP", cDUP, 1, UNKNOWN_TYPE, UNKNOWN_TYPE, JMP_NEXT);
    public final static JOpcode DUP_X1 = new JOpcode("DUP_X1",
                                                     cDUP_X1,
                                                     1,
                                                     UNKNOWN_TYPE,
                                                     UNKNOWN_TYPE,
                                                     JMP_NEXT);
    public final static JOpcode DUP_X2 = new JOpcode("DUP_X2",
                                                     cDUP_X2,
                                                     1,
                                                     UNKNOWN_TYPE,
                                                     UNKNOWN_TYPE,
                                                     JMP_NEXT);
    public final static JOpcode DUP2 =
        new JOpcode("DUP2", cDUP2, 1, UNKNOWN_TYPE, UNKNOWN_TYPE, JMP_NEXT);
    public final static JOpcode DUP2_X1 = new JOpcode("DUP2_X1",
                                                      cDUP2_X1,
                                                      1,
                                                      UNKNOWN_TYPE,
                                                      UNKNOWN_TYPE,
                                                      JMP_NEXT);
    public final static JOpcode DUP2_X2 = new JOpcode("DUP2_X2",
                                                      cDUP2_X2,
                                                      1,
                                                      UNKNOWN_TYPE,
                                                      UNKNOWN_TYPE,
                                                      JMP_NEXT);
    public final static JOpcode SWAP =
        new JOpcode("SWAP", cSWAP, 1, UNKNOWN_TYPE, UNKNOWN_TYPE, JMP_NEXT);
    public final static JOpcode IADD =
        new JOpcode("IADD",
                    cIADD,
                    1,
                    INT_TYPE,
                    new JType[] { JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LADD =
        new JOpcode("LADD",
                    cLADD,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode FADD =
        new JOpcode("FADD",
                    cFADD,
                    1,
                    FLOAT_TYPE,
                    new JType[] { JType.FLOAT, JType.FLOAT },
                    JMP_NEXT);
    public final static JOpcode DADD =
        new JOpcode("DADD",
                    cDADD,
                    1,
                    DOUBLE_TYPE,
                    new JType[] { JType.DOUBLE, JType.DOUBLE },
                    JMP_NEXT);
    public final static JOpcode ISUB =
        new JOpcode("ISUB",
                    cISUB,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LSUB =
        new JOpcode("LSUB",
                    cLSUB,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode FSUB =
        new JOpcode("FSUB",
                    cFSUB,
                    1,
                    FLOAT_TYPE,
                    new JType[] { JType.FLOAT, JType.FLOAT },
                    JMP_NEXT);
    public final static JOpcode DSUB =
        new JOpcode("DSUB",
                    cDSUB,
                    1,
                    DOUBLE_TYPE,
                    new JType[] { JType.DOUBLE, JType.DOUBLE },
                    JMP_NEXT);
    public final static JOpcode IMUL =
        new JOpcode("IMUL",
                    cIMUL,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LMUL =
        new JOpcode("LMUL",
                    cLMUL,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode FMUL =
        new JOpcode("FMUL",
                    cFMUL,
                    1,
                    FLOAT_TYPE,
                    new JType[] { JType.FLOAT, JType.FLOAT },
                    JMP_NEXT);
    public final static JOpcode DMUL =
        new JOpcode("DMUL",
                    cDMUL,
                    1,
                    DOUBLE_TYPE,
                    new JType[] { JType.DOUBLE, JType.DOUBLE },
                    JMP_NEXT);
    public final static JOpcode IDIV =
        new JOpcode("IDIV",
                    cIDIV,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LDIV =
        new JOpcode("LDIV",
                    cLDIV,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode FDIV =
        new JOpcode("FDIV",
                    cFDIV,
                    1,
                    FLOAT_TYPE,
                    new JType[] { JType.FLOAT, JType.FLOAT },
                    JMP_NEXT);
    public final static JOpcode DDIV =
        new JOpcode("DDIV",
                    cDDIV,
                    1,
                    DOUBLE_TYPE,
                    new JType[] { JType.DOUBLE, JType.DOUBLE },
                    JMP_NEXT);
    public final static JOpcode IREM =
        new JOpcode("IREM",
                    cIREM,
                    1,
                    INT_TYPE,
                    new JType[] {JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LREM =
        new JOpcode("LREM",
                    cLREM,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode FREM =
        new JOpcode("FREM",
                    cFREM,
                    1,
                    FLOAT_TYPE,
                    new JType[] { JType.FLOAT, JType.FLOAT },
                    JMP_NEXT);
    public final static JOpcode DREM =
        new JOpcode("DREM",
                    cDREM,
                    1,
                    DOUBLE_TYPE,
                    new JType[] { JType.DOUBLE, JType.DOUBLE },
                    JMP_NEXT);
    public final static JOpcode INEG =
        new JOpcode("INEG", cINEG, 1, INT_TYPE, INT_TYPE, JMP_NEXT);
    public final static JOpcode LNEG =
        new JOpcode("LNEG", cLNEG, 1, LONG_TYPE, LONG_TYPE, JMP_NEXT);
    public final static JOpcode FNEG =
        new JOpcode("FNEG", cFNEG, 1, FLOAT_TYPE, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode DNEG =
        new JOpcode("DNEG", cDNEG, 1, DOUBLE_TYPE, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode ISHL =
        new JOpcode("ISHL", cISHL,
                    1,
                    INT_TYPE,
                    new JType[] { JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LSHL =
        new JOpcode("LSHL",
                    cLSHL,
                    1,
                    LONG_TYPE,
                    new JType [] { JType.INT, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode ISHR =
        new JOpcode("ISHR",
                    cISHR,
                    1,
                    INT_TYPE,
                    new JType[] { JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LSHR =
        new JOpcode("LSHR",
                    cLSHR,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.INT, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode IUSHR =
        new JOpcode("IUSHR",
                    cIUSHR,
                    1,
                    INT_TYPE,
                    new JType[] { JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LUSHR =
        new JOpcode("LUSHR",
                    cLUSHR,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.INT, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode IAND =
        new JOpcode("IAND",
                    cIAND,
                    1,
                    INT_TYPE,
                    new JType[] { JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LAND =
        new JOpcode("LAND",
                    cLAND,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode IOR =
        new JOpcode("IOR",
                    cIOR,
                    1,
                    INT_TYPE,
                    new JType[] { JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LOR =
        new JOpcode("LOR",
                    cLOR,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode IXOR =
        new JOpcode("IXOR",
                    cIXOR,
                    1,
                    INT_TYPE,
                    new JType[] { JType.INT, JType.INT },
                    JMP_NEXT);
    public final static JOpcode LXOR =
        new JOpcode("LXOR",
                    cLXOR,
                    1,
                    LONG_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode IINC =
        new JOpcode("IINC", cIINC, 3, NO_DATA, NO_DATA, JMP_NEXT);
    public final static JOpcode I2L =
        new JOpcode("I2L", cI2L, 1, LONG_TYPE, INT_TYPE, JMP_NEXT);
    public final static JOpcode I2F =
        new JOpcode("I2F", cI2F, 1, FLOAT_TYPE, INT_TYPE, JMP_NEXT);
    public final static JOpcode I2D =
        new JOpcode("I2D", cI2D, 1, DOUBLE_TYPE, INT_TYPE, JMP_NEXT);
    public final static JOpcode L2I =
        new JOpcode("L2I", cL2I, 1, INT_TYPE, LONG_TYPE, JMP_NEXT);
    public final static JOpcode L2F =
        new JOpcode("L2F", cL2F, 1, FLOAT_TYPE, LONG_TYPE, JMP_NEXT);
    public final static JOpcode L2D =
        new JOpcode("L2D", cL2D, 1, DOUBLE_TYPE, LONG_TYPE, JMP_NEXT);
    public final static JOpcode F2I =
        new JOpcode("F2I", cF2I, 1, INT_TYPE, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode F2L =
        new JOpcode("F2L", cF2L, 1, LONG_TYPE, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode F2D =
        new JOpcode("F2D", cF2D, 1, DOUBLE_TYPE, FLOAT_TYPE, JMP_NEXT);
    public final static JOpcode D2I =
        new JOpcode("D2I", cD2I, 1, INT_TYPE, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode D2L =
        new JOpcode("D2L", cD2L, 1, LONG_TYPE, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode D2F =
        new JOpcode("D2F", cD2F, 1, FLOAT_TYPE, DOUBLE_TYPE, JMP_NEXT);
    public final static JOpcode I2B =
        new JOpcode("I2B", cI2B, 1, INT_TYPE, INT_TYPE, JMP_NEXT);
    public final static JOpcode I2C =
        new JOpcode("I2C", cI2C, 1, INT_TYPE, INT_TYPE, JMP_NEXT);
    public final static JOpcode I2S =
        new JOpcode("I2S", cI2S, 1, INT_TYPE, INT_TYPE, JMP_NEXT);
    public final static JOpcode LCMP =
        new JOpcode("LCMP",
                    cLCMP,
                    1,
                    INT_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode FCMPL =
        new JOpcode("FCMPL",
                    cFCMPL,
                    1,
                    INT_TYPE,
                    new JType[] { JType.FLOAT, JType.FLOAT },
                    JMP_NEXT);
    public final static JOpcode FCMPG =
        new JOpcode("FCMPG",
                    cFCMPG,
                    1,
                    INT_TYPE,
                    new JType[] { JType.FLOAT, JType.FLOAT },
                    JMP_NEXT);
    public final static JOpcode DCMPL =
        new JOpcode("DCMPL",
                    cDCMPL,
                    1,
                    INT_TYPE,
                    new JType[] { JType.LONG, JType.LONG },
                    JMP_NEXT);
    public final static JOpcode DCMPG =
        new JOpcode("DCMPG",
                    cDCMPG,
                    1,
                    INT_TYPE,
                    new JType[] { JType.DOUBLE, JType.DOUBLE },
                    JMP_NEXT);
    public final static JOpcode IFEQ =
        new JOpcode("IFEQ", cIFEQ, 3, NO_DATA, INT_TYPE, JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IFNE =
        new JOpcode("IFNE", cIFNE, 3, NO_DATA, INT_TYPE, JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IFLT =
        new JOpcode("IFLT", cIFLT, 3, NO_DATA, INT_TYPE, JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IFGE =
        new JOpcode("IFGE", cIFGE, 3, NO_DATA, INT_TYPE, JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IFGT =
        new JOpcode("IFGT", cIFGT, 3, NO_DATA, INT_TYPE, JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IFLE =
        new JOpcode("IFLE", cIFLE, 3, NO_DATA, INT_TYPE, JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ICMPEQ =
        new JOpcode("IF_ICMPEQ",
                    cIF_ICMPEQ,
                    3,
                    NO_DATA,
                    new JType[] { JType.INT, JType.INT },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ICMPNE =
        new JOpcode("IF_ICMPNE",
                    cIF_ICMPNE,
                    3,
                    NO_DATA,
                    new JType[] { JType.INT, JType.INT },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ICMPLT =
        new JOpcode("IF_ICMPLT",
                    cIF_ICMPLT,
                    3,
                    NO_DATA,
                    new JType[] { JType.INT, JType.INT },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ICMPGE =
        new JOpcode("IF_ICMPGE",
                    cIF_ICMPGE,
                    3,
                    NO_DATA,
                    new JType[] { JType.INT, JType.INT },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ICMPGT =
        new JOpcode("IF_ICMPGT",
                    cIF_ICMPGT,
                    3,
                    NO_DATA,
                    new JType[] { JType.INT, JType.INT },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ICMPLE =
        new JOpcode("IF_ICMPLE",
                    cIF_ICMPLE,
                    3,
                    NO_DATA,
                    new JType[] { JType.INT, JType.INT },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ACMPEQ =
        new JOpcode("IF_ACMPEQ",
                    cIF_ACMPEQ,
                    3,
                    NO_DATA,
                    new JType[] { JType.REFERENCE, JType.REFERENCE },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IF_ACMPNE =
        new JOpcode("IF_ACMPNE",
                    cIF_ACMPNE,
                    3,
                    NO_DATA,
                    new JType[] { JType.REFERENCE, JType.REFERENCE },
                    JMP_MAYBE_S2_OFFSET);
    public final static JOpcode GOTO =
        new JOpcode("GOTO", cGOTO, 3, NO_DATA, NO_DATA, JMP_ALWAYS_S2_OFFSET);
    public final static JOpcode JSR =
        new JOpcode("JSR", cJSR, 3, ADDRESS_TYPE, NO_DATA, JMP_ALWAYS_S2_OFFSET);
    public final static JOpcode RET =
        new JOpcode("RET", cRET, 2, NO_DATA, NO_DATA, JMP_NONE);
    public final static JOpcode TABLESWITCH = new JOpcode("TABLESWITCH",
                                                          cTABLESWITCH,
                                                          UNKNOWN,
                                                          NO_DATA,
                                                          INT_TYPE,
                                                          JMP_TABLE);
    public final static JOpcode LOOKUPSWITCH = new JOpcode("LOOKUPSWITCH",
                                                           cLOOKUPSWITCH,
                                                           UNKNOWN,
                                                           NO_DATA,
                                                           INT_TYPE,
                                                           JMP_LOOKUP);
    public final static JOpcode IRETURN =
        new JOpcode("IRETURN", cIRETURN, 1, NO_DATA, INT_TYPE, JMP_NONE);
    public final static JOpcode LRETURN =
        new JOpcode("LRETURN", cLRETURN, 1, NO_DATA, LONG_TYPE, JMP_NONE);
    public final static JOpcode FRETURN =
        new JOpcode("FRETURN", cFRETURN, 1, NO_DATA, FLOAT_TYPE, JMP_NONE);
    public final static JOpcode DRETURN =
        new JOpcode("DRETURN", cDRETURN, 1, NO_DATA, DOUBLE_TYPE, JMP_NONE);
    public final static JOpcode ARETURN = new JOpcode("ARETURN",
                                                      cARETURN,
                                                      1,
                                                      NO_DATA,
                                                      OBJECT_REF_TYPE,
                                                      JMP_NONE);
    public final static JOpcode RETURN =
        new JOpcode("RETURN", cRETURN, 1, NO_DATA, NO_DATA, JMP_NONE);
    public final static JOpcode GETSTATIC = new JOpcode("GETSTATIC",
                                                        cGETSTATIC,
                                                        3,
                                                        UNKNOWN_TYPE,
                                                        NO_DATA,
                                                        JMP_NEXT);
    public final static JOpcode PUTSTATIC = new JOpcode("PUTSTATIC",
                                                        cPUTSTATIC,
                                                        3,
                                                        NO_DATA,
                                                        UNKNOWN_TYPE,
                                                        JMP_NEXT);
    public final static JOpcode GETFIELD = new JOpcode("GETFIELD",
                                                       cGETFIELD,
                                                       3,
                                                       UNKNOWN_TYPE,
                                                       OBJECT_REF_TYPE,
                                                       JMP_NEXT);
    public final static JOpcode PUTFIELD =
        new JOpcode("PUTFIELD", cPUTFIELD, 3, NO_DATA, UNKNOWN_TYPE, JMP_NEXT);
    public final static JOpcode INVOKEVIRTUAL = new JOpcode("INVOKEVIRTUAL",
                                                            cINVOKEVIRTUAL,
                                                            3,
                                                            NO_DATA,
                                                            UNKNOWN_TYPE,
                                                            JMP_NEXT);
    public final static JOpcode INVOKESPECIAL = new JOpcode("INVOKESPECIAL",
                                                            cINVOKESPECIAL,
                                                            3,
                                                            NO_DATA,
                                                            UNKNOWN_TYPE,
                                                            JMP_NEXT);
    public final static JOpcode INVOKESTATIC = new JOpcode("INVOKESTATIC",
                                                           cINVOKESTATIC,
                                                           3,
                                                           NO_DATA,
                                                           UNKNOWN_TYPE,
                                                           JMP_NEXT);
    public final static JOpcode INVOKEINTERFACE =
        new JOpcode("INVOKEINTERFACE",
                    cINVOKEINTERFACE,
                    5,
                    NO_DATA,
                    UNKNOWN_TYPE,
                    JMP_NEXT);
    public final static JOpcode NEW =
        new JOpcode("NEW", cNEW, 3, OBJECT_REF_TYPE, NO_DATA, JMP_NEXT);
    public final static JOpcode NEWARRAY =
        new JOpcode("NEWARRAY",
                    cNEWARRAY,
                    2,
                    ARRAY_REF_TYPE,
                    INT_TYPE,
                    JMP_NEXT);
    public final static JOpcode ANEWARRAY =
        new JOpcode("ANEWARRAY",
                    cANEWARRAY,
                    3,
                    ARRAY_REF_TYPE,
                    INT_TYPE,
                    JMP_NEXT);
    public final static JOpcode ARRAYLENGTH = new JOpcode("ARRAYLENGTH",
                                                          cARRAYLENGTH,
                                                          1,
                                                          INT_TYPE,
                                                          ARRAY_REF_TYPE,
                                                          JMP_NEXT);
    public final static JOpcode ATHROW = new JOpcode("ATHROW",
                                                     cATHROW,
                                                     1,
                                                     OBJECT_REF_TYPE,
                                                     OBJECT_REF_TYPE,
                                                     JMP_NONE);
    public final static JOpcode CHECKCAST = new JOpcode("CHECKCAST",
                                                        cCHECKCAST,
                                                        3,
                                                        OBJECT_REF_TYPE,
                                                        OBJECT_REF_TYPE,
                                                        JMP_NEXT);
    public final static JOpcode INSTANCEOF = new JOpcode("INSTANCEOF",
                                                         cINSTANCEOF,
                                                         3,
                                                         INT_TYPE,
                                                         OBJECT_REF_TYPE,
                                                         JMP_NEXT);
    public final static JOpcode MONITORENTER = new JOpcode("MONITORENTER",
                                                           cMONITORENTER,
                                                           1,
                                                           NO_DATA,
                                                           OBJECT_REF_TYPE,
                                                           JMP_NEXT);
    public final static JOpcode MONITOREXIT = new JOpcode("MONITOREXIT",
                                                          cMONITOREXIT,
                                                          1,
                                                          NO_DATA,
                                                          OBJECT_REF_TYPE,
                                                          JMP_NEXT);
    public final static JOpcode WIDE = new JOpcode("WIDE",
                                                   cWIDE,
                                                   UNKNOWN,
                                                   UNKNOWN_TYPE,
                                                   UNKNOWN_TYPE,
                                                   JMP_NEXT);
    public final static JOpcode MULTIANEWARRAY = new JOpcode("MULTIANEWARRAY",
                                                             cMULTIANEWARRAY,
                                                             4,
                                                             ARRAY_REF_TYPE,
                                                             UNKNOWN_TYPE,
                                                             JMP_NEXT);
    public final static JOpcode IFNULL = new JOpcode("IFNULL",
                                                     cIFNULL,
                                                     3,
                                                     NO_DATA,
                                                     REFERENCE_TYPE,
                                                     JMP_MAYBE_S2_OFFSET);
    public final static JOpcode IFNONNULL = new JOpcode("IFNONNULL",
                                                        cIFNONNULL,
                                                        3,
                                                        NO_DATA,
                                                        REFERENCE_TYPE,
                                                        JMP_MAYBE_S2_OFFSET);
    public final static JOpcode GOTO_W = new JOpcode("GOTO_W",
                                                     cGOTO_W,
                                                     5,
                                                     NO_DATA,
                                                     NO_DATA,
                                                     JMP_ALWAYS_S4_OFFSET);
    public final static JOpcode JSR_W =
        new JOpcode("JSR_W", cJSR_W, 5, ADDRESS_TYPE, NO_DATA, JMP_NEXT);

    public final static JOpcode[] OPCODES = {
        NOP, ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1,
        ICONST_2, ICONST_3, ICONST_4, ICONST_5, LCONST_0,
        LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0,
        DCONST_1, BIPUSH, SIPUSH, LDC, LDC_W,
        LDC2_W, ILOAD, LLOAD, FLOAD, DLOAD,
        ALOAD, ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3,
        LLOAD_0, LLOAD_1, LLOAD_2, LLOAD_3, FLOAD_0,
        FLOAD_1, FLOAD_2, FLOAD_3, DLOAD_0, DLOAD_1,
        DLOAD_2, DLOAD_3, ALOAD_0, ALOAD_1, ALOAD_2,
        ALOAD_3, IALOAD, LALOAD, FALOAD, DALOAD,
        AALOAD, BALOAD, CALOAD, SALOAD, ISTORE,
        LSTORE, FSTORE, DSTORE, ASTORE, ISTORE_0,
        ISTORE_1, ISTORE_2, ISTORE_3, LSTORE_0, LSTORE_1,
        LSTORE_2, LSTORE_3, FSTORE_0, FSTORE_1, FSTORE_2,
        FSTORE_3, DSTORE_0, DSTORE_1, DSTORE_2, DSTORE_3,
        ASTORE_0, ASTORE_1, ASTORE_2, ASTORE_3, IASTORE,
        LASTORE, FASTORE, DASTORE, AASTORE, BASTORE,
        CASTORE, SASTORE, POP, POP2, DUP,
        DUP_X1, DUP_X2, DUP2, DUP2_X1, DUP2_X2,
        SWAP, IADD, LADD, FADD, DADD,
        ISUB, LSUB, FSUB, DSUB, IMUL,
        LMUL, FMUL, DMUL, IDIV, LDIV,
        FDIV, DDIV, IREM, LREM, FREM,
        DREM, INEG, LNEG, FNEG, DNEG,
        ISHL, LSHL, ISHR, LSHR, IUSHR,
        LUSHR, IAND, LAND, IOR, LOR,
        IXOR, LXOR, IINC, I2L, I2F,
        I2D, L2I, L2F, L2D, F2I,
        F2L, F2D, D2I, D2L, D2F,
        I2B, I2C, I2S, LCMP, FCMPL,
        FCMPG, DCMPL, DCMPG, IFEQ, IFNE,
        IFLT, IFGE, IFGT, IFLE, IF_ICMPEQ,
        IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE,
        IF_ACMPEQ, IF_ACMPNE, GOTO, JSR, RET,
        TABLESWITCH, LOOKUPSWITCH, IRETURN, LRETURN, FRETURN,
        DRETURN, ARETURN, RETURN, GETSTATIC, PUTSTATIC,
        GETFIELD, PUTFIELD, INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC,
        INVOKEINTERFACE, null, NEW, NEWARRAY, ANEWARRAY,
        ARRAYLENGTH, ATHROW, CHECKCAST, INSTANCEOF, MONITORENTER,
        MONITOREXIT, WIDE, MULTIANEWARRAY, IFNULL, IFNONNULL,
        GOTO_W, JSR_W
    };

    protected JOpcode(String name,
                      int code,
                      int size,
                      JType[] producedDataTypes,
                      JType[] consumedDataTypes,
                      int jumpKind) {
        this.name = name;
        this.code = code;
        this.size = size;
        this.producedDataTypes = producedDataTypes;
        this.consumedDataTypes = consumedDataTypes;
        this.jumpKind = jumpKind;
        switch (jumpKind) {
        case JMP_NONE: successorCount = 0; break;
        case JMP_NEXT: successorCount = 1; break;
        case JMP_ALWAYS_S2_OFFSET: successorCount = 1; break;
        case JMP_ALWAYS_S4_OFFSET: successorCount = 1; break;
        case JMP_MAYBE_S2_OFFSET: successorCount = 2; break;
        case JMP_TABLE: successorCount = UNKNOWN; break;
        case JMP_LOOKUP: successorCount = UNKNOWN; break;
        default: successorCount = UNKNOWN; break;
        }
    }

    public String toString() { return name; }
    protected int getSize() { return size; }
    protected JType[] getProducedDataTypes() { return producedDataTypes; }
    protected JType[] getConsumedDataTypes() { return consumedDataTypes; }

    protected int getProducedDataSize() {
        if (producedDataTypes != UNKNOWN_TYPE)
            return JType.getTotalSize(producedDataTypes);
        else {
            switch (code) {
            case cLDC: case cLDC_W: case cBALOAD:
                return 1;
            case cLDC2_W: case cDUP: case cSWAP:
                return 2;
            case cDUP_X1:
                return 3;
            case cDUP_X2: case cDUP2:
                return 4;
            case cDUP2_X1:
                return 5;
            case cDUP2_X2:
                return 6;
            default:
                throw new Error(this.toString());
            }
        }
    }

    protected int getConsumedDataSize() {
        if (consumedDataTypes != UNKNOWN_TYPE)
            return JType.getTotalSize(consumedDataTypes);
        else {
            switch (code) {
            case cPOP: case cDUP:
                return 1;
            case cPOP2: case cDUP_X1: case cDUP2: case cSWAP:
                return 2;
            case cDUP_X2: case cDUP2_X1:
                return 3;
            case cDUP2_X2:
                return 4;
            default:
                throw new Error(this.toString());
            }
        }
    }

    protected int getProducedDataTypesNumber() {
        if (producedDataTypes != UNKNOWN_TYPE)
            return producedDataTypes.length;
        else {
            switch (code) {
            case cLDC: case cLDC_W: case cLDC2_W: case cBALOAD:
            case cGETSTATIC: case cGETFIELD:
                return 1;
            case cDUP: case cSWAP:
                return 2;
            case cDUP_X2: case cDUP2: case cDUP2_X1: case cDUP2_X2:
                return 2;
            case cDUP_X1:
                return 3;
            default:
                throw new Error(this.toString());
            }
        }
    }

    protected int getConsumedDataTypesNumber() {
        if (consumedDataTypes != UNKNOWN_TYPE)
            return consumedDataTypes.length;
        else {
            switch (code) {
            case cPOP: case cDUP: case cPUTSTATIC:
                return 1;
            case cPUTFIELD: case cDUP_X1: case cDUP_X2:
            case cDUP2: case cDUP2_X1: case cPOP2: case cSWAP:
                return 2;
            default:
                throw new Error(this.toString());
            }
        }
    }
}
