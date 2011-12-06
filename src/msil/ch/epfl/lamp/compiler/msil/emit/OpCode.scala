/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import java.io.IOException

/** Describes a Microsoft intermediate language (MSIL) instruction.
 *
 *  @author  Nikolay Mihaylov
 *  @version 1.0
 */
class OpCode extends Visitable {
    import OpCode._

    /** The Operation Code of Microsoft intermediate language (MSIL) instruction. */
    var CEE_opcode : Int = _

    /** The name of the Microsoft intermediate language (MSIL) instruction. */
    var CEE_string: String = _

    /** The type of Microsoft intermediate language (MSIL) instruction. */
    var CEE_code: Short = _

    /** How the Microsoft intermediate language (MSIL) instruction pops the stack. */
    var CEE_pop: Byte = _

    /** How the Microsoft intermediate language (MSIL) instruction pushes operand onto the stack. */
    var CEE_push: Byte = _

    /** Describes the type of flow control. */
    var CEE_flow: Byte = _

    /** ????? */
    var CEE_inline: Byte = _

    var CEE_length: Byte = _

    var CEE_popush: Byte = _

    /**
     * the apply method for a visitor
     */
    @throws(classOf[IOException])
    def apply(v: Visitor) {
	v.caseOpCode(this)
    }

    protected def length(): Byte = {
	val code = OpCode.length(CEE_code)
	val inline = OpCode.INLINE_length(CEE_inline)
	return if(inline < 0) { -1 } else { (code + inline).toByte }
    }

    protected def popush(): Byte = {
	val pop = OpCode.POP_size(CEE_pop)
	val push = OpCode.PUSH_size(CEE_push)
	return if(pop < 0 || push < 0) { OpCode.POPUSH_SPECIAL } else { (push - pop).toByte }
    }

    override def toString(): String = {
	return CEE_string
    }
}

object OpCode {

    //########################################################################
    // Common Execution Environment opcodes

    final val CEE_NOP            : Int = 0x0000
    final val CEE_BREAK          : Int = 0x0001
    final val CEE_LDARG_0        : Int = 0x0002
    final val CEE_LDARG_1        : Int = 0x0003
    final val CEE_LDARG_2        : Int = 0x0004
    final val CEE_LDARG_3        : Int = 0x0005
    final val CEE_LDLOC_0        : Int = 0x0006
    final val CEE_LDLOC_1        : Int = 0x0007
    final val CEE_LDLOC_2        : Int = 0x0008
    final val CEE_LDLOC_3        : Int = 0x0009
    final val CEE_STLOC_0        : Int = 0x000A
    final val CEE_STLOC_1        : Int = 0x000B
    final val CEE_STLOC_2        : Int = 0x000C
    final val CEE_STLOC_3        : Int = 0x000D
    final val CEE_LDARG_S        : Int = 0x000E
    final val CEE_LDARGA_S       : Int = 0x000F
    final val CEE_STARG_S        : Int = 0x0010
    final val CEE_LDLOC_S        : Int = 0x0011
    final val CEE_LDLOCA_S       : Int = 0x0012
    final val CEE_STLOC_S        : Int = 0x0013
    final val CEE_LDNULL         : Int = 0x0014
    final val CEE_LDC_I4_M1      : Int = 0x0015
    final val CEE_LDC_I4_0       : Int = 0x0016
    final val CEE_LDC_I4_1       : Int = 0x0017
    final val CEE_LDC_I4_2       : Int = 0x0018
    final val CEE_LDC_I4_3       : Int = 0x0019
    final val CEE_LDC_I4_4       : Int = 0x001A
    final val CEE_LDC_I4_5       : Int = 0x001B
    final val CEE_LDC_I4_6       : Int = 0x001C
    final val CEE_LDC_I4_7       : Int = 0x001D
    final val CEE_LDC_I4_8       : Int = 0x001E
    final val CEE_LDC_I4_S       : Int = 0x001F
    final val CEE_LDC_I4         : Int = 0x0020
    final val CEE_LDC_I8         : Int = 0x0021
    final val CEE_LDC_R4         : Int = 0x0022
    final val CEE_LDC_R8         : Int = 0x0023
    final val CEE_UNUSED49       : Int = 0x0024
    final val CEE_DUP            : Int = 0x0025
    final val CEE_POP            : Int = 0x0026
    final val CEE_JMP            : Int = 0x0027
    final val CEE_CALL           : Int = 0x0028
    final val CEE_CALLI          : Int = 0x0029
    final val CEE_RET            : Int = 0x002A
    final val CEE_BR_S           : Int = 0x002B
    final val CEE_BRFALSE_S      : Int = 0x002C
    final val CEE_BRTRUE_S       : Int = 0x002D
    final val CEE_BEQ_S          : Int = 0x002E
    final val CEE_BGE_S          : Int = 0x002F
    final val CEE_BGT_S          : Int = 0x0030
    final val CEE_BLE_S          : Int = 0x0031
    final val CEE_BLT_S          : Int = 0x0032
    final val CEE_BNE_UN_S       : Int = 0x0033
    final val CEE_BGE_UN_S       : Int = 0x0034
    final val CEE_BGT_UN_S       : Int = 0x0035
    final val CEE_BLE_UN_S       : Int = 0x0036
    final val CEE_BLT_UN_S       : Int = 0x0037
    final val CEE_BR             : Int = 0x0038
    final val CEE_BRFALSE        : Int = 0x0039
    final val CEE_BRTRUE         : Int = 0x003A
    final val CEE_BEQ            : Int = 0x003B
    final val CEE_BGE            : Int = 0x003C
    final val CEE_BGT            : Int = 0x003D
    final val CEE_BLE            : Int = 0x003E
    final val CEE_BLT            : Int = 0x003F
    final val CEE_BNE_UN         : Int = 0x0040
    final val CEE_BGE_UN         : Int = 0x0041
    final val CEE_BGT_UN         : Int = 0x0042
    final val CEE_BLE_UN         : Int = 0x0043
    final val CEE_BLT_UN         : Int = 0x0044
    final val CEE_SWITCH         : Int = 0x0045
    final val CEE_LDIND_I1       : Int = 0x0046
    final val CEE_LDIND_U1       : Int = 0x0047
    final val CEE_LDIND_I2       : Int = 0x0048
    final val CEE_LDIND_U2       : Int = 0x0049
    final val CEE_LDIND_I4       : Int = 0x004A
    final val CEE_LDIND_U4       : Int = 0x004B
    final val CEE_LDIND_I8       : Int = 0x004C
    final val CEE_LDIND_I        : Int = 0x004D
    final val CEE_LDIND_R4       : Int = 0x004E
    final val CEE_LDIND_R8       : Int = 0x004F
    final val CEE_LDIND_REF      : Int = 0x0050
    final val CEE_STIND_REF      : Int = 0x0051
    final val CEE_STIND_I1       : Int = 0x0052
    final val CEE_STIND_I2       : Int = 0x0053
    final val CEE_STIND_I4       : Int = 0x0054
    final val CEE_STIND_I8       : Int = 0x0055
    final val CEE_STIND_R4       : Int = 0x0056
    final val CEE_STIND_R8       : Int = 0x0057
    final val CEE_ADD            : Int = 0x0058
    final val CEE_SUB            : Int = 0x0059
    final val CEE_MUL            : Int = 0x005A
    final val CEE_DIV            : Int = 0x005B
    final val CEE_DIV_UN         : Int = 0x005C
    final val CEE_REM            : Int = 0x005D
    final val CEE_REM_UN         : Int = 0x005E
    final val CEE_AND            : Int = 0x005F
    final val CEE_OR             : Int = 0x0060
    final val CEE_XOR            : Int = 0x0061
    final val CEE_SHL            : Int = 0x0062
    final val CEE_SHR            : Int = 0x0063
    final val CEE_SHR_UN         : Int = 0x0064
    final val CEE_NEG            : Int = 0x0065
    final val CEE_NOT            : Int = 0x0066
    final val CEE_CONV_I1        : Int = 0x0067
    final val CEE_CONV_I2        : Int = 0x0068
    final val CEE_CONV_I4        : Int = 0x0069
    final val CEE_CONV_I8        : Int = 0x006A
    final val CEE_CONV_R4        : Int = 0x006B
    final val CEE_CONV_R8        : Int = 0x006C
    final val CEE_CONV_U4        : Int = 0x006D
    final val CEE_CONV_U8        : Int = 0x006E
    final val CEE_CALLVIRT       : Int = 0x006F
    final val CEE_CPOBJ          : Int = 0x0070
    final val CEE_LDOBJ          : Int = 0x0071
    final val CEE_LDSTR          : Int = 0x0072
    final val CEE_NEWOBJ         : Int = 0x0073
    final val CEE_CASTCLASS      : Int = 0x0074
    final val CEE_ISINST         : Int = 0x0075
    final val CEE_CONV_R_UN      : Int = 0x0076
    final val CEE_UNUSED58       : Int = 0x0077
    final val CEE_UNUSED1        : Int = 0x0078
    final val CEE_UNBOX          : Int = 0x0079
    final val CEE_THROW          : Int = 0x007A
    final val CEE_LDFLD          : Int = 0x007B
    final val CEE_LDFLDA         : Int = 0x007C
    final val CEE_STFLD          : Int = 0x007D
    final val CEE_LDSFLD         : Int = 0x007E
    final val CEE_LDSFLDA        : Int = 0x007F
    final val CEE_STSFLD         : Int = 0x0080
    final val CEE_STOBJ          : Int = 0x0081
    final val CEE_CONV_OVF_I1_UN : Int = 0x0082
    final val CEE_CONV_OVF_I2_UN : Int = 0x0083
    final val CEE_CONV_OVF_I4_UN : Int = 0x0084
    final val CEE_CONV_OVF_I8_UN : Int = 0x0085
    final val CEE_CONV_OVF_U1_UN : Int = 0x0086
    final val CEE_CONV_OVF_U2_UN : Int = 0x0087
    final val CEE_CONV_OVF_U4_UN : Int = 0x0088
    final val CEE_CONV_OVF_U8_UN : Int = 0x0089
    final val CEE_CONV_OVF_I_UN  : Int = 0x008A
    final val CEE_CONV_OVF_U_UN  : Int = 0x008B
    final val CEE_BOX            : Int = 0x008C
    final val CEE_NEWARR         : Int = 0x008D
    final val CEE_LDLEN          : Int = 0x008E
    final val CEE_LDELEMA        : Int = 0x008F
    final val CEE_LDELEM_I1      : Int = 0x0090
    final val CEE_LDELEM_U1      : Int = 0x0091
    final val CEE_LDELEM_I2      : Int = 0x0092
    final val CEE_LDELEM_U2      : Int = 0x0093
    final val CEE_LDELEM_I4      : Int = 0x0094
    final val CEE_LDELEM_U4      : Int = 0x0095
    final val CEE_LDELEM_I8      : Int = 0x0096
    final val CEE_LDELEM_I       : Int = 0x0097
    final val CEE_LDELEM_R4      : Int = 0x0098
    final val CEE_LDELEM_R8      : Int = 0x0099
    final val CEE_LDELEM_REF     : Int = 0x009A
    final val CEE_STELEM_I       : Int = 0x009B
    final val CEE_STELEM_I1      : Int = 0x009C
    final val CEE_STELEM_I2      : Int = 0x009D
    final val CEE_STELEM_I4      : Int = 0x009E
    final val CEE_STELEM_I8      : Int = 0x009F
    final val CEE_STELEM_R4      : Int = 0x00A0
    final val CEE_STELEM_R8      : Int = 0x00A1
    final val CEE_STELEM_REF     : Int = 0x00A2
    final val CEE_UNUSED2        : Int = 0x00A3
    final val CEE_UNUSED3        : Int = 0x00A4
    final val CEE_UNUSED4        : Int = 0x00A5
    final val CEE_UNUSED5        : Int = 0x00A6
    final val CEE_UNUSED6        : Int = 0x00A7
    final val CEE_UNUSED7        : Int = 0x00A8
    final val CEE_UNUSED8        : Int = 0x00A9
    final val CEE_UNUSED9        : Int = 0x00AA
    final val CEE_UNUSED10       : Int = 0x00AB
    final val CEE_UNUSED11       : Int = 0x00AC
    final val CEE_UNUSED12       : Int = 0x00AD
    final val CEE_UNUSED13       : Int = 0x00AE
    final val CEE_UNUSED14       : Int = 0x00AF
    final val CEE_UNUSED15       : Int = 0x00B0
    final val CEE_UNUSED16       : Int = 0x00B1
    final val CEE_UNUSED17       : Int = 0x00B2
    final val CEE_CONV_OVF_I1    : Int = 0x00B3
    final val CEE_CONV_OVF_U1    : Int = 0x00B4
    final val CEE_CONV_OVF_I2    : Int = 0x00B5
    final val CEE_CONV_OVF_U2    : Int = 0x00B6
    final val CEE_CONV_OVF_I4    : Int = 0x00B7
    final val CEE_CONV_OVF_U4    : Int = 0x00B8
    final val CEE_CONV_OVF_I8    : Int = 0x00B9
    final val CEE_CONV_OVF_U8    : Int = 0x00BA
    final val CEE_UNUSED50       : Int = 0x00BB
    final val CEE_UNUSED18       : Int = 0x00BC
    final val CEE_UNUSED19       : Int = 0x00BD
    final val CEE_UNUSED20       : Int = 0x00BE
    final val CEE_UNUSED21       : Int = 0x00BF
    final val CEE_UNUSED22       : Int = 0x00C0
    final val CEE_UNUSED23       : Int = 0x00C1
    final val CEE_REFANYVAL      : Int = 0x00C2
    final val CEE_CKFINITE       : Int = 0x00C3
    final val CEE_UNUSED24       : Int = 0x00C4
    final val CEE_UNUSED25       : Int = 0x00C5
    final val CEE_MKREFANY       : Int = 0x00C6
    final val CEE_UNUSED59       : Int = 0x00C7
    final val CEE_UNUSED60       : Int = 0x00C8
    final val CEE_UNUSED61       : Int = 0x00C9
    final val CEE_UNUSED62       : Int = 0x00CA
    final val CEE_UNUSED63       : Int = 0x00CB
    final val CEE_UNUSED64       : Int = 0x00CC
    final val CEE_UNUSED65       : Int = 0x00CD
    final val CEE_UNUSED66       : Int = 0x00CE
    final val CEE_UNUSED67       : Int = 0x00CF
    final val CEE_LDTOKEN        : Int = 0x00D0
    final val CEE_CONV_U2        : Int = 0x00D1
    final val CEE_CONV_U1        : Int = 0x00D2
    final val CEE_CONV_I         : Int = 0x00D3
    final val CEE_CONV_OVF_I     : Int = 0x00D4
    final val CEE_CONV_OVF_U     : Int = 0x00D5
    final val CEE_ADD_OVF        : Int = 0x00D6
    final val CEE_ADD_OVF_UN     : Int = 0x00D7
    final val CEE_MUL_OVF        : Int = 0x00D8
    final val CEE_MUL_OVF_UN     : Int = 0x00D9
    final val CEE_SUB_OVF        : Int = 0x00DA
    final val CEE_SUB_OVF_UN     : Int = 0x00DB
    final val CEE_ENDFINALLY     : Int = 0x00DC
    final val CEE_LEAVE          : Int = 0x00DD
    final val CEE_LEAVE_S        : Int = 0x00DE
    final val CEE_STIND_I        : Int = 0x00DF
    final val CEE_CONV_U         : Int = 0x00E0
    final val CEE_UNUSED26       : Int = 0x00E1
    final val CEE_UNUSED27       : Int = 0x00E2
    final val CEE_UNUSED28       : Int = 0x00E3
    final val CEE_UNUSED29       : Int = 0x00E4
    final val CEE_UNUSED30       : Int = 0x00E5
    final val CEE_UNUSED31       : Int = 0x00E6
    final val CEE_UNUSED32       : Int = 0x00E7
    final val CEE_UNUSED33       : Int = 0x00E8
    final val CEE_UNUSED34       : Int = 0x00E9
    final val CEE_UNUSED35       : Int = 0x00EA
    final val CEE_UNUSED36       : Int = 0x00EB
    final val CEE_UNUSED37       : Int = 0x00EC
    final val CEE_UNUSED38       : Int = 0x00ED
    final val CEE_UNUSED39       : Int = 0x00EE
    final val CEE_UNUSED40       : Int = 0x00EF
    final val CEE_UNUSED41       : Int = 0x00F0
    final val CEE_UNUSED42       : Int = 0x00F1
    final val CEE_UNUSED43       : Int = 0x00F2
    final val CEE_UNUSED44       : Int = 0x00F3
    final val CEE_UNUSED45       : Int = 0x00F4
    final val CEE_UNUSED46       : Int = 0x00F5
    final val CEE_UNUSED47       : Int = 0x00F6
    final val CEE_UNUSED48       : Int = 0x00F7
    final val CEE_PREFIX7        : Int = 0x00F8
    final val CEE_PREFIX6        : Int = 0x00F9
    final val CEE_PREFIX5        : Int = 0x00FA
    final val CEE_PREFIX4        : Int = 0x00FB
    final val CEE_PREFIX3        : Int = 0x00FC
    final val CEE_PREFIX2        : Int = 0x00FD
    final val CEE_PREFIX1        : Int = 0x00FE
    final val CEE_PREFIXREF      : Int = 0x00FF

    final val CEE_ARGLIST         : Int = 0x0100
    final val CEE_CEQ             : Int = 0x0101
    final val CEE_CGT             : Int = 0x0102
    final val CEE_CGT_UN          : Int = 0x0103
    final val CEE_CLT             : Int = 0x0104
    final val CEE_CLT_UN          : Int = 0x0105
    final val CEE_LDFTN           : Int = 0x0106
    final val CEE_LDVIRTFTN       : Int = 0x0107
    final val CEE_UNUSED56        : Int = 0x0108
    final val CEE_LDARG           : Int = 0x0109
    final val CEE_LDARGA          : Int = 0x010A
    final val CEE_STARG           : Int = 0x010B
    final val CEE_LDLOC           : Int = 0x010C
    final val CEE_LDLOCA          : Int = 0x010D
    final val CEE_STLOC           : Int = 0x010E
    final val CEE_LOCALLOC        : Int = 0x010F
    final val CEE_UNUSED57        : Int = 0x0110
    final val CEE_ENDFILTER       : Int = 0x0111
    final val CEE_UNALIGNED       : Int = 0x0112
    final val CEE_VOLATILE        : Int = 0x0113
    final val CEE_TAILCALL        : Int = 0x0114
    final val CEE_INITOBJ         : Int = 0x0115
    final val CEE_CONSTRAINED     : Int = 0xFE16
    final val CEE_READONLY        : Int = 0xFE1E
    final val CEE_UNUSED68        : Int = 0x0116
    final val CEE_CPBLK           : Int = 0x0117
    final val CEE_INITBLK         : Int = 0x0118
    final val CEE_UNUSED69        : Int = 0x0119
    final val CEE_RETHROW         : Int = 0x011A
    final val CEE_UNUSED51        : Int = 0x011B
    final val CEE_SIZEOF          : Int = 0x011C
    final val CEE_REFANYTYPE      : Int = 0x011D
    final val CEE_UNUSED52        : Int = 0x011E
    final val CEE_UNUSED53        : Int = 0x011F
    final val CEE_UNUSED54        : Int = 0x0120
    final val CEE_UNUSED55        : Int = 0x0121
    final val CEE_UNUSED70        : Int = 0x0122

    final val CEE_ILLEGAL         : Int = 0x0140
    final val CEE_MACRO_END       : Int = 0x0141

    final val CEE_BRNULL          : Int = 0x0180 // CEE_BRFALSE
    final val CEE_BRNULL_S        : Int = 0x0181 // CEE_BRFALSE_S
    final val CEE_BRZERO          : Int = 0x0182 // CEE_BRFALSE
    final val CEE_BRZERO_S        : Int = 0x0183 // CEE_BRFALSE_S
    final val CEE_BRINST          : Int = 0x0184 // CEE_BRTRUE
    final val CEE_BRINST_S        : Int = 0x0185 // CEE_BRTRUE_S
    final val CEE_LDIND_U8        : Int = 0x0186 // CEE_LDIND_I8
    final val CEE_LDELEM_U8       : Int = 0x0187 // CEE_LDELEM_I8
    final val CEE_LDC_I4_M1x      : Int = 0x0188 // CEE_LDC_I4_M1
    final val CEE_ENDFAULT        : Int = 0x0189 // CEE_ENDFINALLY

    final val CEE_BRNONZERO       : Int = 0x01C0 // CEE_BRTRUE
    final val CEE_BRNONZERO_S     : Int = 0x01C1 // CEE_BRTRUE_S

    final val CEE_BRNOT           : Int = 0x01C2
    final val CEE_BRNOT_S         : Int = 0x01C3
    final val CEE_NOCODE          : Int = 0x01C4

    final val CEE_count           : Int = 0x0200


    //########################################################################
    // Opcode's amount and type of poped data

    final val POP_NONE          : Byte        = 0x00
    final val POP_1             : Byte        = 0x01
    final val POP_1_1           : Byte        = 0x02
    final val POP_I             : Byte        = 0x03
    final val POP_I_1           : Byte        = 0x04
    final val POP_I_I           : Byte        = 0x05
    final val POP_I_I8          : Byte        = 0x06
    final val POP_I_R4          : Byte        = 0x07
    final val POP_I_R8          : Byte        = 0x08
    final val POP_I_I_I         : Byte        = 0x09
    final val POP_REF           : Byte        = 0x0A
    final val POP_REF_1         : Byte        = 0x0B
    final val POP_REF_I         : Byte        = 0x0C
    final val POP_REF_I_I       : Byte        = 0x0D
    final val POP_REF_I_I8      : Byte        = 0x0E
    final val POP_REF_I_R4      : Byte        = 0x0F
    final val POP_REF_I_R8      : Byte        = 0x10
    final val POP_REF_I_REF     : Byte        = 0x11
    final val POP_SPECIAL       : Byte        = 0x12
    final val POP_count         : Int         = 0x13
    final val POP_size          : Array[Byte] = new Array[Byte](POP_count)

	POP_size(POP_NONE)              =  0
	POP_size(POP_1)                 =  1
	POP_size(POP_1_1)               =  2
	POP_size(POP_I)                 =  1
	POP_size(POP_I_1)               =  2
	POP_size(POP_I_I)               =  2
	POP_size(POP_I_I8)              =  2
	POP_size(POP_I_R4)              =  2
	POP_size(POP_I_R8)              =  2
	POP_size(POP_I_I_I)             =  3
	POP_size(POP_REF)               =  1
	POP_size(POP_REF_1)             =  2
	POP_size(POP_REF_I)             =  2
	POP_size(POP_REF_I_I)           =  3
	POP_size(POP_REF_I_I8)          =  3
	POP_size(POP_REF_I_R4)          =  3
	POP_size(POP_REF_I_R8)          =  3
	POP_size(POP_REF_I_REF)         =  3
	POP_size(POP_SPECIAL)           = -1

    //########################################################################
    // Opcode's amount and type of pushed data

    final val PUSH_NONE         : Byte        = 0x00
    final val PUSH_1            : Byte        = 0x01
    final val PUSH_1_1          : Byte        = 0x02
    final val PUSH_I            : Byte        = 0x03
    final val PUSH_I8           : Byte        = 0x04
    final val PUSH_R4           : Byte        = 0x05
    final val PUSH_R8           : Byte        = 0x06
    final val PUSH_REF          : Byte        = 0x07
    final val PUSH_SPECIAL      : Byte        = 0x08
    final val PUSH_count        : Int         = 0x09
    final val PUSH_size         : Array[Byte] = new Array[Byte](PUSH_count)

	PUSH_size(PUSH_NONE)             =  0
	PUSH_size(PUSH_1)                =  1
	PUSH_size(PUSH_1_1)              =  2
	PUSH_size(PUSH_I)                =  1
	PUSH_size(PUSH_I8)               =  1
	PUSH_size(PUSH_R4)               =  1
	PUSH_size(PUSH_R8)               =  1
	PUSH_size(PUSH_REF)              =  1
	PUSH_size(PUSH_SPECIAL)          = -1

    //########################################################################
    // Opcode's amount of moved data

    final val POPUSH_SPECIAL    : Byte        = -128

    //########################################################################
    // Opcode's inline argument types

    final val INLINE_NONE       : Byte        = 0x00
    final val INLINE_VARIABLE_S : Byte        = 0x01
    final val INLINE_TARGET_S   : Byte        = 0x02
    final val INLINE_I_S        : Byte        = 0x03
    final val INLINE_VARIABLE   : Byte        = 0x04
    final val INLINE_TARGET     : Byte        = 0x05
    final val INLINE_I          : Byte        = 0x06
    final val INLINE_I8         : Byte        = 0x07
    final val INLINE_R          : Byte        = 0x08
    final val INLINE_R8         : Byte        = 0x09
    final val INLINE_STRING     : Byte        = 0x0A
    final val INLINE_TYPE       : Byte        = 0x0B
    final val INLINE_FIELD      : Byte        = 0x0C
    final val INLINE_METHOD     : Byte        = 0x0D
    final val INLINE_SIGNATURE  : Byte        = 0x0E
    final val INLINE_TOKEN      : Byte        = 0x0F
    final val INLINE_SWITCH     : Byte        = 0x10
    final val INLINE_count      : Int         = 0x11
    final val INLINE_length     : Array[Byte] = new Array[Byte](INLINE_count)

	INLINE_length(INLINE_NONE)       =  0
	INLINE_length(INLINE_VARIABLE_S) =  1
	INLINE_length(INLINE_TARGET_S)   =  1
	INLINE_length(INLINE_I_S)        =  1
	INLINE_length(INLINE_VARIABLE)   =  2
	INLINE_length(INLINE_TARGET)     =  4
	INLINE_length(INLINE_I)          =  4
	INLINE_length(INLINE_I8)         =  8
	INLINE_length(INLINE_R)          =  4
	INLINE_length(INLINE_R8)         =  8
	INLINE_length(INLINE_STRING)     =  4
	INLINE_length(INLINE_TYPE)       =  4
	INLINE_length(INLINE_FIELD)      =  4
	INLINE_length(INLINE_METHOD)     =  4
	INLINE_length(INLINE_SIGNATURE)  =  4
	INLINE_length(INLINE_SWITCH)     =  4
	INLINE_length(INLINE_TOKEN)      =  4

    //########################################################################
    // Opcode's control flow implications

    final val FLOW_META         : Byte = 0x00
    final val FLOW_NEXT         : Byte = 0x01
    final val FLOW_BRANCH       : Byte = 0x02
    final val FLOW_COND_BRANCH  : Byte = 0x03
    final val FLOW_BREAK        : Byte = 0x04
    final val FLOW_CALL         : Byte = 0x05
    final val FLOW_RETURN       : Byte = 0x06
    final val FLOW_THROW        : Byte = 0x07
    final val FLOW_count        : Int  = 0x08

    //########################################################################
    // Init methods for Opcode

    def opcode(that: OpCode, opcode: Int, string: String, code: Int,
			pop: Byte, push: Byte, inline: Byte, flow: Byte) {
	that.CEE_opcode = opcode
	that.CEE_string = string
	that.CEE_code = code.toShort
	that.CEE_pop = pop
	that.CEE_push = push
	that.CEE_inline = inline
	that.CEE_flow = flow
	that.CEE_length = that.length()
	that.CEE_popush = that.popush()
    }

    def length(code: Int): Byte = {
	if ((code & 0xFFFFFF00) == 0xFFFFFF00) return 1
	if ((code & 0xFFFFFF00) == 0xFFFFFE00) return 2
	return 0
    }

    //########################################################################
    // case OpCode

    /**
     * Adds two values and pushes the result onto the evaluation stack.
     */
    final val Add = new OpCode()
    opcode(Add, CEE_ADD, "add", 0xFFFFFF58, POP_1_1, PUSH_1, INLINE_NONE, FLOW_NEXT)

    /**
     * Fills space if bytecodes are patched. No meaningful operation is performed
     * although a processing cycle can be consumed.
     */
    final val Nop = new OpCode()
	opcode(Nop, CEE_NOP, "nop", 0xFFFFFF00, POP_NONE, PUSH_NONE, INLINE_NONE , FLOW_NEXT)

    /**
     * Signals the Common Language Infrastructure (CLI) to inform the debugger that
     * a break point has been tripped.
     */
    final val Break = new OpCode()
	opcode(Break, CEE_BREAK, "break"    , 0xFFFFFF01, POP_NONE, PUSH_NONE   , INLINE_NONE , FLOW_BREAK)

    /**
     * Loads the argument at index 0 onto the evaluation stack.
     */
    final val Ldarg_0 = new OpCode()
	opcode(Ldarg_0, CEE_LDARG_0  , "ldarg.0"  , 0xFFFFFF02, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the argument at index 1 onto the evaluation stack.
     */
    final val Ldarg_1 = new OpCode()
	opcode(Ldarg_1, CEE_LDARG_1  , "ldarg.1"  , 0xFFFFFF03, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the argument at index 2 onto the evaluation stack.
     */
    final val Ldarg_2 = new OpCode()
	opcode(Ldarg_2, CEE_LDARG_2  , "ldarg.2"  , 0xFFFFFF04, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the argument at index 3 onto the evaluation stack.
     */
    final val Ldarg_3 = new OpCode()
	opcode(Ldarg_3, CEE_LDARG_3  , "ldarg.3"  , 0xFFFFFF05, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the local variable at index 0 onto the evaluation stack.
     */
    final val Ldloc_0 = new OpCode()
	opcode(Ldloc_0, CEE_LDLOC_0  , "ldloc.0"  , 0xFFFFFF06, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the local variable at index 1 onto the evaluation stack.
     */
    final val Ldloc_1 = new OpCode()
	opcode(Ldloc_1, CEE_LDLOC_1  , "ldloc.1"  , 0xFFFFFF07, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the local variable at index 2 onto the evaluation stack.
     */
    final val Ldloc_2 = new OpCode()
	opcode(Ldloc_2, CEE_LDLOC_2  , "ldloc.2"  , 0xFFFFFF08, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the local variable at index 3 onto the evaluation stack.
     */
    final val Ldloc_3 = new OpCode()
	opcode(Ldloc_3, CEE_LDLOC_3  , "ldloc.3"  , 0xFFFFFF09, POP_NONE, PUSH_1 , INLINE_NONE , FLOW_NEXT)

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 0.
     */
    final val Stloc_0 = new OpCode()
	opcode(Stloc_0, CEE_STLOC_0  , "stloc.0"  , 0xFFFFFF0A, POP_1   , PUSH_NONE, INLINE_NONE , FLOW_NEXT)

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 1.
     */
    final val Stloc_1 = new OpCode()
	opcode(Stloc_1, CEE_STLOC_1  , "stloc.1"  , 0xFFFFFF0B, POP_1   , PUSH_NONE, INLINE_NONE , FLOW_NEXT)

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 2.
     */
    final val Stloc_2 = new OpCode()
	opcode(Stloc_2, CEE_STLOC_2  , "stloc.2"  , 0xFFFFFF0C, POP_1   , PUSH_NONE, INLINE_NONE , FLOW_NEXT)

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 3.
     */
    final val Stloc_3 = new OpCode()
	opcode(Stloc_3, CEE_STLOC_3  , "stloc.3"  , 0xFFFFFF0D, POP_1   , PUSH_NONE, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the argument (referenced by a specified short form index)
     * onto the evaluation stack.
     */
    final val Ldarg_S = new OpCode()
	opcode(Ldarg_S, CEE_LDARG_S  , "ldarg.s"  , 0xFFFFFF0E, POP_NONE, PUSH_1 , INLINE_VARIABLE_S, FLOW_NEXT)

    /**
     * Load an argument address, in short form, onto the evaluation stack.
     */
    final val Ldarga_S = new OpCode()
	opcode(Ldarga_S, CEE_LDARGA_S , "ldarga.s" , 0xFFFFFF0F, POP_NONE, PUSH_I , INLINE_VARIABLE_S, FLOW_NEXT)

    /**
     * Loads the local variable at a specific index onto the evaluation stack,
     * short form.
     */
    final val Ldloc_S = new OpCode()
	opcode(Ldloc_S, CEE_LDLOC_S  , "ldloc.s"  , 0xFFFFFF11, POP_NONE, PUSH_1 , INLINE_VARIABLE_S, FLOW_NEXT)

    /**
     * Loads the address of the local variable at a specific index onto
     * the evaluation stack, short form.
     */
    final val Ldloca_S = new OpCode()
	opcode(Ldloca_S, CEE_LDLOCA_S , "ldloca.s" , 0xFFFFFF12, POP_NONE, PUSH_I , INLINE_VARIABLE_S, FLOW_NEXT)

    /**
     * Stores the value on top of the evaluation stack in the argument slot
     * at a specified index, short form.
     */
    final val Starg_S = new OpCode()
	opcode(Starg_S, CEE_STARG_S  , "starg.s"  , 0xFFFFFF10, POP_1   , PUSH_NONE , INLINE_VARIABLE_S, FLOW_NEXT)

    /**
     * Pops the current value from the top of the evaluation stack and stores it
     * in a the local variable list at index (short form).
     */
    final val Stloc_S = new OpCode()
	opcode(Stloc_S, CEE_STLOC_S  , "stloc.s"  , 0xFFFFFF13, POP_1   , PUSH_NONE, INLINE_VARIABLE_S, FLOW_NEXT)

    /**
     * Pushes a null reference (type O) onto the evaluation stack.
     */
    final val Ldnull = new OpCode()
	opcode(Ldnull, CEE_LDNULL   , "ldnull"   , 0xFFFFFF14, POP_NONE, PUSH_REF , INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of -1 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_M1 = new OpCode()
	opcode(Ldc_I4_M1, CEE_LDC_I4_M1, "ldc.i4.m1", 0xFFFFFF15, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 0 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_0 = new OpCode()
	opcode(Ldc_I4_0, CEE_LDC_I4_0 , "ldc.i4.0" , 0xFFFFFF16, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 1 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_1 = new OpCode()
	opcode(Ldc_I4_1, CEE_LDC_I4_1 , "ldc.i4.1" , 0xFFFFFF17, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 2 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_2 = new OpCode()
	opcode(Ldc_I4_2, CEE_LDC_I4_2 , "ldc.i4.2" , 0xFFFFFF18, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 3 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_3 = new OpCode()
	opcode(Ldc_I4_3, CEE_LDC_I4_3 , "ldc.i4.3" , 0xFFFFFF19, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 4 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_4 = new OpCode()
	opcode(Ldc_I4_4, CEE_LDC_I4_4 , "ldc.i4.4" , 0xFFFFFF1A, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 5 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_5 = new OpCode()
	opcode(Ldc_I4_5, CEE_LDC_I4_5 , "ldc.i4.5" , 0xFFFFFF1B, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 6 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_6 = new OpCode()
	opcode(Ldc_I4_6, CEE_LDC_I4_6 , "ldc.i4.6", 0xFFFFFF1C, POP_NONE, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 7 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_7 = new OpCode()
	opcode(Ldc_I4_7, CEE_LDC_I4_7 , "ldc.i4.7", 0xFFFFFF1D, POP_NONE   , PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the integer value of 8 onto the evaluation stack as an int32.
     */
    final val Ldc_I4_8 = new OpCode()
	opcode(Ldc_I4_8, CEE_LDC_I4_8 , "ldc.i4.8", 0xFFFFFF1E, POP_NONE   , PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes the supplied int8 value onto the evaluation stack as an int32, short form.
     */
    final val Ldc_I4_S = new OpCode()
	opcode(Ldc_I4_S, CEE_LDC_I4_S , "ldc.i4.s", 0xFFFFFF1F, POP_NONE   , PUSH_I, INLINE_I_S, FLOW_NEXT)

    /**
     * Pushes a supplied value of type int32 onto the evaluation stack as an int32.
     */
    final val Ldc_I4 = new OpCode()
	opcode(Ldc_I4, CEE_LDC_I4, "ldc.i4"  , 0xFFFFFF20, POP_NONE   , PUSH_I, INLINE_I  , FLOW_NEXT)

    /**
     *  Pushes a supplied value of type int64 onto the evaluation stack as an int64.
     */
    final val Ldc_I8 = new OpCode()
	opcode(Ldc_I8, CEE_LDC_I8, "ldc.i8"  , 0xFFFFFF21, POP_NONE   , PUSH_I8, INLINE_I8 , FLOW_NEXT)

    /**
     * Pushes a supplied value of type float32 onto the evaluation stack as type F (float).
     */
    final val Ldc_R4 = new OpCode()
	opcode(Ldc_R4, CEE_LDC_R4, "ldc.r4"  , 0xFFFFFF22, POP_NONE   , PUSH_R4, INLINE_R  , FLOW_NEXT)

    /**
     * Pushes a supplied value of type float64 onto the evaluation stack as type F (float).
     */
    final val Ldc_R8 = new OpCode()
	opcode(Ldc_R8, CEE_LDC_R8, "ldc.r8"  , 0xFFFFFF23, POP_NONE   , PUSH_R8, INLINE_R8 , FLOW_NEXT)

    /**
     * Copies the current topmost value on the evaluation stack, and then pushes the copy
     * onto the evaluation stack.
     */
    final val Dup = new OpCode()
	opcode(Dup, CEE_DUP , "dup"     , 0xFFFFFF25, POP_1      , PUSH_1_1 , INLINE_NONE  , FLOW_NEXT)

    /**
     * Removes the value currently on top of the evaluation stack.
     */
    final val Pop = new OpCode()
	opcode(Pop, CEE_POP , "pop"     , 0xFFFFFF26, POP_1      , PUSH_NONE , INLINE_NONE  , FLOW_NEXT)

    /**
     * Exits current method and jumps to specified method.
     */
    final val Jmp = new OpCode()
	opcode(Jmp, CEE_JMP , "jmp"     , 0xFFFFFF27, POP_NONE   , PUSH_NONE , INLINE_METHOD, FLOW_CALL)

    /**
     * Calls the method indicated by the passed method descriptor.
     */
    final val Call = new OpCode()
	opcode(Call, CEE_CALL , "call"    , 0xFFFFFF28, POP_SPECIAL, PUSH_SPECIAL, INLINE_METHOD    , FLOW_CALL)

    /**
   * constrained prefix
   */
  final val Constrained = new OpCode()
opcode(Constrained, CEE_CONSTRAINED , "constrained."    , 0xFFFFFE16, POP_NONE, PUSH_NONE, INLINE_NONE    , FLOW_NEXT)

  /**
   * readonly prefix
   */
  final val Readonly = new OpCode()
opcode(Readonly, CEE_READONLY , "readonly."    , 0xFFFFFE1E, POP_NONE, PUSH_NONE, INLINE_NONE    , FLOW_NEXT)

    /**
     * Calls the method indicated on the evaluation stack (as a pointer to an entry point)
     * with arguments described by a calling convention.
     */
    final val Calli = new OpCode()
	opcode(Calli, CEE_CALLI, "calli"   , 0xFFFFFF29, POP_SPECIAL, PUSH_SPECIAL, INLINE_SIGNATURE , FLOW_CALL)

    /**
     * Returns from the current method, pushing a return value (if present) from the caller's
     * evaluation stack onto the callee's evaluation stack.
     */
    final val Ret = new OpCode()
	opcode(Ret, CEE_RET  , "ret"     , 0xFFFFFF2A, POP_SPECIAL, PUSH_NONE, INLINE_NONE      , FLOW_RETURN)

    /**
     * Unconditionally transfers control to a target instruction (short form).
     */
    final val Br_S = new OpCode()
	opcode(Br_S, CEE_BR_S , "br.s"    , 0xFFFFFF2B, POP_NONE, PUSH_NONE, INLINE_TARGET_S  , FLOW_BRANCH)

    /**
     * Transfers control to a target instruction if value is false, a null reference, or zero.
     */
    final val Brfalse_S = new OpCode()
	opcode(Brfalse_S, CEE_BRFALSE_S,"brfalse.s", 0xFFFFFF2C, POP_I, PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if value is true, not null, or non-zero.
     */
    final val Brtrue_S = new OpCode()
	opcode(Brtrue_S, CEE_BRTRUE_S , "brtrue.s", 0xFFFFFF2D, POP_I, PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if two values are equal.
     */
    final val Beq_S = new OpCode()
	opcode(Beq_S, CEE_BEQ_S, "beq.s", 0xFFFFFF2E, POP_1_1 , PUSH_NONE, INLINE_TARGET_S  , FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is greater than
     * or equal to the second value.
     */
    final val Bge_S = new OpCode()
	opcode(Bge_S, CEE_BGE_S, "bge.s", 0xFFFFFF2F, POP_1_1 , PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is greater than
     * the second value.
     */
    final val Bgt_S = new OpCode()
	opcode(Bgt_S, CEE_BGT_S, "bgt.s"    , 0xFFFFFF30, POP_1_1 , PUSH_NONE, INLINE_TARGET_S  , FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * or equal to the second value.
     */
    final val Ble_S = new OpCode()
	opcode(Ble_S, CEE_BLE_S, "ble.s"    , 0xFFFFFF31, POP_1_1 , PUSH_NONE, INLINE_TARGET_S  , FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * the second value.
     */
    final val Blt_S = new OpCode()
	opcode(Blt_S, CEE_BLT_S, "blt.s", 0xFFFFFF32, POP_1_1, PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) when two unsigned integer values
     * or unordered float values are not equal.
     */
    final val Bne_Un_S = new OpCode()
	opcode(Bne_Un_S, CEE_BNE_UN_S, "bne.un.s", 0xFFFFFF33, POP_1_1 , PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is greather
     * than the second value, when comparing unsigned integer values or unordered float values.
     */
    final val Bge_Un_S = new OpCode()
	opcode(Bge_Un_S, CEE_BGE_UN_S, "bge.un.s", 0xFFFFFF34, POP_1_1, PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is greater than
     * the second value, when comparing unsigned integer values or unordered float values.
     */
    final val Bgt_Un_S = new OpCode()
	opcode(Bgt_Un_S, CEE_BGT_UN_S, "bgt.un.s", 0xFFFFFF35, POP_1_1, PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * or equal to the second value, when comparing unsigned integer values or unordered float values.
     */
    final val Ble_Un_S = new OpCode()
	opcode(Ble_Un_S, CEE_BLE_UN_S , "ble.un.s", 0xFFFFFF36, POP_1_1, PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * the second value, when comparing unsigned integer values or unordered float values.
     */
    final val Blt_Un_S = new OpCode()
	opcode(Blt_Un_S, CEE_BLT_UN_S, "blt.un.s", 0xFFFFFF37, POP_1_1, PUSH_NONE, INLINE_TARGET_S, FLOW_COND_BRANCH)

    /**
     * Unconditionally transfers control to a target instruction.
     */
    final val Br = new OpCode()
	opcode(Br, CEE_BR       , "br"       , 0xFFFFFF38, POP_NONE, PUSH_NONE, INLINE_TARGET, FLOW_BRANCH)

    /**
     * Transfers control to a target instruction if value is false, a null reference
     * (Nothing in Visual Basic), or zero.
     */
    final val Brfalse = new OpCode()
	opcode(Brfalse, CEE_BRFALSE, "brfalse", 0xFFFFFF39, POP_I, PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if value is true, not null, or non-zero.
     */
    final val Brtrue = new OpCode()
	opcode(Brtrue, CEE_BRTRUE , "brtrue", 0xFFFFFF3A, POP_I   , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if two values are equal.
     */
    final val Beq = new OpCode()
	opcode(Beq, CEE_BEQ, "beq", 0xFFFFFF3B, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if the first value is greater than or
     * equal to the second value.
     */
    final val Bge = new OpCode()
	opcode(Bge, CEE_BGE, "bge", 0xFFFFFF3C, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if the first value is greater than the second value.
     */
    final val Bgt = new OpCode()
	opcode(Bgt, CEE_BGT, "bgt", 0xFFFFFF3D, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if the first value is less than or equal
     * to the second value.
     */
    final val Ble = new OpCode()
	opcode(Ble, CEE_BLE, "ble", 0xFFFFFF3E, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     *  Transfers control to a target instruction if the first value is less than the second value.
     */
    final val Blt = new OpCode()
	opcode(Blt, CEE_BLT, "blt", 0xFFFFFF3F, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction when two unsigned integer values or
     * unordered float values are not equal.
     */
    final val Bne_Un = new OpCode()
	opcode(Bne_Un, CEE_BNE_UN , "bne.un", 0xFFFFFF40, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if the first value is greather than
     * the second value, when comparing unsigned integer values or unordered float values.
     */
    final val Bge_Un = new OpCode()
	opcode(Bge_Un, CEE_BGE_UN , "bge.un", 0xFFFFFF41, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if the first value is greater than the
     * second value, when comparing unsigned integer values or unordered float values.
     */
    final val Bgt_Un = new OpCode()
	opcode(Bgt_Un, CEE_BGT_UN , "bgt.un", 0xFFFFFF42, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if the first value is less than or equal to
     * the second value, when comparing unsigned integer values or unordered float values.
     */
    final val Ble_Un = new OpCode()
	opcode(Ble_Un, CEE_BLE_UN , "ble.un"   , 0xFFFFFF43, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Transfers control to a target instruction if the first value is less than the second value,
     * when comparing unsigned integer values or unordered float values.
     */
    final val Blt_Un = new OpCode()
	opcode(Blt_Un, CEE_BLT_UN , "blt.un", 0xFFFFFF44, POP_1_1 , PUSH_NONE, INLINE_TARGET, FLOW_COND_BRANCH)

    /**
     * Implements a jump table.
     */
    final val Switch = new OpCode()
	opcode(Switch, CEE_SWITCH , "switch", 0xFFFFFF45, POP_I   , PUSH_NONE, INLINE_SWITCH, FLOW_COND_BRANCH)

    /**
     * Loads a value of type int8 as an int32 onto the evaluation stack indirectly.
     */
    final val Ldind_I1 = new OpCode()
	opcode(Ldind_I1, CEE_LDIND_I1 , "ldind.i1" , 0xFFFFFF46, POP_I   , PUSH_I  , INLINE_NONE, FLOW_NEXT)

    /**
     *  Loads a value of type int16 as an int32 onto the evaluation stack indirectly.
     */
    final val Ldind_I2 = new OpCode()
	opcode(Ldind_I2, CEE_LDIND_I2 , "ldind.i2" , 0xFFFFFF48, POP_I   , PUSH_I  , INLINE_NONE, FLOW_NEXT)

    /**
     * Loads a value of type int32 as an int32 onto the evaluation stack indirectly.
     */
    final val Ldind_I4 = new OpCode()
	opcode(Ldind_I4, CEE_LDIND_I4 , "ldind.i4" , 0xFFFFFF4A, POP_I   , PUSH_I  , INLINE_NONE, FLOW_NEXT)

    /**
     * Loads a value of type int64 as an int64 onto the evaluation stack indirectly.
     */
    final val Ldind_I8 = new OpCode()
	opcode(Ldind_I8, CEE_LDIND_I8 , "ldind.i8" , 0xFFFFFF4C, POP_I   , PUSH_I8 , INLINE_NONE, FLOW_NEXT)

    /**
     * Loads a value of type natural int as a natural int onto the evaluation stack indirectly.
     */
    final val Ldind_I = new OpCode()
	opcode(Ldind_I, CEE_LDIND_I  , "ldind.i"  , 0xFFFFFF4D, POP_I   , PUSH_I  , INLINE_NONE, FLOW_NEXT)

    /**
     *  Loads a value of type float32 as a type F (float) onto the evaluation stack indirectly.
     */
    final val Ldind_R4 = new OpCode()
	opcode(Ldind_R4, CEE_LDIND_R4 , "ldind.r4" , 0xFFFFFF4E, POP_I   , PUSH_R4 , INLINE_NONE, FLOW_NEXT)

    /**
     * Loads a value of type float64 as a type F (float) onto the evaluation stack indirectly.
     */
    final val Ldind_R8 = new OpCode()
	opcode(Ldind_R8, CEE_LDIND_R8 , "ldind.r8" , 0xFFFFFF4F, POP_I   , PUSH_R8 , INLINE_NONE, FLOW_NEXT)

    /**
     * Loads an object reference as a type O (object reference) onto the evaluation stack indirectly.
     */
    final val Ldind_Ref = new OpCode()
	opcode(Ldind_Ref, CEE_LDIND_REF, "ldind.ref", 0xFFFFFF50, POP_I   , PUSH_REF, INLINE_NONE, FLOW_NEXT)

    /**
     * Loads a value of type unsigned int8 as an int32 onto the evaluation stack indirectly.
     */
    final val Ldind_U1 = new OpCode()
	opcode(Ldind_U1, CEE_LDIND_U1 , "ldind.u1" , 0xFFFFFF47, POP_I   , PUSH_I  , INLINE_NONE, FLOW_NEXT)

    /**
     * Loads a value of type unsigned int16 as an int32 onto the evaluation stack indirectly.
     */
    final val Ldind_U2 = new OpCode()
	opcode(Ldind_U2, CEE_LDIND_U2 , "ldind.u2" , 0xFFFFFF49, POP_I   , PUSH_I  , INLINE_NONE, FLOW_NEXT)

    /**
     * Loads a value of type unsigned int32 as an int32 onto the evaluation stack indirectly.
     */
    final val Ldind_U4 = new OpCode()
	opcode(Ldind_U4, CEE_LDIND_U4 , "ldind.u4" , 0xFFFFFF4B, POP_I   , PUSH_I  , INLINE_NONE, FLOW_NEXT)

    /**
     * Stores a object reference value at a supplied address.
     */
    final val Stind_Ref = new OpCode()
	opcode(Stind_Ref, CEE_STIND_REF, "stind.ref", 0xFFFFFF51, POP_I_I , PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Stores a value of type int8 at a supplied address.
     */
    final val Stind_I1 = new OpCode()
	opcode(Stind_I1, CEE_STIND_I1 , "stind.i1", 0xFFFFFF52, POP_I_I , PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Stores a value of type int16 at a supplied address.
     */
    final val Stind_I2 = new OpCode()
	opcode(Stind_I2, CEE_STIND_I2 , "stind.i2", 0xFFFFFF53, POP_I_I , PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Stores a value of type int32 at a supplied address.
     */
    final val Stind_I4 = new OpCode()
	opcode(Stind_I4, CEE_STIND_I4 , "stind.i4", 0xFFFFFF54, POP_I_I , PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Stores a value of type int64 at a supplied address.
     */
    final val Stind_I8 = new OpCode()
	opcode(Stind_I8, CEE_STIND_I8 , "stind.i8", 0xFFFFFF55, POP_I_I8, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Stores a value of type float32 at a supplied address.
     */
    final val Stind_R4 = new OpCode()
	opcode(Stind_R4, CEE_STIND_R4 , "stind.r4", 0xFFFFFF56, POP_I_R4, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Stores a value of type float64 at a supplied address.
     */
    final val Stind_R8 = new OpCode()
	opcode(Stind_R8, CEE_STIND_R8 , "stind.r8", 0xFFFFFF57, POP_I_R8, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Subtracts one value from another and pushes the result onto the evaluation stack.
     */
    final val Sub = new OpCode()
	opcode(Sub, CEE_SUB, "sub"    , 0xFFFFFF59, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Multiplies two values and pushes the result on the evaluation stack.
     */
    final val Mul = new OpCode()
	opcode(Mul, CEE_MUL, "mul"    , 0xFFFFFF5A, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Divides two values and pushes the result as a floating-point (type F) or
     * quotient (type int32) onto the evaluation stack.
     */
    final val Div = new OpCode()
	opcode(Div, CEE_DIV, "div"    , 0xFFFFFF5B, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Divides two unsigned integer values and pushes the result (int32) onto the evaluation stack.
     */
    final val Div_Un = new OpCode()
	opcode(Div_Un, CEE_DIV_UN, "div.un" , 0xFFFFFF5C, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Divides two values and pushes the remainder onto the evaluation stack.
     */
    final val Rem = new OpCode()
	opcode(Rem, CEE_REM   , "rem"    , 0xFFFFFF5D, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Divides two unsigned values and pushes the remainder onto the evaluation stack.
     */
    final val Rem_Un = new OpCode()
	opcode(Rem_Un, CEE_REM_UN, "rem.un" , 0xFFFFFF5E, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Computes the bitwise AND of two values and pushes the result onto the evaluation stack.
     */
    final val And = new OpCode()
	opcode(And, CEE_AND, "and"    , 0xFFFFFF5F, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Compute the bitwise complement of the two integer values on top of the stack and
     * pushes the result onto the evaluation stack.
     */
    final val Or = new OpCode()
	opcode(Or, CEE_OR , "or"     , 0xFFFFFF60, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Computes the bitwise XOR of the top two values on the evaluation stack,
     * pushing the result onto the evaluation stack.
     */
    final val Xor = new OpCode()
	opcode(Xor, CEE_XOR, "xor"    , 0xFFFFFF61, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Shifts an integer value to the left (in zeroes) by a specified number of bits,
     *  pushing the result onto the evaluation stack.
     */
    final val Shl = new OpCode()
	opcode(Shl, CEE_SHL, "shl"    , 0xFFFFFF62, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Shifts an integer value (in sign) to the right by a specified number of bits,
     * pushing the result onto the evaluation stack.
     */
    final val Shr = new OpCode()
	opcode(Shr, CEE_SHR, "shr"    , 0xFFFFFF63, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Shifts an unsigned integer value (in zeroes) to the right by a specified number of bits,
     * pushing the result onto the evaluation stack.
     */
    final val Shr_Un = new OpCode()
	opcode(Shr_Un, CEE_SHR_UN, "shr.un" , 0xFFFFFF64, POP_1_1, PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Negates a value and pushes the result onto the evaluation stack.
     */
    final val Neg = new OpCode()
	opcode(Neg, CEE_NEG , "neg"    , 0xFFFFFF65, POP_1  , PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     * Computes the bitwise complement of the integer value on top of the stack and pushes
     * the result onto the evaluation stack as the same type.
     */
    final val Not = new OpCode()
	opcode(Not, CEE_NOT , "not"    , 0xFFFFFF66, POP_1  , PUSH_1 , INLINE_NONE, FLOW_NEXT)

    /**
     *  Converts the value on top of the evaluation stack to int8, then extends (pads) it to int32.
     */
    final val Conv_I1 = new OpCode()
	opcode(Conv_I1, CEE_CONV_I1, "conv.i1", 0xFFFFFF67, POP_1  , PUSH_I , INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to int16, then extends (pads) it to int32.
     */
    final val Conv_I2 = new OpCode()
	opcode(Conv_I2, CEE_CONV_I2, "conv.i2", 0xFFFFFF68, POP_1  , PUSH_I , INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to int32.
     */
    final val Conv_I4 = new OpCode()
	opcode(Conv_I4, CEE_CONV_I4, "conv.i4", 0xFFFFFF69, POP_1  , PUSH_I , INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to int64.
     */
    final val Conv_I8 = new OpCode()
	opcode(Conv_I8, CEE_CONV_I8, "conv.i8", 0xFFFFFF6A, POP_1  , PUSH_I8, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to float32.
     */
    final val Conv_R4 = new OpCode()
	opcode(Conv_R4, CEE_CONV_R4, "conv.r4", 0xFFFFFF6B, POP_1  , PUSH_R4, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to float64.
     */
    final val Conv_R8 = new OpCode()
	opcode(Conv_R8, CEE_CONV_R8, "conv.r8", 0xFFFFFF6C, POP_1  , PUSH_R8, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to unsigned int32, and extends it to int32.
     */
    final val Conv_U4 = new OpCode()
	opcode(Conv_U4, CEE_CONV_U4, "conv.u4", 0xFFFFFF6D, POP_1  , PUSH_I , INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to unsigned int64, and extends it to int64.
     */
    final val Conv_U8 = new OpCode()
	opcode(Conv_U8, CEE_CONV_U8, "conv.u8", 0xFFFFFF6E, POP_1  , PUSH_I8, INLINE_NONE, FLOW_NEXT)

    /**
     * Calls a late-bound method on an object, pushing the return value onto the evaluation stack.
     */
    final val Callvirt = new OpCode()
	opcode(Callvirt, CEE_CALLVIRT, "callvirt", 0xFFFFFF6F,POP_SPECIAL,PUSH_SPECIAL,INLINE_METHOD,FLOW_CALL)

    /**
     * Copies the value type located at the address of an object (type &, * or natural int)
     * to the address of the destination object (type &, * or natural int).
     */
    final val Cpobj = new OpCode()
	opcode(Cpobj, CEE_CPOBJ , "cpobj" , 0xFFFFFF70, POP_I_I , PUSH_NONE, INLINE_TYPE  , FLOW_NEXT)

    /**
     * Copies the value type object pointed to by an address to the top of the evaluation stack.
     */
    final val Ldobj = new OpCode()
	opcode(Ldobj, CEE_LDOBJ , "ldobj" , 0xFFFFFF71, POP_I    , PUSH_1   , INLINE_TYPE  , FLOW_NEXT)

    /**
     * Pushes a new object reference to a string literal stored in the metadata.
     */
    final val Ldstr = new OpCode()
	opcode(Ldstr, CEE_LDSTR , "ldstr" , 0xFFFFFF72, POP_NONE  , PUSH_REF , INLINE_STRING, FLOW_NEXT)

    /**
     * Creates a new object or a new instance of a value type, pushing an object reference
     * (type O) onto the evaluation stack.
     */
    final val Newobj = new OpCode()
	opcode(Newobj, CEE_NEWOBJ, "newobj", 0xFFFFFF73, POP_SPECIAL , PUSH_REF , INLINE_METHOD, FLOW_CALL)

    /**
     * Attempts to cast an object passed by reference to the specified class.
     */
    final val Castclass = new OpCode()
	opcode(Castclass, CEE_CASTCLASS, "castclass", 0xFFFFFF74, POP_REF  , PUSH_REF , INLINE_TYPE  , FLOW_NEXT)

    /**
     * Tests whether an object reference (type O) is an instance of a particular class.
     */
    final val Isinst = new OpCode()
	opcode(Isinst, CEE_ISINST   , "isinst"   , 0xFFFFFF75, POP_REF  , PUSH_I   , INLINE_TYPE  , FLOW_NEXT)

    /**
     *  Converts the unsigned integer value on top of the evaluation stack to float32.
     */
    final val Conv_R_Un = new OpCode()
	opcode(Conv_R_Un, CEE_CONV_R_UN, "conv.r.un", 0xFFFFFF76, POP_1    , PUSH_R8  , INLINE_NONE  , FLOW_NEXT)

    /**
     * Converts the boxed representation of a value type to its unboxed form.
     */
    final val Unbox = new OpCode()
	opcode(Unbox, CEE_UNBOX  , "unbox"  , 0xFFFFFF79, POP_REF  , PUSH_I   , INLINE_TYPE  , FLOW_NEXT)

    /**
     * Throws the exception object currently on the evaluation stack.
     */
    final val Throw = new OpCode()
	opcode(Throw, CEE_THROW  , "throw"  , 0xFFFFFF7A, POP_REF  , PUSH_NONE, INLINE_NONE  , FLOW_THROW)

    /**
     *  Finds the value of a field in the object whose reference is currently
     * on the evaluation stack.
     */
    final val Ldfld = new OpCode()
	opcode(Ldfld, CEE_LDFLD  , "ldfld"  , 0xFFFFFF7B, POP_REF  , PUSH_1   , INLINE_FIELD , FLOW_NEXT)

    /**
     *  Finds the address of a field in the object whose reference is currently
     * on the evaluation stack.
     */
    final val Ldflda = new OpCode()
	opcode(Ldflda, CEE_LDFLDA , "ldflda" , 0xFFFFFF7C, POP_REF  , PUSH_I   , INLINE_FIELD , FLOW_NEXT)

    /**
     * Pushes the value of a static field onto the evaluation stack.
     */
    final val Ldsfld = new OpCode()
	opcode(Ldsfld, CEE_LDSFLD , "ldsfld" , 0xFFFFFF7E, POP_NONE , PUSH_1   , INLINE_FIELD , FLOW_NEXT)

    /**
     * Pushes the address of a static field onto the evaluation stack.
     */
    final val Ldsflda = new OpCode()
	opcode(Ldsflda, CEE_LDSFLDA, "ldsflda", 0xFFFFFF7F, POP_NONE , PUSH_I   , INLINE_FIELD , FLOW_NEXT)

    /**
     *  Replaces the value stored in the field of an object reference or pointer with a new value.
     */
    final val Stfld = new OpCode()
	opcode(Stfld, CEE_STFLD  , "stfld"  , 0xFFFFFF7D, POP_REF_1, PUSH_NONE, INLINE_FIELD , FLOW_NEXT)

    /**
     * Replaces the value of a static field with a value from the evaluation stack.
     */
    final val Stsfld = new OpCode()
	opcode(Stsfld, CEE_STSFLD , "stsfld" , 0xFFFFFF80, POP_1    , PUSH_NONE, INLINE_FIELD , FLOW_NEXT)

    /**
     * Copies a value of a specified type from the evaluation stack into a supplied memory address.
     */
    final val Stobj = new OpCode()
	opcode(Stobj, CEE_STOBJ  , "stobj"  , 0xFFFFFF81, POP_I_1, PUSH_NONE, INLINE_TYPE  , FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to signed int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I1_Un = new OpCode()
	opcode(Conv_Ovf_I1_Un, CEE_CONV_OVF_I1_UN, "conv.ovf.i1.un", 0xFFFFFF82, POP_1,PUSH_I,INLINE_NONE, FLOW_NEXT)

    /**
     *  Converts the unsigned value on top of the evaluation stack to signed int16 and
     * extends it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I2_Un = new OpCode()
	opcode(Conv_Ovf_I2_Un, CEE_CONV_OVF_I2_UN, "conv.ovf.i2.un", 0xFFFFFF83,POP_1,PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to signed int32,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I4_Un = new OpCode()
	opcode(Conv_Ovf_I4_Un, CEE_CONV_OVF_I4_UN, "conv.ovf.i4.un", 0xFFFFFF84,POP_1,PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to signed int64,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I8_Un = new OpCode()
	opcode(Conv_Ovf_I8_Un, CEE_CONV_OVF_I8_UN, "conv.ovf.i8.un", 0xFFFFFF85,POP_1,PUSH_I8, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to signed natural int,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I_Un = new OpCode()
	opcode(Conv_Ovf_I_Un, CEE_CONV_OVF_I_UN , "conv.ovf.i.un" , 0xFFFFFF8A,POP_1,PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U1_Un = new OpCode()
	opcode(Conv_Ovf_U1_Un, CEE_CONV_OVF_U1_UN, "conv.ovf.u1.un", 0xFFFFFF86,POP_1,PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int16 and
     * extends it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U2_Un = new OpCode()
	opcode(Conv_Ovf_U2_Un, CEE_CONV_OVF_U2_UN, "conv.ovf.u2.un", 0xFFFFFF87,POP_1,PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int32,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U4_Un = new OpCode()
	opcode(Conv_Ovf_U4_Un, CEE_CONV_OVF_U4_UN, "conv.ovf.u4.un", 0xFFFFFF88,POP_1,PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int64,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U8_Un = new OpCode()
	opcode(Conv_Ovf_U8_Un, CEE_CONV_OVF_U8_UN, "conv.ovf.u8.un", 0xFFFFFF89,POP_1,PUSH_I8, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned natural int,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U_Un = new OpCode()
	opcode(Conv_Ovf_U_Un, CEE_CONV_OVF_U_UN , "conv.ovf.u.un" , 0xFFFFFF8B,POP_1,PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts a value type to an object reference (type O).
     */
    final val Box = new OpCode()
	opcode(Box, CEE_BOX       , "box"       , 0xFFFFFF8C, POP_1  , PUSH_REF , INLINE_TYPE , FLOW_NEXT)

    /**
     * Pushes an object reference to a new zero-based, one-dimensional array whose elements
     * are of a specific type onto the evaluation stack.
     */
    final val Newarr = new OpCode()
	opcode(Newarr, CEE_NEWARR, "newarr"    , 0xFFFFFF8D, POP_I  , PUSH_REF , INLINE_TYPE , FLOW_NEXT)

    /**
     * Pushes the number of elements of a zero-based, one-dimensional array
     * onto the evaluation stack.
     */
    final val Ldlen = new OpCode()
	opcode(Ldlen, CEE_LDLEN, "ldlen", 0xFFFFFF8E, POP_REF, PUSH_I,INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the address of the array element at a specified array index onto
     * the top of the evaluation stack as type & (managed pointer).
     */
    final val Ldelema = new OpCode()
	opcode(Ldelema, CEE_LDELEMA, "ldelema"   , 0xFFFFFF8F, POP_REF_I, PUSH_I, INLINE_TYPE , FLOW_NEXT)

    /**
     * Loads the element with type natural int at a specified array index onto the top
     * of the evaluation stack as a natural int.
     */
    final val Ldelem_I = new OpCode()
	opcode(Ldelem_I, CEE_LDELEM_I, "ldelem.i"  , 0xFFFFFF97, POP_REF_I, PUSH_I, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element with type int8 at a specified array index onto the top of the
     * evaluation stack as an int32.
     */
    final val Ldelem_I1 = new OpCode()
	opcode(Ldelem_I1, CEE_LDELEM_I1, "ldelem.i1" , 0xFFFFFF90, POP_REF_I, PUSH_I, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element with type int16 at a specified array index onto the top of
     * the evaluation stack as an int32.
     */
    final val Ldelem_I2 = new OpCode()
	opcode(Ldelem_I2, CEE_LDELEM_I2, "ldelem.i2" , 0xFFFFFF92, POP_REF_I, PUSH_I, INLINE_NONE , FLOW_NEXT)

    /**
     *  Loads the element with type int32 at a specified array index onto the top of the
     * evaluation stack as an int32.
     */
    final val Ldelem_I4 = new OpCode()
	opcode(Ldelem_I4, CEE_LDELEM_I4, "ldelem.i4" , 0xFFFFFF94, POP_REF_I, PUSH_I, INLINE_NONE , FLOW_NEXT)

    /**
     *  Loads the element with type int64 at a specified array index onto the top of the
     * evaluation stack as an int64.
     */
    final val Ldelem_I8 = new OpCode()
	opcode(Ldelem_I8, CEE_LDELEM_I8, "ldelem.i8" , 0xFFFFFF96, POP_REF_I, PUSH_I8, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element with type float32 at a specified array index onto the top of the
     * evaluation stack as type F (float)
     */
    final val Ldelem_R4 = new OpCode()
	opcode(Ldelem_R4, CEE_LDELEM_R4, "ldelem.r4" , 0xFFFFFF98, POP_REF_I, PUSH_R4, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element with type float64 at a specified array index onto the top of the
     * evaluation stack as type F (float) .
     */
    final val Ldelem_R8 = new OpCode()
	opcode(Ldelem_R8, CEE_LDELEM_R8, "ldelem.r8" , 0xFFFFFF99, POP_REF_I, PUSH_R8, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element containing an object reference at a specified array index onto
     * the top of the evaluation stack as type O (object reference).
     */
    final val Ldelem_Ref = new OpCode()
	opcode(Ldelem_Ref, CEE_LDELEM_REF, "ldelem.ref", 0xFFFFFF9A, POP_REF_I, PUSH_REF, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element with type unsigned int8 at a specified array index onto the top
     * of the evaluation stack as an int32.
     */
    final val Ldelem_U1 = new OpCode()
	opcode(Ldelem_U1, CEE_LDELEM_U1, "ldelem.u1" , 0xFFFFFF91, POP_REF_I, PUSH_I, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element with type unsigned int16 at a specified array index onto the top
     * of the evaluation stack as an int32.
     */
    final val Ldelem_U2 = new OpCode()
	opcode(Ldelem_U2, CEE_LDELEM_U2, "ldelem.u2" , 0xFFFFFF93, POP_REF_I, PUSH_I, INLINE_NONE , FLOW_NEXT)

    /**
     * Loads the element with type unsigned int32 at a specified array index onto the top
     * of the evaluation stack as an int32.
     */
    final val Ldelem_U4 = new OpCode()
	opcode(Ldelem_U4, CEE_LDELEM_U4, "ldelem.u4" , 0xFFFFFF95, POP_REF_I, PUSH_I, INLINE_NONE , FLOW_NEXT)

    /**
     *  Replaces the array element at a given index with the natural int value on
     * the evaluation stack.
     */
    final val Stelem_I = new OpCode()
	opcode(Stelem_I, CEE_STELEM_I, "stelem.i", 0xFFFFFF9B, POP_REF_I_I, PUSH_NONE, INLINE_NONE , FLOW_NEXT)

    /**
     * Replaces the array element at a given index with the int8 value on the evaluation stack.
     */
    final val Stelem_I1 = new OpCode()
	opcode(Stelem_I1, CEE_STELEM_I1, "stelem.i1", 0xFFFFFF9C, POP_REF_I_I, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     *  Replaces the array element at a given index with the int16 value on the evaluation stack.
     */
    final val Stelem_I2 = new OpCode()
	opcode(Stelem_I2, CEE_STELEM_I2, "stelem.i2", 0xFFFFFF9D, POP_REF_I_I, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     *  Replaces the array element at a given index with the int32 value on the evaluation stack.
     */
    final val Stelem_I4 = new OpCode()
	opcode(Stelem_I4, CEE_STELEM_I4, "stelem.i4", 0xFFFFFF9E, POP_REF_I_I, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Replaces the array element at a given index with the int64 value on the evaluation stack.
     */
    final val Stelem_I8 = new OpCode()
	opcode(Stelem_I8, CEE_STELEM_I8,"stelem.i8", 0xFFFFFF9F, POP_REF_I_I8, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Replaces the array element at a given index with the float32 value on the evaluation stack.
     */
    final val Stelem_R4 = new OpCode()
	opcode(Stelem_R4, CEE_STELEM_R4,"stelem.r4", 0xFFFFFFA0, POP_REF_I_R4, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Replaces the array element at a given index with the float64 value on the evaluation stack.
     */
    final val Stelem_R8 = new OpCode()
	opcode(Stelem_R8, CEE_STELEM_R8,"stelem.r8", 0xFFFFFFA1, POP_REF_I_R8, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Replaces the array element at a given index with the object ref value (type O)
     * on the evaluation stack.
     */
    final val Stelem_Ref = new OpCode()
	opcode(Stelem_Ref, CEE_STELEM_REF,"stelem.ref",0xFFFFFFA2,POP_REF_I_REF,PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to signed int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I1 = new OpCode()
	opcode(Conv_Ovf_I1, CEE_CONV_OVF_I1, "conv.ovf.i1", 0xFFFFFFB3, POP_1, PUSH_I , INLINE_NONE , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to signed int16 and
     * extending it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I2 = new OpCode()
	opcode(Conv_Ovf_I2, CEE_CONV_OVF_I2, "conv.ovf.i2", 0xFFFFFFB5, POP_1, PUSH_I , INLINE_NONE , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to signed int32,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I4 = new OpCode()
	opcode(Conv_Ovf_I4, CEE_CONV_OVF_I4, "conv.ovf.i4", 0xFFFFFFB7, POP_1, PUSH_I , INLINE_NONE , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to signed int64,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I8 = new OpCode()
	opcode(Conv_Ovf_I8, CEE_CONV_OVF_I8, "conv.ovf.i8", 0xFFFFFFB9, POP_1, PUSH_I8, INLINE_NONE , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to unsigned int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U1 = new OpCode()
	opcode(Conv_Ovf_U1, CEE_CONV_OVF_U1, "conv.ovf.u1", 0xFFFFFFB4, POP_1, PUSH_I , INLINE_NONE , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to unsigned int16 and
     * extends it to int32, throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U2 = new OpCode()
	opcode(Conv_Ovf_U2, CEE_CONV_OVF_U2, "conv.ovf.u2", 0xFFFFFFB6, POP_1, PUSH_I , INLINE_NONE , FLOW_NEXT)

    /**
     *  Converts the signed value on top of the evaluation stack to unsigned int32,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U4 = new OpCode()
	opcode(Conv_Ovf_U4, CEE_CONV_OVF_U4, "conv.ovf.u4", 0xFFFFFFB8, POP_1, PUSH_I , INLINE_NONE , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to unsigned int64,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U8 = new OpCode()
	opcode(Conv_Ovf_U8, CEE_CONV_OVF_U8, "conv.ovf.u8", 0xFFFFFFBA, POP_1, PUSH_I8, INLINE_NONE , FLOW_NEXT)

    /**
     *  Retrieves the address (type &) embedded in a typed reference.
     */
    final val Refanyval = new OpCode()
	opcode(Refanyval, CEE_REFANYVAL, "refanyval", 0xFFFFFFC2, POP_1, PUSH_I   , INLINE_TYPE  , FLOW_NEXT)

    /**
     * Retrieves the type token embedded in a typed reference .
     */
    final val Refanytype = new OpCode()
	opcode(Refanytype, CEE_REFANYTYPE, "refanytype", 0xFFFFFE1D, POP_1    , PUSH_I   , INLINE_NONE, FLOW_NEXT)

    /**
     * Throws ArithmeticException if value is not a finite number.
     */
    final val Ckfinite = new OpCode()
	opcode(Ckfinite, CEE_CKFINITE, "ckfinite" , 0xFFFFFFC3, POP_1, PUSH_R8  , INLINE_NONE  , FLOW_NEXT)

    /**
     * Pushes a typed reference to an instance of a specific type onto the evaluation stack.
     */
    final val Mkrefany = new OpCode()
	opcode(Mkrefany, CEE_MKREFANY, "mkrefany" , 0xFFFFFFC6, POP_I, PUSH_1   , INLINE_TYPE  , FLOW_NEXT)

    /**
     * Converts a metadata token to its runtime representation, pushing it onto the evaluation stack.
     */
    final val Ldtoken = new OpCode()
	opcode(Ldtoken, CEE_LDTOKEN    , "ldtoken"   , 0xFFFFFFD0, POP_NONE, PUSH_I, INLINE_TOKEN , FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to unsigned int8, and extends it to int32.
     */
    final val Conv_U1 = new OpCode()
	opcode(Conv_U1, CEE_CONV_U1    , "conv.u1"   , 0xFFFFFFD2, POP_1, PUSH_I, INLINE_NONE  , FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to unsigned int16, and extends it to int32.
     */
    final val Conv_U2 = new OpCode()
	opcode(Conv_U2, CEE_CONV_U2    , "conv.u2"   , 0xFFFFFFD1, POP_1, PUSH_I, INLINE_NONE  , FLOW_NEXT)

    /**
     * Converts the value on top of the evaluation stack to natural int.
     */
    final val Conv_I = new OpCode()
	opcode(Conv_I, CEE_CONV_I     , "conv.i"    , 0xFFFFFFD3, POP_1, PUSH_I, INLINE_NONE  , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to signed natural int,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_I = new OpCode()
	opcode(Conv_Ovf_I, CEE_CONV_OVF_I , "conv.ovf.i", 0xFFFFFFD4, POP_1, PUSH_I, INLINE_NONE  , FLOW_NEXT)

    /**
     * Converts the signed value on top of the evaluation stack to unsigned natural int,
     * throwing OverflowException on overflow.
     */
    final val Conv_Ovf_U = new OpCode()
	opcode(Conv_Ovf_U, CEE_CONV_OVF_U , "conv.ovf.u", 0xFFFFFFD5, POP_1, PUSH_I, INLINE_NONE  , FLOW_NEXT)

    /**
     * Adds two integers, performs an overflow check, and pushes the result
     * onto the evaluation stack.
     */
    final val Add_Ovf = new OpCode()
	opcode(Add_Ovf, CEE_ADD_OVF    , "add.ovf"   , 0xFFFFFFD6, POP_1_1, PUSH_1, INLINE_NONE  , FLOW_NEXT)

    /**
     *  Adds two unsigned integer values, performs an overflow check, and pushes the result
     * onto the evaluation stack.
     */
    final val Add_Ovf_Un = new OpCode()
	opcode(Add_Ovf_Un, CEE_ADD_OVF_UN , "add.ovf.un", 0xFFFFFFD7, POP_1_1, PUSH_1, INLINE_NONE  , FLOW_NEXT)

    /**
     * Multiplies two integer values, performs an overflow check, and pushes the result
     * onto the evaluation stack.
     */
    final val Mul_Ovf = new OpCode()
	opcode(Mul_Ovf, CEE_MUL_OVF    , "mul.ovf"   , 0xFFFFFFD8, POP_1_1, PUSH_1, INLINE_NONE  , FLOW_NEXT)

    /**
     * Multiplies two unsigned integer values , performs an overflow check ,
     * and pushes the result onto the evaluation stack.
     */
    final val Mul_Ovf_Un = new OpCode()
	opcode(Mul_Ovf_Un, CEE_MUL_OVF_UN , "mul.ovf.un", 0xFFFFFFD9, POP_1_1, PUSH_1, INLINE_NONE  , FLOW_NEXT)

    /**
     * Subtracts one integer value from another, performs an overflow check,
     * and pushes the result onto the evaluation stack.
     */
    final val Sub_Ovf = new OpCode()
	opcode(Sub_Ovf, CEE_SUB_OVF   , "sub.ovf"   , 0xFFFFFFDA, POP_1_1, PUSH_1, INLINE_NONE  , FLOW_NEXT)

    /**
     * Subtracts one unsigned integer value from another, performs an overflow check,
     * and pushes the result onto the evaluation stack.
     */
    final val Sub_Ovf_Un = new OpCode()
	opcode(Sub_Ovf_Un, CEE_SUB_OVF_UN, "sub.ovf.un", 0xFFFFFFDB, POP_1_1, PUSH_1, INLINE_NONE  , FLOW_NEXT)

    /**
     * Transfers control from the fault or finally clause of an exception block back to
     * the Common Language Infrastructure (CLI) exception handler.
     */
    final val Endfinally = new OpCode()
	opcode(Endfinally, CEE_ENDFINALLY, "endfinally", 0xFFFFFFDC, POP_NONE, PUSH_NONE, INLINE_NONE, FLOW_RETURN)

    /**
     * Exits a protected region of code, unconditionally tranferring control
     * to a specific target instruction.
     */
    final val Leave = new OpCode()
	opcode(Leave, CEE_LEAVE, "leave", 0xFFFFFFDD, POP_NONE, PUSH_NONE, INLINE_TARGET, FLOW_BRANCH)

    /**
     * Exits a protected region of code, unconditionally tranferring control
     * to a target instruction (short form).
     */
    final val Leave_S = new OpCode()
	opcode(Leave_S, CEE_LEAVE_S, "leave.s", 0xFFFFFFDE, POP_NONE, PUSH_NONE, INLINE_TARGET_S, FLOW_BRANCH)

    /**
     * Stores a value of type natural int at a supplied address.
     */
    final val Stind_I = new OpCode()
	opcode(Stind_I, CEE_STIND_I, "stind.i", 0xFFFFFFDF, POP_I_I , PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     *  Converts the value on top of the evaluation stack to unsigned natural int,
     * and extends it to natural int.
     */
    final val Conv_U = new OpCode()
	opcode(Conv_U, CEE_CONV_U, "conv.u", 0xFFFFFFE0, POP_1   , PUSH_I   , INLINE_NONE, FLOW_NEXT)

    /**
     * Returns an unmanaged pointer to the argument list of the current method.
     */
    final val Arglist = new OpCode()
	opcode(Arglist, CEE_ARGLIST, "arglist"  , 0xFFFFFE00, POP_NONE, PUSH_I   , INLINE_NONE, FLOW_NEXT)

    /**
     * Compares two values. If they are equal, the integer value 1 (int32) is pushed
     * onto the evaluation stack otherwise 0 (int32) is pushed onto the evaluation stack.
     */
    final val Ceq = new OpCode()
	opcode(Ceq, CEE_CEQ, "ceq", 0xFFFFFE01, POP_1_1 , PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Compares two values. If the first value is greater than the second,
     * the integer value 1 (int32) is pushed onto the evaluation stack
     * otherwise 0 (int32) is pushed onto the evaluation stack.
     */
    final val Cgt = new OpCode()
	opcode(Cgt, CEE_CGT, "cgt", 0xFFFFFE02, POP_1_1 , PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     *  Compares two unsigned or unordered values. If the first value is greater than
     * the second, the integer value 1 (int32) is pushed onto the evaluation stack
     * otherwise 0 (int32) is pushed onto the evaluation stack.
     */
    final val Cgt_Un = new OpCode()
	opcode(Cgt_Un, CEE_CGT_UN, "cgt.un", 0xFFFFFE03, POP_1_1 , PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Compares two values. If the first value is less than the second,
     * the integer value 1 (int32) is pushed onto the evaluation stack
     * otherwise 0 (int32) is pushed onto the evaluation stack.
     */
    final val Clt = new OpCode()
	opcode(Clt, CEE_CLT, "clt"      , 0xFFFFFE04, POP_1_1 , PUSH_I   , INLINE_NONE, FLOW_NEXT)

    /**
     *  Compares the unsigned or unordered values value1 and value2. If value1 is
     * less than value2, then the integer value 1 (int32) is pushed onto the
     * evaluation stack otherwise 0 (int32) is pushed onto the evaluation stack.
     */
    final val Clt_Un = new OpCode()
	opcode(Clt_Un, CEE_CLT_UN , "clt.un"   , 0xFFFFFE05, POP_1_1 , PUSH_I   , INLINE_NONE, FLOW_NEXT)

    /**
     * Pushes an unmanaged pointer (type natural int) to the native code implementing
     * a specific method onto the evaluation stack.
     */
    final val Ldftn = new OpCode()
	opcode(Ldftn, CEE_LDFTN , "ldftn"    , 0xFFFFFE06, POP_NONE, PUSH_I   , INLINE_METHOD, FLOW_NEXT)

    /**
     * Pushes an unmanaged pointer (type natural int) to the native code implementing
     * a particular virtual method associated with a specified object onto the evaluation stack.
     */
    final val Ldvirtftn = new OpCode()
	opcode(Ldvirtftn, CEE_LDVIRTFTN, "ldvirtftn", 0xFFFFFE07, POP_REF , PUSH_I   , INLINE_METHOD, FLOW_NEXT)

    /**
     * Loads an argument (referenced by a specified index value) onto the stack.
     */
    final val Ldarg = new OpCode()
	opcode(Ldarg, CEE_LDARG , "ldarg"    , 0xFFFFFE09, POP_NONE, PUSH_1   , INLINE_VARIABLE  , FLOW_NEXT)

    /**
     * Load an argument address onto the evaluation stack.
     */
    final val Ldarga = new OpCode()
	opcode(Ldarga, CEE_LDARGA , "ldarga", 0xFFFFFE0A, POP_NONE, PUSH_I, INLINE_VARIABLE  , FLOW_NEXT)

    /**
     * Loads the local variable at a specific index onto the evaluation stack.
     */
    final val Ldloc = new OpCode()
	opcode(Ldloc, CEE_LDLOC, "ldloc", 0xFFFFFE0C, POP_NONE, PUSH_1   , INLINE_VARIABLE  , FLOW_NEXT)

    /**
     *  Loads the address of the local variable at a specific index onto the evaluation stack.
     */
    final val Ldloca = new OpCode()
	opcode(Ldloca, CEE_LDLOCA, "ldloca", 0xFFFFFE0D, POP_NONE, PUSH_I, INLINE_VARIABLE  , FLOW_NEXT)

    /**
     *  Stores the value on top of the evaluation stack in the argument slot at a specified index.
     */
    final val Starg = new OpCode()
	opcode(Starg, CEE_STARG, "starg", 0xFFFFFE0B, POP_1   , PUSH_NONE, INLINE_VARIABLE  , FLOW_NEXT)

    /**
     * Pops the current value from the top of the evaluation stack and stores it in a
     * the local variable list at a specified index.
     */
    final val Stloc = new OpCode()
	opcode(Stloc, CEE_STLOC, "stloc", 0xFFFFFE0E, POP_1   , PUSH_NONE, INLINE_VARIABLE  , FLOW_NEXT)

    /**
     * Allocates a certain number of bytes from the local dynamic memory pool and pushes the
     * address (a transient pointer, type *) of the first allocated Byte onto the evaluation stack.
     */
    final val Localloc = new OpCode()
	opcode(Localloc, CEE_LOCALLOC, "localloc"  , 0xFFFFFE0F, POP_I, PUSH_I, INLINE_NONE, FLOW_NEXT)

    /**
     * Transfers control from the filter clause of an exception back to the
     * Common Language Infrastructure (CLI) exception handler.
     */
    final val Endfilter = new OpCode()
	opcode(Endfilter, CEE_ENDFILTER, "endfilter" , 0xFFFFFE11, POP_I   , PUSH_NONE, INLINE_NONE, FLOW_RETURN)

    /**
     * Indicates that an address currently atop the evaluation stack might not be aligned
     * to the natural size of the immediately following ldind, stind, ldfld, stfld, ldobj,
     * stobj, initblk, or cpblk instruction.
     */
    final val Unaligned = new OpCode()
	opcode(Unaligned, CEE_UNALIGNED, "unaligned.", 0xFFFFFE12, POP_NONE, PUSH_NONE, INLINE_I_S , FLOW_META)

    /**
     * Specifies that an address currently atop the evaluation stack might be volatile,
     * and the results of reading that location cannot be cached or that multiple stores
     * to that location cannot be suppressed.
     */
    final val Volatile = new OpCode()
	opcode(Volatile, CEE_VOLATILE, "volatile." , 0xFFFFFE13, POP_NONE, PUSH_NONE, INLINE_NONE, FLOW_META)

    /**
     * Performs a postfixed method call instruction such that the current method's stack
     * frame is removed before the actual call instruction is executed.
     */
    final val Tailcall = new OpCode()
	opcode(Tailcall, CEE_TAILCALL, "tail."     , 0xFFFFFE14, POP_NONE, PUSH_NONE, INLINE_NONE, FLOW_META)

    /**
     * Initializes all the fields of the object at a specific address to a null reference
     * or a 0 of the appropriate primitive type.
     */
    final val Initobj = new OpCode()
	opcode(Initobj, CEE_INITOBJ , "initobj"   , 0xFFFFFE15, POP_I   , PUSH_NONE, INLINE_TYPE, FLOW_NEXT)

    /**
     * Copies a specified number bytes from a source address to a destination address .
     */
    final val Cpblk = new OpCode()
	opcode(Cpblk, CEE_CPBLK , "cpblk"     , 0xFFFFFE17, POP_I_I_I, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Initializes a specified block of memory at a specific address to a given size
     * and initial value.
     */
    final val Initblk = new OpCode()
	opcode(Initblk, CEE_INITBLK , "initblk"   , 0xFFFFFE18, POP_I_I_I, PUSH_NONE, INLINE_NONE, FLOW_NEXT)

    /**
     * Rethrows the current exception.
     */
    final val Rethrow = new OpCode()
	opcode(Rethrow, CEE_RETHROW , "rethrow", 0xFFFFFE1A, POP_NONE , PUSH_NONE, INLINE_NONE, FLOW_THROW)

    /**
     * Pushes the size, in bytes, of a supplied value type onto the evaluation stack.
     */
    final val Sizeof = new OpCode()
	opcode(Sizeof, CEE_SIZEOF, "sizeof", 0xFFFFFE1C, POP_NONE , PUSH_I   , INLINE_TYPE, FLOW_NEXT)



    //##########################################################################
}
