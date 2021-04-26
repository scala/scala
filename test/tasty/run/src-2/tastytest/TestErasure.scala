package tastytest

import tastytest.{dottyErasure => dotc, scala2Erasure => nsc}

object TestErasure extends Suite("TestErasure") {

  val z = new dotc.Z

  test("erasure of scala 3 from scala 2") {
    z.a_01(anyObj)
    z.a_02(anyObj)
    z.a_02X(anyObj)
    z.a_03(anyObj)
    z.a_04(anyObj)
    z.a_04X(anyObj)
    z.a_05(anyObj)
    z.a_06(anyObj)
    z.a_07(anyObj)
    z.a_08(anyObj)
    z.a_09(anyObj)
    z.a_10(anyObj)
    z.b_11(anyObj)
    z.subb_12(anyObj)
    z.d_13(anyObj)
    z.d_13x(anyObj)
    z.d_14(anyObj)
    z.d_14x(anyObj)
    z.d_15(anyObj)
    z.d_15b(anyObj)
    z.d_16(anyObj)
    z.d_16b(anyObj)
    z.d_17(anyObj)
    z.d_18(anyObj)
    z.d_19(anyObj)
    z.d_19x(anyObj)
    z.d_20(anyObj)
    z.a_21(anyObj)
    z.a_22(anyObj)
    z.z_23(anyObj)
    z.z_24(anyObj)
    z.a_25(anyObj)
    z.a_26(anyObj)
    z.a_27(anyObj) @@ ExpectCastOrNull
    z.a_28(anyObj) @@ ExpectCastOrNull
    z.e_29(anyObj)
    z.e_30(anyObj)
    z.e_31(anyObj)
    z.e_32(anyObj)
    z.e_33(anyObj)
    z.e_34(anyObj)
    z.d_35(anyObj)
    z.d_36(anyObj)
    z.d_37(anyObj)
    z.d_38(anyObj)
    z.b_39(anyObj)
    z.b_40(anyObj)
    z.b_41(anyObj)
    z.b_42(anyObj)
    z.b_43(anyObj)
    z.b_44(anyObj)
    z.b_45(anyObj)
    z.b_46(anyObj)
    z.a_47(anyObj)
    z.a_48(anyObj)
    z.a_49(anyObj)
    z.a_50(anyObj)
    z.a_51(anyObj)
    z.a_52(anyObj)
    z.a_53(anyObj)
    z.a_54(anyObj)
    z.a_55(anyObj)
    z.a_56(anyObj)
    z.a_57(anyObj)
    z.int_58(1)
    z.int_59(1)
    z.int_60(1)
    z.int_61(1)
    z.int_62(1)
    z.int_63(1)
    z.intARRAY_64(anyObj)
    z.intARRAY_65(anyObj)
    z.intARRAY_66(anyObj)
    z.intARRAY_67(anyObj)
    z.intARRAY_68(anyObj)
    z.intARRAY_69(anyObj)
    z.intARRAY_70(anyObj)
    z.intARRAY_71(anyObj)
    // z.intARRAY_71a(anyObj) // illegal union type
    // z.intARRAY_71b(anyObj) // illegal union type
    z.stringARRAY_72(anyObj)
    z.stringARRAY_73(anyObj)
    z.stringARRAY_74(anyObj)
    z.stringARRAY_75(anyObj)
    z.stringARRAY_76(anyObj)
    z.stringARRAY_77(anyObj)
    z.stringARRAY_78(anyObj)
    z.stringARRAY_79(anyObj)
    // z.stringARRAY_79a(anyObj) // illegal union type
    // z.stringARRAY_79b(anyObj) // illegal union type
    z.object_80(anyObj)
    z.object_81(anyObj)
    z.objectARRAY_82(anyObj)
    z.object_83(anyObj)
    z.object_83a(anyObj)
    // z.object_83b(anyObj) // illegal union type
    // z.object_83c(anyObj) // illegal union type
    // z.object_83d(anyObj) // illegal union type
    // z.object_83e(anyObj) // illegal union type
    z.serializableARRAY_84(anyObj)
    z.univARRAY_85(anyObj)
    z.aARRAY_86(anyObj)
    z.aARRAY_87(anyObj)
    z.objectARRAY_88(anyObj)
    z.objectARRAY_89(anyObj)
    z.objectARRAY_90(anyObj)
    z.stringARRAY_91(anyObj)
    z.stringARRAY_92(anyObj)
    z.stringARRAY_93(anyObj)
    z.covARRAY_94(anyObj)
    z.aARRAY_95(anyObj)
    z.aARRAY_96(anyObj)
    z.zARRAY_97(anyObj)
    z.aARRAY_98(anyObj)
    z.stringARRAY_99(anyObj)
    z.aARRAY_100(anyObj)
    z.dARRAY_101(anyObj)
    z.aARRAY_102(anyObj)
    z.aARRAY_103(anyObj)
    z.dARRAY_104(anyObj)
    z.intARRAY_105(anyObj)
    z.vcARRAY_106(anyObj)
    z.listARRAY_107(anyObj)
    z.intARRAY_108(anyObj)
    z.intARRAY_109(anyObj)
    z.a_110(anyObj) @@ ExpectCastOrNull
    z.a_111(anyObj) @@ ExpectCastOrNull
    z.vcARRAY_112(anyObj)
    z.vcARRAY_113(anyObj)
    z.a_114(anyObj) @@ ExpectCastOrNull
    z.a_115(anyObj) @@ ExpectCastOrNull
    z.a_116(anyObj) @@ ExpectCastOrNull
    z.a_117(anyObj) @@ ExpectCastOrNull
    z.a_118(anyObj) @@ ExpectCastOrNull
    z.a_119(anyObj) @@ ExpectCastOrNull
    z.a_120(anyObj) @@ ExpectCastOrNull
    z.object_121(anyObj)
    z.object_122(anyObj)
    z.objectARRAY_123(anyObj)
    z.object_124(anyObj)
    z.objectARRAY_125(anyObj)
    z.covARRAY_126(anyObj)
    z.covARRAY_127(anyObj)
    z.object_128(anyObj)
    z.intARRAYARRAY_129(anyObj)
    z.intARRAYARRAY_130(anyObj)
    z.objectARRAY_130(anyObj)
    z.intARRAY_131(anyObj)
    z.enumerated_132(anyObj)
    z.enumerated_133(anyObj)
    z.enumerated_134(anyObj)
    z.enumeratedARRAY_135(anyObj)
    z.enumeratedARRAY_136(anyObj)
    z.enumeratedARRAY_137(anyObj)
  }

  test("erasure matches name") {
    val methods = classOf[nsc.Z].getDeclaredMethods.toList ++ classOf[dotc.Z].getDeclaredMethods.toList
    methods.foreach { m =>
      m.getName match {
        case s"${prefix}_${suffix}" =>
          val paramClass = m.getParameterTypes()(0).getSimpleName.toLowerCase.replaceAll("""\[\]""", "ARRAY")
          assert(prefix == paramClass, s"Method `$m` erased to `$paramClass` which does not match its prefix `$prefix`")
        case _ =>
      }
    }
  }

}
