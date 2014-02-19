import scala.reflect.runtime.universe._
import scala.language._

object Test {
  def whatis[T: TypeTag](x: T) = typeOf[T]
  def sshow(label: String, xs: Traversable[Any]) {
    println("==== " + label + " ====\n")
    xs.toList.map("" + _).sorted foreach println
    println("\n")
  }

  type R1_0 = (   Any { val y: P }) with (   Any { val y: P })
  type R2_0 = R1_0 { val y: (P) with (P) }
  type R1_1 = (   Any { val y: P }) with (   Any { val y: Q })
  type R2_1 = R1_1 { val y: (P) with (Q) }
  type R1_2 = (   Any { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_2 = R1_2 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_3 = (   Any { val y: P }) with (     A { val y: P })
  type R2_3 = R1_3 { val y: (P) with (P) }
  type R1_4 = (   Any { val y: P }) with (     A { val y: Q })
  type R2_4 = R1_4 { val y: (P) with (Q) }
  type R1_5 = (   Any { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_5 = R1_5 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_6 = (   Any { val y: P }) with (     B { val y: P })
  type R2_6 = R1_6 { val y: (P) with (P) }
  type R1_7 = (   Any { val y: P }) with (     B { val y: Q })
  type R2_7 = R1_7 { val y: (P) with (Q) }
  type R1_8 = (   Any { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_8 = R1_8 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_9 = (   Any { val y: P }) with (     C { val y: P })
  type R2_9 = R1_9 { val y: (P) with (P) }
  type R1_10 = (   Any { val y: P }) with (     C { val y: Q })
  type R2_10 = R1_10 { val y: (P) with (Q) }
  type R1_11 = (   Any { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_11 = R1_11 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_12 = (   Any { val y: Q }) with (   Any { val y: P })
  type R2_12 = R1_12 { val y: (Q) with (P) }
  type R1_13 = (   Any { val y: Q }) with (   Any { val y: Q })
  type R2_13 = R1_13 { val y: (Q) with (Q) }
  type R1_14 = (   Any { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_14 = R1_14 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_15 = (   Any { val y: Q }) with (     A { val y: P })
  type R2_15 = R1_15 { val y: (Q) with (P) }
  type R1_16 = (   Any { val y: Q }) with (     A { val y: Q })
  type R2_16 = R1_16 { val y: (Q) with (Q) }
  type R1_17 = (   Any { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_17 = R1_17 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_18 = (   Any { val y: Q }) with (     B { val y: P })
  type R2_18 = R1_18 { val y: (Q) with (P) }
  type R1_19 = (   Any { val y: Q }) with (     B { val y: Q })
  type R2_19 = R1_19 { val y: (Q) with (Q) }
  type R1_20 = (   Any { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_20 = R1_20 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_21 = (   Any { val y: Q }) with (     C { val y: P })
  type R2_21 = R1_21 { val y: (Q) with (P) }
  type R1_22 = (   Any { val y: Q }) with (     C { val y: Q })
  type R2_22 = R1_22 { val y: (Q) with (Q) }
  type R1_23 = (   Any { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_23 = R1_23 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_24 = (   Any { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })
  type R2_24 = R1_24 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_25 = (   Any { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })
  type R2_25 = R1_25 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_26 = (   Any { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_26 = R1_26 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_27 = (   Any { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })
  type R2_27 = R1_27 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_28 = (   Any { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })
  type R2_28 = R1_28 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_29 = (   Any { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_29 = R1_29 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_30 = (   Any { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })
  type R2_30 = R1_30 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_31 = (   Any { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })
  type R2_31 = R1_31 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_32 = (   Any { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_32 = R1_32 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_33 = (   Any { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })
  type R2_33 = R1_33 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_34 = (   Any { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })
  type R2_34 = R1_34 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_35 = (   Any { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_35 = R1_35 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_36 = (     A { val y: P }) with (   Any { val y: P })
  type R2_36 = R1_36 { val y: (P) with (P) }
  type R1_37 = (     A { val y: P }) with (   Any { val y: Q })
  type R2_37 = R1_37 { val y: (P) with (Q) }
  type R1_38 = (     A { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_38 = R1_38 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_39 = (     A { val y: P }) with (     A { val y: P })
  type R2_39 = R1_39 { val y: (P) with (P) }
  type R1_40 = (     A { val y: P }) with (     A { val y: Q })
  type R2_40 = R1_40 { val y: (P) with (Q) }
  type R1_41 = (     A { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_41 = R1_41 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_42 = (     A { val y: P }) with (     B { val y: P })
  type R2_42 = R1_42 { val y: (P) with (P) }
  type R1_43 = (     A { val y: P }) with (     B { val y: Q })
  type R2_43 = R1_43 { val y: (P) with (Q) }
  type R1_44 = (     A { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_44 = R1_44 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_45 = (     A { val y: P }) with (     C { val y: P })
  type R2_45 = R1_45 { val y: (P) with (P) }
  type R1_46 = (     A { val y: P }) with (     C { val y: Q })
  type R2_46 = R1_46 { val y: (P) with (Q) }
  type R1_47 = (     A { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_47 = R1_47 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_48 = (     A { val y: Q }) with (   Any { val y: P })
  type R2_48 = R1_48 { val y: (Q) with (P) }
  type R1_49 = (     A { val y: Q }) with (   Any { val y: Q })
  type R2_49 = R1_49 { val y: (Q) with (Q) }
  type R1_50 = (     A { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_50 = R1_50 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_51 = (     A { val y: Q }) with (     A { val y: P })
  type R2_51 = R1_51 { val y: (Q) with (P) }
  type R1_52 = (     A { val y: Q }) with (     A { val y: Q })
  type R2_52 = R1_52 { val y: (Q) with (Q) }
  type R1_53 = (     A { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_53 = R1_53 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_54 = (     A { val y: Q }) with (     B { val y: P })
  type R2_54 = R1_54 { val y: (Q) with (P) }
  type R1_55 = (     A { val y: Q }) with (     B { val y: Q })
  type R2_55 = R1_55 { val y: (Q) with (Q) }
  type R1_56 = (     A { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_56 = R1_56 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_57 = (     A { val y: Q }) with (     C { val y: P })
  type R2_57 = R1_57 { val y: (Q) with (P) }
  type R1_58 = (     A { val y: Q }) with (     C { val y: Q })
  type R2_58 = R1_58 { val y: (Q) with (Q) }
  type R1_59 = (     A { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_59 = R1_59 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_60 = (     A { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })
  type R2_60 = R1_60 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_61 = (     A { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })
  type R2_61 = R1_61 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_62 = (     A { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_62 = R1_62 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_63 = (     A { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })
  type R2_63 = R1_63 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_64 = (     A { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })
  type R2_64 = R1_64 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_65 = (     A { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_65 = R1_65 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_66 = (     A { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })
  type R2_66 = R1_66 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_67 = (     A { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })
  type R2_67 = R1_67 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_68 = (     A { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_68 = R1_68 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_69 = (     A { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })
  type R2_69 = R1_69 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_70 = (     A { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })
  type R2_70 = R1_70 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_71 = (     A { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_71 = R1_71 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_72 = (     B { val y: P }) with (   Any { val y: P })
  type R2_72 = R1_72 { val y: (P) with (P) }
  type R1_73 = (     B { val y: P }) with (   Any { val y: Q })
  type R2_73 = R1_73 { val y: (P) with (Q) }
  type R1_74 = (     B { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_74 = R1_74 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_75 = (     B { val y: P }) with (     A { val y: P })
  type R2_75 = R1_75 { val y: (P) with (P) }
  type R1_76 = (     B { val y: P }) with (     A { val y: Q })
  type R2_76 = R1_76 { val y: (P) with (Q) }
  type R1_77 = (     B { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_77 = R1_77 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_78 = (     B { val y: P }) with (     B { val y: P })
  type R2_78 = R1_78 { val y: (P) with (P) }
  type R1_79 = (     B { val y: P }) with (     B { val y: Q })
  type R2_79 = R1_79 { val y: (P) with (Q) }
  type R1_80 = (     B { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_80 = R1_80 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_81 = (     B { val y: P }) with (     C { val y: P })
  type R2_81 = R1_81 { val y: (P) with (P) }
  type R1_82 = (     B { val y: P }) with (     C { val y: Q })
  type R2_82 = R1_82 { val y: (P) with (Q) }
  type R1_83 = (     B { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_83 = R1_83 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_84 = (     B { val y: Q }) with (   Any { val y: P })
  type R2_84 = R1_84 { val y: (Q) with (P) }
  type R1_85 = (     B { val y: Q }) with (   Any { val y: Q })
  type R2_85 = R1_85 { val y: (Q) with (Q) }
  type R1_86 = (     B { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_86 = R1_86 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_87 = (     B { val y: Q }) with (     A { val y: P })
  type R2_87 = R1_87 { val y: (Q) with (P) }
  type R1_88 = (     B { val y: Q }) with (     A { val y: Q })
  type R2_88 = R1_88 { val y: (Q) with (Q) }
  type R1_89 = (     B { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_89 = R1_89 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_90 = (     B { val y: Q }) with (     B { val y: P })
  type R2_90 = R1_90 { val y: (Q) with (P) }
  type R1_91 = (     B { val y: Q }) with (     B { val y: Q })
  type R2_91 = R1_91 { val y: (Q) with (Q) }
  type R1_92 = (     B { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_92 = R1_92 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_93 = (     B { val y: Q }) with (     C { val y: P })
  type R2_93 = R1_93 { val y: (Q) with (P) }
  type R1_94 = (     B { val y: Q }) with (     C { val y: Q })
  type R2_94 = R1_94 { val y: (Q) with (Q) }
  type R1_95 = (     B { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_95 = R1_95 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_96 = (     B { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })
  type R2_96 = R1_96 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_97 = (     B { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })
  type R2_97 = R1_97 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_98 = (     B { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_98 = R1_98 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_99 = (     B { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })
  type R2_99 = R1_99 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_100 = (     B { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })
  type R2_100 = R1_100 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_101 = (     B { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_101 = R1_101 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_102 = (     B { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })
  type R2_102 = R1_102 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_103 = (     B { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })
  type R2_103 = R1_103 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_104 = (     B { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_104 = R1_104 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_105 = (     B { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })
  type R2_105 = R1_105 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_106 = (     B { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })
  type R2_106 = R1_106 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_107 = (     B { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_107 = R1_107 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_108 = (     C { val y: P }) with (   Any { val y: P })
  type R2_108 = R1_108 { val y: (P) with (P) }
  type R1_109 = (     C { val y: P }) with (   Any { val y: Q })
  type R2_109 = R1_109 { val y: (P) with (Q) }
  type R1_110 = (     C { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_110 = R1_110 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_111 = (     C { val y: P }) with (     A { val y: P })
  type R2_111 = R1_111 { val y: (P) with (P) }
  type R1_112 = (     C { val y: P }) with (     A { val y: Q })
  type R2_112 = R1_112 { val y: (P) with (Q) }
  type R1_113 = (     C { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_113 = R1_113 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_114 = (     C { val y: P }) with (     B { val y: P })
  type R2_114 = R1_114 { val y: (P) with (P) }
  type R1_115 = (     C { val y: P }) with (     B { val y: Q })
  type R2_115 = R1_115 { val y: (P) with (Q) }
  type R1_116 = (     C { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_116 = R1_116 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_117 = (     C { val y: P }) with (     C { val y: P })
  type R2_117 = R1_117 { val y: (P) with (P) }
  type R1_118 = (     C { val y: P }) with (     C { val y: Q })
  type R2_118 = R1_118 { val y: (P) with (Q) }
  type R1_119 = (     C { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_119 = R1_119 { val y: (P) with (R forSome { type R <: P with Q }) }
  type R1_120 = (     C { val y: Q }) with (   Any { val y: P })
  type R2_120 = R1_120 { val y: (Q) with (P) }
  type R1_121 = (     C { val y: Q }) with (   Any { val y: Q })
  type R2_121 = R1_121 { val y: (Q) with (Q) }
  type R1_122 = (     C { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_122 = R1_122 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_123 = (     C { val y: Q }) with (     A { val y: P })
  type R2_123 = R1_123 { val y: (Q) with (P) }
  type R1_124 = (     C { val y: Q }) with (     A { val y: Q })
  type R2_124 = R1_124 { val y: (Q) with (Q) }
  type R1_125 = (     C { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_125 = R1_125 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_126 = (     C { val y: Q }) with (     B { val y: P })
  type R2_126 = R1_126 { val y: (Q) with (P) }
  type R1_127 = (     C { val y: Q }) with (     B { val y: Q })
  type R2_127 = R1_127 { val y: (Q) with (Q) }
  type R1_128 = (     C { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_128 = R1_128 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_129 = (     C { val y: Q }) with (     C { val y: P })
  type R2_129 = R1_129 { val y: (Q) with (P) }
  type R1_130 = (     C { val y: Q }) with (     C { val y: Q })
  type R2_130 = R1_130 { val y: (Q) with (Q) }
  type R1_131 = (     C { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_131 = R1_131 { val y: (Q) with (R forSome { type R <: P with Q }) }
  type R1_132 = (     C { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })
  type R2_132 = R1_132 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_133 = (     C { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })
  type R2_133 = R1_133 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_134 = (     C { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })
  type R2_134 = R1_134 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_135 = (     C { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })
  type R2_135 = R1_135 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_136 = (     C { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })
  type R2_136 = R1_136 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_137 = (     C { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })
  type R2_137 = R1_137 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_138 = (     C { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })
  type R2_138 = R1_138 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_139 = (     C { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })
  type R2_139 = R1_139 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_140 = (     C { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })
  type R2_140 = R1_140 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  type R1_141 = (     C { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })
  type R2_141 = R1_141 { val y: (R forSome { type R <: P with Q }) with (P) }
  type R1_142 = (     C { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })
  type R2_142 = R1_142 { val y: (R forSome { type R <: P with Q }) with (Q) }
  type R1_143 = (     C { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })
  type R2_143 = R1_143 { val y: (R forSome { type R <: P with Q }) with (R forSome { type R <: P with Q }) }
  def f0 = { val x = ((new ABC): (   Any { val y: P }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f1 = { val x = ((new ABC): (   Any { val y: P }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f2 = { val x = ((new ABC): (   Any { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f3 = { val x = ((new ABC): (   Any { val y: P }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f4 = { val x = ((new ABC): (   Any { val y: P }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f5 = { val x = ((new ABC): (   Any { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f6 = { val x = ((new ABC): (   Any { val y: P }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f7 = { val x = ((new ABC): (   Any { val y: P }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f8 = { val x = ((new ABC): (   Any { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f9 = { val x = ((new ABC): (   Any { val y: P }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f10 = { val x = ((new ABC): (   Any { val y: P }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f11 = { val x = ((new ABC): (   Any { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f12 = { val x = ((new ABC): (   Any { val y: Q }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f13 = { val x = ((new ABC): (   Any { val y: Q }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f14 = { val x = ((new ABC): (   Any { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f15 = { val x = ((new ABC): (   Any { val y: Q }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f16 = { val x = ((new ABC): (   Any { val y: Q }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f17 = { val x = ((new ABC): (   Any { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f18 = { val x = ((new ABC): (   Any { val y: Q }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f19 = { val x = ((new ABC): (   Any { val y: Q }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f20 = { val x = ((new ABC): (   Any { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f21 = { val x = ((new ABC): (   Any { val y: Q }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f22 = { val x = ((new ABC): (   Any { val y: Q }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f23 = { val x = ((new ABC): (   Any { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f24 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f25 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f26 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f27 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f28 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f29 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f30 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f31 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f32 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f33 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f34 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f35 = { val x = ((new ABC): (   Any { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f36 = { val x = ((new ABC): (     A { val y: P }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f37 = { val x = ((new ABC): (     A { val y: P }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f38 = { val x = ((new ABC): (     A { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f39 = { val x = ((new ABC): (     A { val y: P }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f40 = { val x = ((new ABC): (     A { val y: P }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f41 = { val x = ((new ABC): (     A { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f42 = { val x = ((new ABC): (     A { val y: P }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f43 = { val x = ((new ABC): (     A { val y: P }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f44 = { val x = ((new ABC): (     A { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f45 = { val x = ((new ABC): (     A { val y: P }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f46 = { val x = ((new ABC): (     A { val y: P }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f47 = { val x = ((new ABC): (     A { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f48 = { val x = ((new ABC): (     A { val y: Q }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f49 = { val x = ((new ABC): (     A { val y: Q }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f50 = { val x = ((new ABC): (     A { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f51 = { val x = ((new ABC): (     A { val y: Q }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f52 = { val x = ((new ABC): (     A { val y: Q }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f53 = { val x = ((new ABC): (     A { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f54 = { val x = ((new ABC): (     A { val y: Q }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f55 = { val x = ((new ABC): (     A { val y: Q }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f56 = { val x = ((new ABC): (     A { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f57 = { val x = ((new ABC): (     A { val y: Q }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f58 = { val x = ((new ABC): (     A { val y: Q }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f59 = { val x = ((new ABC): (     A { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f60 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f61 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f62 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f63 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f64 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f65 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f66 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f67 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f68 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f69 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f70 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f71 = { val x = ((new ABC): (     A { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f72 = { val x = ((new ABC): (     B { val y: P }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f73 = { val x = ((new ABC): (     B { val y: P }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f74 = { val x = ((new ABC): (     B { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f75 = { val x = ((new ABC): (     B { val y: P }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f76 = { val x = ((new ABC): (     B { val y: P }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f77 = { val x = ((new ABC): (     B { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f78 = { val x = ((new ABC): (     B { val y: P }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f79 = { val x = ((new ABC): (     B { val y: P }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f80 = { val x = ((new ABC): (     B { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f81 = { val x = ((new ABC): (     B { val y: P }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f82 = { val x = ((new ABC): (     B { val y: P }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f83 = { val x = ((new ABC): (     B { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f84 = { val x = ((new ABC): (     B { val y: Q }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f85 = { val x = ((new ABC): (     B { val y: Q }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f86 = { val x = ((new ABC): (     B { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f87 = { val x = ((new ABC): (     B { val y: Q }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f88 = { val x = ((new ABC): (     B { val y: Q }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f89 = { val x = ((new ABC): (     B { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f90 = { val x = ((new ABC): (     B { val y: Q }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f91 = { val x = ((new ABC): (     B { val y: Q }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f92 = { val x = ((new ABC): (     B { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f93 = { val x = ((new ABC): (     B { val y: Q }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f94 = { val x = ((new ABC): (     B { val y: Q }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f95 = { val x = ((new ABC): (     B { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f96 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f97 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f98 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f99 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f100 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f101 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f102 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f103 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f104 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f105 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f106 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f107 = { val x = ((new ABC): (     B { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f108 = { val x = ((new ABC): (     C { val y: P }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f109 = { val x = ((new ABC): (     C { val y: P }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f110 = { val x = ((new ABC): (     C { val y: P }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f111 = { val x = ((new ABC): (     C { val y: P }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f112 = { val x = ((new ABC): (     C { val y: P }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f113 = { val x = ((new ABC): (     C { val y: P }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f114 = { val x = ((new ABC): (     C { val y: P }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f115 = { val x = ((new ABC): (     C { val y: P }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f116 = { val x = ((new ABC): (     C { val y: P }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f117 = { val x = ((new ABC): (     C { val y: P }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f118 = { val x = ((new ABC): (     C { val y: P }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f119 = { val x = ((new ABC): (     C { val y: P }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f120 = { val x = ((new ABC): (     C { val y: Q }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f121 = { val x = ((new ABC): (     C { val y: Q }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f122 = { val x = ((new ABC): (     C { val y: Q }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f123 = { val x = ((new ABC): (     C { val y: Q }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f124 = { val x = ((new ABC): (     C { val y: Q }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f125 = { val x = ((new ABC): (     C { val y: Q }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f126 = { val x = ((new ABC): (     C { val y: Q }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f127 = { val x = ((new ABC): (     C { val y: Q }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f128 = { val x = ((new ABC): (     C { val y: Q }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f129 = { val x = ((new ABC): (     C { val y: Q }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f130 = { val x = ((new ABC): (     C { val y: Q }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f131 = { val x = ((new ABC): (     C { val y: Q }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f132 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (   Any { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f133 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (   Any { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f134 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (   Any { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f135 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     A { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f136 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     A { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f137 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     A { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f138 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     B { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f139 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     B { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f140 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     B { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def f141 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     C { val y: P })) ; x.y.reflected -> whatis(x).toString }
  def f142 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     C { val y: Q })) ; x.y.reflected -> whatis(x).toString }
  def f143 = { val x = ((new ABC): (     C { val y: R forSome { type R <: P with Q } }) with (     C { val y: R forSome { type R <: P with Q } })) ; x.y.reflected -> whatis(x).toString }
  def g0(x: R1_0) = x.y
  def g1(x: R1_1) = x.y
  def g2(x: R1_2) = x.y
  def g3(x: R1_3) = x.y
  def g4(x: R1_4) = x.y
  def g5(x: R1_5) = x.y
  def g6(x: R1_6) = x.y
  def g7(x: R1_7) = x.y
  def g8(x: R1_8) = x.y
  def g9(x: R1_9) = x.y
  def g10(x: R1_10) = x.y
  def g11(x: R1_11) = x.y
  def g12(x: R1_12) = x.y
  def g13(x: R1_13) = x.y
  def g14(x: R1_14) = x.y
  def g15(x: R1_15) = x.y
  def g16(x: R1_16) = x.y
  def g17(x: R1_17) = x.y
  def g18(x: R1_18) = x.y
  def g19(x: R1_19) = x.y
  def g20(x: R1_20) = x.y
  def g21(x: R1_21) = x.y
  def g22(x: R1_22) = x.y
  def g23(x: R1_23) = x.y
  def g24(x: R1_24) = x.y
  def g25(x: R1_25) = x.y
  def g26(x: R1_26) = x.y
  def g27(x: R1_27) = x.y
  def g28(x: R1_28) = x.y
  def g29(x: R1_29) = x.y
  def g30(x: R1_30) = x.y
  def g31(x: R1_31) = x.y
  def g32(x: R1_32) = x.y
  def g33(x: R1_33) = x.y
  def g34(x: R1_34) = x.y
  def g35(x: R1_35) = x.y
  def g36(x: R1_36) = x.y
  def g37(x: R1_37) = x.y
  def g38(x: R1_38) = x.y
  def g39(x: R1_39) = x.y
  def g40(x: R1_40) = x.y
  def g41(x: R1_41) = x.y
  def g42(x: R1_42) = x.y
  def g43(x: R1_43) = x.y
  def g44(x: R1_44) = x.y
  def g45(x: R1_45) = x.y
  def g46(x: R1_46) = x.y
  def g47(x: R1_47) = x.y
  def g48(x: R1_48) = x.y
  def g49(x: R1_49) = x.y
  def g50(x: R1_50) = x.y
  def g51(x: R1_51) = x.y
  def g52(x: R1_52) = x.y
  def g53(x: R1_53) = x.y
  def g54(x: R1_54) = x.y
  def g55(x: R1_55) = x.y
  def g56(x: R1_56) = x.y
  def g57(x: R1_57) = x.y
  def g58(x: R1_58) = x.y
  def g59(x: R1_59) = x.y
  def g60(x: R1_60) = x.y
  def g61(x: R1_61) = x.y
  def g62(x: R1_62) = x.y
  def g63(x: R1_63) = x.y
  def g64(x: R1_64) = x.y
  def g65(x: R1_65) = x.y
  def g66(x: R1_66) = x.y
  def g67(x: R1_67) = x.y
  def g68(x: R1_68) = x.y
  def g69(x: R1_69) = x.y
  def g70(x: R1_70) = x.y
  def g71(x: R1_71) = x.y
  def g72(x: R1_72) = x.y
  def g73(x: R1_73) = x.y
  def g74(x: R1_74) = x.y
  def g75(x: R1_75) = x.y
  def g76(x: R1_76) = x.y
  def g77(x: R1_77) = x.y
  def g78(x: R1_78) = x.y
  def g79(x: R1_79) = x.y
  def g80(x: R1_80) = x.y
  def g81(x: R1_81) = x.y
  def g82(x: R1_82) = x.y
  def g83(x: R1_83) = x.y
  def g84(x: R1_84) = x.y
  def g85(x: R1_85) = x.y
  def g86(x: R1_86) = x.y
  def g87(x: R1_87) = x.y
  def g88(x: R1_88) = x.y
  def g89(x: R1_89) = x.y
  def g90(x: R1_90) = x.y
  def g91(x: R1_91) = x.y
  def g92(x: R1_92) = x.y
  def g93(x: R1_93) = x.y
  def g94(x: R1_94) = x.y
  def g95(x: R1_95) = x.y
  def g96(x: R1_96) = x.y
  def g97(x: R1_97) = x.y
  def g98(x: R1_98) = x.y
  def g99(x: R1_99) = x.y
  def g100(x: R1_100) = x.y
  def g101(x: R1_101) = x.y
  def g102(x: R1_102) = x.y
  def g103(x: R1_103) = x.y
  def g104(x: R1_104) = x.y
  def g105(x: R1_105) = x.y
  def g106(x: R1_106) = x.y
  def g107(x: R1_107) = x.y
  def g108(x: R1_108) = x.y
  def g109(x: R1_109) = x.y
  def g110(x: R1_110) = x.y
  def g111(x: R1_111) = x.y
  def g112(x: R1_112) = x.y
  def g113(x: R1_113) = x.y
  def g114(x: R1_114) = x.y
  def g115(x: R1_115) = x.y
  def g116(x: R1_116) = x.y
  def g117(x: R1_117) = x.y
  def g118(x: R1_118) = x.y
  def g119(x: R1_119) = x.y
  def g120(x: R1_120) = x.y
  def g121(x: R1_121) = x.y
  def g122(x: R1_122) = x.y
  def g123(x: R1_123) = x.y
  def g124(x: R1_124) = x.y
  def g125(x: R1_125) = x.y
  def g126(x: R1_126) = x.y
  def g127(x: R1_127) = x.y
  def g128(x: R1_128) = x.y
  def g129(x: R1_129) = x.y
  def g130(x: R1_130) = x.y
  def g131(x: R1_131) = x.y
  def g132(x: R1_132) = x.y
  def g133(x: R1_133) = x.y
  def g134(x: R1_134) = x.y
  def g135(x: R1_135) = x.y
  def g136(x: R1_136) = x.y
  def g137(x: R1_137) = x.y
  def g138(x: R1_138) = x.y
  def g139(x: R1_139) = x.y
  def g140(x: R1_140) = x.y
  def g141(x: R1_141) = x.y
  def g142(x: R1_142) = x.y
  def g143(x: R1_143) = x.y
  def h0(x: R2_0) = x.y
  def h1(x: R2_1) = x.y
  def h2(x: R2_2) = x.y
  def h3(x: R2_3) = x.y
  def h4(x: R2_4) = x.y
  def h5(x: R2_5) = x.y
  def h6(x: R2_6) = x.y
  def h7(x: R2_7) = x.y
  def h8(x: R2_8) = x.y
  def h9(x: R2_9) = x.y
  def h10(x: R2_10) = x.y
  def h11(x: R2_11) = x.y
  def h12(x: R2_12) = x.y
  def h13(x: R2_13) = x.y
  def h14(x: R2_14) = x.y
  def h15(x: R2_15) = x.y
  def h16(x: R2_16) = x.y
  def h17(x: R2_17) = x.y
  def h18(x: R2_18) = x.y
  def h19(x: R2_19) = x.y
  def h20(x: R2_20) = x.y
  def h21(x: R2_21) = x.y
  def h22(x: R2_22) = x.y
  def h23(x: R2_23) = x.y
  def h24(x: R2_24) = x.y
  def h25(x: R2_25) = x.y
  def h26(x: R2_26) = x.y
  def h27(x: R2_27) = x.y
  def h28(x: R2_28) = x.y
  def h29(x: R2_29) = x.y
  def h30(x: R2_30) = x.y
  def h31(x: R2_31) = x.y
  def h32(x: R2_32) = x.y
  def h33(x: R2_33) = x.y
  def h34(x: R2_34) = x.y
  def h35(x: R2_35) = x.y
  def h36(x: R2_36) = x.y
  def h37(x: R2_37) = x.y
  def h38(x: R2_38) = x.y
  def h39(x: R2_39) = x.y
  def h40(x: R2_40) = x.y
  def h41(x: R2_41) = x.y
  def h42(x: R2_42) = x.y
  def h43(x: R2_43) = x.y
  def h44(x: R2_44) = x.y
  def h45(x: R2_45) = x.y
  def h46(x: R2_46) = x.y
  def h47(x: R2_47) = x.y
  def h48(x: R2_48) = x.y
  def h49(x: R2_49) = x.y
  def h50(x: R2_50) = x.y
  def h51(x: R2_51) = x.y
  def h52(x: R2_52) = x.y
  def h53(x: R2_53) = x.y
  def h54(x: R2_54) = x.y
  def h55(x: R2_55) = x.y
  def h56(x: R2_56) = x.y
  def h57(x: R2_57) = x.y
  def h58(x: R2_58) = x.y
  def h59(x: R2_59) = x.y
  def h60(x: R2_60) = x.y
  def h61(x: R2_61) = x.y
  def h62(x: R2_62) = x.y
  def h63(x: R2_63) = x.y
  def h64(x: R2_64) = x.y
  def h65(x: R2_65) = x.y
  def h66(x: R2_66) = x.y
  def h67(x: R2_67) = x.y
  def h68(x: R2_68) = x.y
  def h69(x: R2_69) = x.y
  def h70(x: R2_70) = x.y
  def h71(x: R2_71) = x.y
  def h72(x: R2_72) = x.y
  def h73(x: R2_73) = x.y
  def h74(x: R2_74) = x.y
  def h75(x: R2_75) = x.y
  def h76(x: R2_76) = x.y
  def h77(x: R2_77) = x.y
  def h78(x: R2_78) = x.y
  def h79(x: R2_79) = x.y
  def h80(x: R2_80) = x.y
  def h81(x: R2_81) = x.y
  def h82(x: R2_82) = x.y
  def h83(x: R2_83) = x.y
  def h84(x: R2_84) = x.y
  def h85(x: R2_85) = x.y
  def h86(x: R2_86) = x.y
  def h87(x: R2_87) = x.y
  def h88(x: R2_88) = x.y
  def h89(x: R2_89) = x.y
  def h90(x: R2_90) = x.y
  def h91(x: R2_91) = x.y
  def h92(x: R2_92) = x.y
  def h93(x: R2_93) = x.y
  def h94(x: R2_94) = x.y
  def h95(x: R2_95) = x.y
  def h96(x: R2_96) = x.y
  def h97(x: R2_97) = x.y
  def h98(x: R2_98) = x.y
  def h99(x: R2_99) = x.y
  def h100(x: R2_100) = x.y
  def h101(x: R2_101) = x.y
  def h102(x: R2_102) = x.y
  def h103(x: R2_103) = x.y
  def h104(x: R2_104) = x.y
  def h105(x: R2_105) = x.y
  def h106(x: R2_106) = x.y
  def h107(x: R2_107) = x.y
  def h108(x: R2_108) = x.y
  def h109(x: R2_109) = x.y
  def h110(x: R2_110) = x.y
  def h111(x: R2_111) = x.y
  def h112(x: R2_112) = x.y
  def h113(x: R2_113) = x.y
  def h114(x: R2_114) = x.y
  def h115(x: R2_115) = x.y
  def h116(x: R2_116) = x.y
  def h117(x: R2_117) = x.y
  def h118(x: R2_118) = x.y
  def h119(x: R2_119) = x.y
  def h120(x: R2_120) = x.y
  def h121(x: R2_121) = x.y
  def h122(x: R2_122) = x.y
  def h123(x: R2_123) = x.y
  def h124(x: R2_124) = x.y
  def h125(x: R2_125) = x.y
  def h126(x: R2_126) = x.y
  def h127(x: R2_127) = x.y
  def h128(x: R2_128) = x.y
  def h129(x: R2_129) = x.y
  def h130(x: R2_130) = x.y
  def h131(x: R2_131) = x.y
  def h132(x: R2_132) = x.y
  def h133(x: R2_133) = x.y
  def h134(x: R2_134) = x.y
  def h135(x: R2_135) = x.y
  def h136(x: R2_136) = x.y
  def h137(x: R2_137) = x.y
  def h138(x: R2_138) = x.y
  def h139(x: R2_139) = x.y
  def h140(x: R2_140) = x.y
  def h141(x: R2_141) = x.y
  def h142(x: R2_142) = x.y
  def h143(x: R2_143) = x.y
  lazy val fcalls = List(
    f0,
    f1,
    f2,
    f3,
    f4,
    f5,
    f6,
    f7,
    f8,
    f9,
    f10,
    f11,
    f12,
    f13,
    f14,
    f15,
    f16,
    f17,
    f18,
    f19,
    f20,
    f21,
    f22,
    f23,
    f24,
    f25,
    f26,
    f27,
    f28,
    f29,
    f30,
    f31,
    f32,
    f33,
    f34,
    f35,
    f36,
    f37,
    f38,
    f39,
    f40,
    f41,
    f42,
    f43,
    f44,
    f45,
    f46,
    f47,
    f48,
    f49,
    f50,
    f51,
    f52,
    f53,
    f54,
    f55,
    f56,
    f57,
    f58,
    f59,
    f60,
    f61,
    f62,
    f63,
    f64,
    f65,
    f66,
    f67,
    f68,
    f69,
    f70,
    f71,
    f72,
    f73,
    f74,
    f75,
    f76,
    f77,
    f78,
    f79,
    f80,
    f81,
    f82,
    f83,
    f84,
    f85,
    f86,
    f87,
    f88,
    f89,
    f90,
    f91,
    f92,
    f93,
    f94,
    f95,
    f96,
    f97,
    f98,
    f99,
    f100,
    f101,
    f102,
    f103,
    f104,
    f105,
    f106,
    f107,
    f108,
    f109,
    f110,
    f111,
    f112,
    f113,
    f114,
    f115,
    f116,
    f117,
    f118,
    f119,
    f120,
    f121,
    f122,
    f123,
    f124,
    f125,
    f126,
    f127,
    f128,
    f129,
    f130,
    f131,
    f132,
    f133,
    f134,
    f135,
    f136,
    f137,
    f138,
    f139,
    f140,
    f141,
    f142,
    f143
  )

  def main(args: Array[String]) {
    sshow("Direct Calls", fcalls collect { case (false, n) => n })
    sshow("Reflective Calls", fcalls collect { case (true, n) => n })
    // For a good time try printing this - have to fix bugs in
    // reflection before that's going to be a good idea
    // println(typeOf[Test.type].typeSymbol.asClass.info)
  }
}
