import scala.language.existentials

object A extends A[Any, Object, Int] { }
abstract class A[T1, T2 <: Object, T3 <: Int] {
  type T4
  type T5 <: AnyRef
  type T6 <: Boolean

  def f001: Array[Any with Any] = null
  def f002: Array[Any with AnyVal] = null
  def f003: Array[Any with Int] = null
  def f004: Array[Any with String] = null
  def f005: Array[Any with Null] = null
  def f006: Array[Any with Nothing] = null
  def f007: Array[Any with T3] = null
  def f008: Array[Any with T6] = null
  def f009: Array[Any with Array[Any]] = null
  def f010: Array[Any with Array[Int]] = null
  def f011: Array[AnyVal with Any] = null
  def f012: Array[AnyVal with AnyVal] = null
  def f013: Array[AnyVal with Int] = null
  def f014: Array[AnyVal with String] = null
  def f015: Array[AnyVal with Null] = null
  def f016: Array[AnyVal with Nothing] = null
  def f017: Array[AnyVal with T3] = null
  def f018: Array[AnyVal with T6] = null
  def f019: Array[AnyVal with Array[Any]] = null
  def f020: Array[AnyVal with Array[Int]] = null
  def f021: Array[Int with Any] = null
  def f022: Array[Int with AnyVal] = null
  def f023: Array[Int with Int] = null
  def f024: Array[Int with String] = null
  def f025: Array[Int with Null] = null
  def f026: Array[Int with Nothing] = null
  def f027: Array[Int with T3] = null
  def f028: Array[Int with T6] = null
  def f029: Array[Int with Array[Any]] = null
  def f030: Array[Int with Array[Int]] = null
  def f031: Array[String with Any] = null
  def f032: Array[String with AnyVal] = null
  def f033: Array[String with Int] = null
  def f034: Array[String with String] = null
  def f035: Array[String with Null] = null
  def f036: Array[String with Nothing] = null
  def f037: Array[String with T3] = null
  def f038: Array[String with T6] = null
  def f039: Array[String with Array[Any]] = null
  def f040: Array[String with Array[Int]] = null
  def f041: Array[Null with Any] = null
  def f042: Array[Null with AnyVal] = null
  def f043: Array[Null with Int] = null
  def f044: Array[Null with String] = null
  def f045: Array[Null with Null] = null
  def f046: Array[Null with Nothing] = null
  def f047: Array[Null with T3] = null
  def f048: Array[Null with T6] = null
  def f049: Array[Null with Array[Any]] = null
  def f050: Array[Null with Array[Int]] = null
  def f051: Array[Nothing with Any] = null
  def f052: Array[Nothing with AnyVal] = null
  def f053: Array[Nothing with Int] = null
  def f054: Array[Nothing with String] = null
  def f055: Array[Nothing with Null] = null
  def f056: Array[Nothing with Nothing] = null
  def f057: Array[Nothing with T3] = null
  def f058: Array[Nothing with T6] = null
  def f059: Array[Nothing with Array[Any]] = null
  def f060: Array[Nothing with Array[Int]] = null
  def f061: Array[T3 with Any] = null
  def f062: Array[T3 with AnyVal] = null
  def f063: Array[T3 with Int] = null
  def f064: Array[T3 with String] = null
  def f065: Array[T3 with Null] = null
  def f066: Array[T3 with Nothing] = null
  def f067: Array[T3 with T3] = null
  def f068: Array[T3 with T6] = null
  def f069: Array[T3 with Array[Any]] = null
  def f070: Array[T3 with Array[Int]] = null
  def f071: Array[T6 with Any] = null
  def f072: Array[T6 with AnyVal] = null
  def f073: Array[T6 with Int] = null
  def f074: Array[T6 with String] = null
  def f075: Array[T6 with Null] = null
  def f076: Array[T6 with Nothing] = null
  def f077: Array[T6 with T3] = null
  def f078: Array[T6 with T6] = null
  def f079: Array[T6 with Array[Any]] = null
  def f080: Array[T6 with Array[Int]] = null
  def f081: Array[Array[Any] with Any] = null
  def f082: Array[Array[Any] with AnyVal] = null
  def f083: Array[Array[Any] with Int] = null
  def f084: Array[Array[Any] with String] = null
  def f085: Array[Array[Any] with Null] = null
  def f086: Array[Array[Any] with Nothing] = null
  def f087: Array[Array[Any] with T3] = null
  def f088: Array[Array[Any] with T6] = null
  def f089: Array[Array[Any] with Array[Any]] = null
  def f090: Array[Array[Any] with Array[Int]] = null
  def f091: Array[Array[Int] with Any] = null
  def f092: Array[Array[Int] with AnyVal] = null
  def f093: Array[Array[Int] with Int] = null
  def f094: Array[Array[Int] with String] = null
  def f095: Array[Array[Int] with Null] = null
  def f096: Array[Array[Int] with Nothing] = null
  def f097: Array[Array[Int] with T3] = null
  def f098: Array[Array[Int] with T6] = null
  def f099: Array[Array[Int] with Array[Any]] = null
  def f100: Array[Array[Int] with Array[Int]] = null
  def f101: Array[Any] = null
  def f102: Array[_ <: Any] = null
  def f103: Array[T forSome { type T <: Any }] = null
  def f104: Array[AnyVal] = null
  def f105: Array[_ <: AnyVal] = null
  def f106: Array[T forSome { type T <: AnyVal }] = null
  def f107: Array[Int] = null
  def f108: Array[_ <: Int] = null
  def f109: Array[T forSome { type T <: Int }] = null
  def f110: Array[String] = null
  def f111: Array[_ <: String] = null
  def f112: Array[T forSome { type T <: String }] = null
  def f113: Array[Null] = null
  def f114: Array[_ <: Null] = null
  def f115: Array[T forSome { type T <: Null }] = null
  def f116: Array[Nothing] = null
  def f117: Array[_ <: Nothing] = null
  def f118: Array[T forSome { type T <: Nothing }] = null
  def f119: Array[T3] = null
  def f120: Array[_ <: T3] = null
  def f121: Array[T forSome { type T <: T3 }] = null
  def f122: Array[T6] = null
  def f123: Array[_ <: T6] = null
  def f124: Array[T forSome { type T <: T6 }] = null
  def f125: Array[Array[Any]] = null
  def f126: Array[_ <: Array[Any]] = null
  def f127: Array[T forSome { type T <: Array[Any] }] = null
  def f128: Array[Array[Int]] = null
  def f129: Array[_ <: Array[Int]] = null
  def f130: Array[T forSome { type T <: Array[Int] }] = null
  def f131: Array[this.type] = null
  def f132: Array[_ <: this.type] = null
  def f133: Array[T forSome { type T <: this.type }] = null
  def f134: Array[A.type] = null
  def f135: Array[_ <: A.type] = null
  def f136: Array[T forSome { type T <: A.type }] = null
  def f137: Array[T1] = null
  def f138: Array[_ <: T1] = null
  def f139: Array[T forSome { type T <: T1 }] = null
  def f140: Array[T2] = null
  def f141: Array[_ <: T2] = null
  def f142: Array[T forSome { type T <: T2 }] = null
  def f143: Array[T4] = null
  def f144: Array[_ <: T4] = null
  def f145: Array[T forSome { type T <: T4 }] = null
  def f146: Array[T5] = null
  def f147: Array[_ <: T5] = null
  def f148: Array[T forSome { type T <: T5 }] = null
  def g001 = "Array[Any with Any]"
  def g002 = "Array[Any with AnyVal]"
  def g003 = "Array[Any with Int]"
  def g004 = "Array[Any with String]"
  def g005 = "Array[Any with Null]"
  def g006 = "Array[Any with Nothing]"
  def g007 = "Array[Any with T3]"
  def g008 = "Array[Any with T6]"
  def g009 = "Array[Any with Array[Any]]"
  def g010 = "Array[Any with Array[Int]]"
  def g011 = "Array[AnyVal with Any]"
  def g012 = "Array[AnyVal with AnyVal]"
  def g013 = "Array[AnyVal with Int]"
  def g014 = "Array[AnyVal with String]"
  def g015 = "Array[AnyVal with Null]"
  def g016 = "Array[AnyVal with Nothing]"
  def g017 = "Array[AnyVal with T3]"
  def g018 = "Array[AnyVal with T6]"
  def g019 = "Array[AnyVal with Array[Any]]"
  def g020 = "Array[AnyVal with Array[Int]]"
  def g021 = "Array[Int with Any]"
  def g022 = "Array[Int with AnyVal]"
  def g023 = "Array[Int with Int]"
  def g024 = "Array[Int with String]"
  def g025 = "Array[Int with Null]"
  def g026 = "Array[Int with Nothing]"
  def g027 = "Array[Int with T3]"
  def g028 = "Array[Int with T6]"
  def g029 = "Array[Int with Array[Any]]"
  def g030 = "Array[Int with Array[Int]]"
  def g031 = "Array[String with Any]"
  def g032 = "Array[String with AnyVal]"
  def g033 = "Array[String with Int]"
  def g034 = "Array[String with String]"
  def g035 = "Array[String with Null]"
  def g036 = "Array[String with Nothing]"
  def g037 = "Array[String with T3]"
  def g038 = "Array[String with T6]"
  def g039 = "Array[String with Array[Any]]"
  def g040 = "Array[String with Array[Int]]"
  def g041 = "Array[Null with Any]"
  def g042 = "Array[Null with AnyVal]"
  def g043 = "Array[Null with Int]"
  def g044 = "Array[Null with String]"
  def g045 = "Array[Null with Null]"
  def g046 = "Array[Null with Nothing]"
  def g047 = "Array[Null with T3]"
  def g048 = "Array[Null with T6]"
  def g049 = "Array[Null with Array[Any]]"
  def g050 = "Array[Null with Array[Int]]"
  def g051 = "Array[Nothing with Any]"
  def g052 = "Array[Nothing with AnyVal]"
  def g053 = "Array[Nothing with Int]"
  def g054 = "Array[Nothing with String]"
  def g055 = "Array[Nothing with Null]"
  def g056 = "Array[Nothing with Nothing]"
  def g057 = "Array[Nothing with T3]"
  def g058 = "Array[Nothing with T6]"
  def g059 = "Array[Nothing with Array[Any]]"
  def g060 = "Array[Nothing with Array[Int]]"
  def g061 = "Array[T3 with Any]"
  def g062 = "Array[T3 with AnyVal]"
  def g063 = "Array[T3 with Int]"
  def g064 = "Array[T3 with String]"
  def g065 = "Array[T3 with Null]"
  def g066 = "Array[T3 with Nothing]"
  def g067 = "Array[T3 with T3]"
  def g068 = "Array[T3 with T6]"
  def g069 = "Array[T3 with Array[Any]]"
  def g070 = "Array[T3 with Array[Int]]"
  def g071 = "Array[T6 with Any]"
  def g072 = "Array[T6 with AnyVal]"
  def g073 = "Array[T6 with Int]"
  def g074 = "Array[T6 with String]"
  def g075 = "Array[T6 with Null]"
  def g076 = "Array[T6 with Nothing]"
  def g077 = "Array[T6 with T3]"
  def g078 = "Array[T6 with T6]"
  def g079 = "Array[T6 with Array[Any]]"
  def g080 = "Array[T6 with Array[Int]]"
  def g081 = "Array[Array[Any] with Any]"
  def g082 = "Array[Array[Any] with AnyVal]"
  def g083 = "Array[Array[Any] with Int]"
  def g084 = "Array[Array[Any] with String]"
  def g085 = "Array[Array[Any] with Null]"
  def g086 = "Array[Array[Any] with Nothing]"
  def g087 = "Array[Array[Any] with T3]"
  def g088 = "Array[Array[Any] with T6]"
  def g089 = "Array[Array[Any] with Array[Any]]"
  def g090 = "Array[Array[Any] with Array[Int]]"
  def g091 = "Array[Array[Int] with Any]"
  def g092 = "Array[Array[Int] with AnyVal]"
  def g093 = "Array[Array[Int] with Int]"
  def g094 = "Array[Array[Int] with String]"
  def g095 = "Array[Array[Int] with Null]"
  def g096 = "Array[Array[Int] with Nothing]"
  def g097 = "Array[Array[Int] with T3]"
  def g098 = "Array[Array[Int] with T6]"
  def g099 = "Array[Array[Int] with Array[Any]]"
  def g100 = "Array[Array[Int] with Array[Int]]"
  def g101 = "Array[Any]"
  def g102 = "Array[_ <: Any]"
  def g103 = "Array[T forSome { type T <: Any }]"
  def g104 = "Array[AnyVal]"
  def g105 = "Array[_ <: AnyVal]"
  def g106 = "Array[T forSome { type T <: AnyVal }]"
  def g107 = "Array[Int]"
  def g108 = "Array[_ <: Int]"
  def g109 = "Array[T forSome { type T <: Int }]"
  def g110 = "Array[String]"
  def g111 = "Array[_ <: String]"
  def g112 = "Array[T forSome { type T <: String }]"
  def g113 = "Array[Null]"
  def g114 = "Array[_ <: Null]"
  def g115 = "Array[T forSome { type T <: Null }]"
  def g116 = "Array[Nothing]"
  def g117 = "Array[_ <: Nothing]"
  def g118 = "Array[T forSome { type T <: Nothing }]"
  def g119 = "Array[T3]"
  def g120 = "Array[_ <: T3]"
  def g121 = "Array[T forSome { type T <: T3 }]"
  def g122 = "Array[T6]"
  def g123 = "Array[_ <: T6]"
  def g124 = "Array[T forSome { type T <: T6 }]"
  def g125 = "Array[Array[Any]]"
  def g126 = "Array[_ <: Array[Any]]"
  def g127 = "Array[T forSome { type T <: Array[Any] }]"
  def g128 = "Array[Array[Int]]"
  def g129 = "Array[_ <: Array[Int]]"
  def g130 = "Array[T forSome { type T <: Array[Int] }]"
  def g131 = "Array[this.type]"
  def g132 = "Array[_ <: this.type]"
  def g133 = "Array[T forSome { type T <: this.type }]"
  def g134 = "Array[A.type]"
  def g135 = "Array[_ <: A.type]"
  def g136 = "Array[T forSome { type T <: A.type }]"
  def g137 = "Array[T1]"
  def g138 = "Array[_ <: T1]"
  def g139 = "Array[T forSome { type T <: T1 }]"
  def g140 = "Array[T2]"
  def g141 = "Array[_ <: T2]"
  def g142 = "Array[T forSome { type T <: T2 }]"
  def g143 = "Array[T4]"
  def g144 = "Array[_ <: T4]"
  def g145 = "Array[T forSome { type T <: T4 }]"
  def g146 = "Array[T5]"
  def g147 = "Array[_ <: T5]"
  def g148 = "Array[T forSome { type T <: T5 }]"
}

object Test {
  val methods = A.getClass.getMethods.toList

  def lookupType(name: String): String = {
    methods find (_.getName == "g" + name.tail) match {
      case Some(m) => m.invoke(A).asInstanceOf[String]
      case _       => "<error>"
    }
  }
  def returnType(s: String): String = (
    if (s startsWith "[") s"Array[${returnType(s.tail)}]"
    else if ((s startsWith "L") && (s endsWith ";")) s.init.tail
    else s
  ).replaceAllLiterally("java.lang.", "")

  def main(args: Array[String]): Unit = {
    println(f"""${"type"}%45s  erasure""")
    println("")

    ( A.getClass.getMethods
      filter(_.getName startsWith "f")
      map(m => m.getName -> returnType(m.getReturnType.getName))
      sortBy (_._1)
      foreach { case (k, v) => println(f"${lookupType(k)}%45s  $v") }
    )
  }
}

object Generate {
  val id = { var x = 1 ; () => try x finally x += 1 }

  def mkArrayDecl(tp: String) = {
    val x = "%03d" format id()
    List(
      s"def f$x: Array[$tp] = null",
      s"""def g$x = "Array[$tp]" """
    )
  }

  val tps1 = List("Any", "AnyVal", "Int", "String", "Null", "Nothing", "T3", "T6", "Array[Any]", "Array[Int]")
  val tps2 = List("this.type", "A.type", "T1", "T2", "T4", "T5")
  // val tps3 = tps1 map (s => s"Array[$s]")

  def generate() {
    val pairs  = for (t1 <- tps1; t2 <- tps1) yield t1 -> t2
    val lines1 = pairs flatMap { case (t1, t2) => mkArrayDecl(s"$t1 with $t2") }
    val lines2 = (tps1 ++ tps2) flatMap (tp => List(tp, s"_ <: $tp", s"T forSome { type T <: $tp }")) flatMap mkArrayDecl

    (lines1 ++ lines2).sorted foreach println
  }

  def main(args: Array[String]): Unit = generate()
}
