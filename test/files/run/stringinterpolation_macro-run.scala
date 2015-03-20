/*
 * filter: inliner warnings; re-run with
 */
object Test extends App {

// 'b' / 'B' (category: general)
// -----------------------------
println(f"${null}%b")
println(f"${false}%b")
println(f"${true}%b")
println(f"${new java.lang.Boolean(false)}%b")
println(f"${new java.lang.Boolean(true)}%b")

println(f"${null}%B")
println(f"${false}%B")
println(f"${true}%B")
println(f"${new java.lang.Boolean(false)}%B")
println(f"${new java.lang.Boolean(true)}%B")

implicit val stringToBoolean = java.lang.Boolean.parseBoolean(_: String)
println(f"${"true"}%b")
println(f"${"false"}%b")

// 'h' | 'H' (category: general)
// -----------------------------
println(f"${null}%h")
println(f"${0.0}%h")
println(f"${-0.0}%h")
println(f"${"Scala"}%h")

println(f"${null}%H")
println(f"${"Scala"}%H")

// 's' | 'S' (category: general)
// -----------------------------
println(f"${null}%s")
println(f"${null}%S")
println(f"${"Scala"}%s")
println(f"${"Scala"}%S")
println(f"${5}")

// 'c' | 'C' (category: character)
// -------------------------------
println(f"${120:Char}%c")
println(f"${120:Byte}%c")
println(f"${120:Short}%c")
println(f"${120:Int}%c")
println(f"${new java.lang.Character('x')}%c")
println(f"${new java.lang.Byte(120:Byte)}%c")
println(f"${new java.lang.Short(120:Short)}%c")
println(f"${new java.lang.Integer(120)}%c")

println(f"${'x' : java.lang.Character}%c")
println(f"${(120:Byte) : java.lang.Byte}%c")
println(f"${(120:Short) : java.lang.Short}%c")
println(f"${120 : java.lang.Integer}%c")

implicit val stringToChar = (x: String) => x(0)
println(f"${"Scala"}%c")

// 'd' | 'o' | 'x' | 'X' (category: integral)
// ------------------------------------------
println(f"${120:Byte}%d")
println(f"${120:Short}%d")
println(f"${120:Int}%d")
println(f"${120:Long}%d")
println(f"${new java.lang.Byte(120:Byte)}%d")
println(f"${new java.lang.Short(120:Short)}%d")
println(f"${new java.lang.Integer(120)}%d")
println(f"${new java.lang.Long(120)}%d")
println(f"${120 : java.lang.Integer}%d")
println(f"${120 : java.lang.Long}%d")
println(f"${BigInt(120)}%d")
println(f"${new java.math.BigInteger("120")}%d")
println(f"${4}%#10X")

locally {
  val fff  = new java.util.Formattable {
    def formatTo(f: java.util.Formatter, g: Int, w: Int, p: Int) = f.format("4")
  }
  println(f"She is ${fff}%#s feet tall.")
}

{
  implicit val strToShort = (s: String) => java.lang.Short.parseShort(s)
  println(f"${"120"}%d")
  implicit val strToInt = (s: String) => 42
  println(f"${"120"}%d")
}

// 'e' | 'E' | 'g' | 'G' | 'f' | 'a' | 'A' (category: floating point)
// ------------------------------------------------------------------
println(f"${3.4f}%e")
println(f"${3.4}%e")
println(f"${3.4f : java.lang.Float}%e")
println(f"${3.4 : java.lang.Double}%e")
println(f"${BigDecimal(3.4)}%e")
println(f"${new java.math.BigDecimal(3.4)}%e")
println(f"${3}%e")
println(f"${3L}%e")

// 't' | 'T' (category: date/time)
// -------------------------------
import java.util.Calendar
import java.util.Locale
val c = Calendar.getInstance(Locale.US)
c.set(2012, Calendar.MAY, 26)
println(f"${c}%TD")
println(f"${c.getTime}%TD")
println(f"${c.getTime.getTime}%TD")

implicit val strToDate = (x: String) => c
println(f"""${"1234"}%TD""")


// literals and arg indexes
println(f"%%")
println(f" mind%n------%nmatter%n")
println(f"${7}%d %<d ${9}%d")
println(f"${7}%d %2$$d ${9}%d")

}
