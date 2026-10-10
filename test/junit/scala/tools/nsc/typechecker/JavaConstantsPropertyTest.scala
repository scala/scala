package scala.tools.nsc.typechecker

import org.junit.Assert.fail
import org.junit.Test

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.BatchSourceFile
import scala.util.Properties.isJavaAtLeast
import scala.util.Random
import scala.util.control.NonFatal

/** Like `JavaConstantsTest`, but with random constant expressions biased towards numeric edge cases:
 *  boundary values, negative zero, NaN and infinities, subnormals, literals that need rounding,
 *  every literal syntax, overflowing arithmetic and casts, shift distances out of range, the typing
 *  of `?:`, string conversion, and references to other fields, including cyclic ones.
 *
 *  The seeds are fixed, so that the test is deterministic. To explore further, run with
 *  `-Dscala.javaconstants.seeds=1,2,3` or `-Dscala.javaconstants.seeds=random:100`.
 */
class JavaConstantsPropertyTest {
  import JavaConstantsPropertyTest._

  @Test def randomConstantExpressions(): Unit = {
    val seeds = sys.props.get("scala.javaconstants.seeds") match {
      case Some(s) if s.startsWith("random:") => List.fill(s.stripPrefix("random:").toInt)(Random.nextLong())
      case Some(s)                            => s.split(",").toList.map(_.trim.toLong)
      case None                               => (1L to 20L).toList
    }
    val failures = seeds.flatMap(check(_, fields = 400))
    if (failures.nonEmpty) fail(failures.mkString("\n\n"))
  }

  def check(seed: Long, fields: Int): Option[String] = {
    def withSeed[R](what: String)(body: => R): R =
      try body catch { case NonFatal(e) => throw new AssertionError(s"seed $seed: $what", e) }
    val gen = new Gen(new Random(seed), fields)
    val source = withSeed("generator crashed")(gen.source)
    val expected = withSeed("invalid Java")(JavaConstantsTest.javacFieldTypes("J.java", source, List("J")))
    val actual = withSeed("scalac failed to compile valid Java") {
      JavaConstantsTest.fieldTypes(JavaConstantsTest.newGlobal(""), List(new BatchSourceFile("J.java", source)), List("J"))
    }
    val decls = gen.decls.map { case (name, code) => s"J.$name" -> code }.toMap
    def byField(types: List[String]) = types.map(t => t.substring(0, t.indexOf(':')) -> t).toMap
    val e = byField(expected)
    val a = byField(actual)
    val diffs = (e.keySet ++ a.keySet).toList.sorted.filter(k => e.get(k) != a.get(k)).map { k =>
      s"  ${decls.getOrElse(k, "?")}\n    javac:  ${e.getOrElse(k, "<missing>")}\n    scalac: ${a.getOrElse(k, "<missing>")}"
    }
    if (diffs.isEmpty) None
    else Some(s"seed $seed: ${diffs.size} of ${e.size} fields differ\n${diffs.mkString("\n")}")
  }
}

object JavaConstantsPropertyTest {
  sealed abstract class T(val name: String, val rank: Int)
  case object Z   extends T("boolean", -1)
  case object B   extends T("byte", 0)
  case object S   extends T("short", 1)
  case object C   extends T("char", 2)
  case object I   extends T("int", 3)
  case object J   extends T("long", 4)
  case object F   extends T("float", 5)
  case object D   extends T("double", 6)
  case object Str extends T("String", -1)

  val numeric  = List(B, S, C, I, J, F, D)
  val integral = List(B, S, C, I, J)
  val subInt   = List(B, S, C)

  /** Generated expression, with precedence of its outermost operator (higher binds tighter). */
  final case class E(code: String, prec: Int)

  // JLS 15: precedence levels
  val Ternary = 1; val OrOr = 2; val AndAnd = 3; val Or = 4; val Xor = 5; val And = 6
  val Equality = 7; val Relational = 8; val Shift = 9; val Additive = 10; val Multiplicative = 11
  val Unary = 12; val Primary = 13

  /** Generates a class with `n` constant (and occasionally non-constant) fields.
   *
   *  `gen(t)` produces an expression whose static type is assignable to `t` by widening:
   *  the exact type of `?:` depends on the values of its operands, which we don't track.
   */
  final class Gen(rnd: Random, n: Int) {
    /** The types of the initializers of the fields, and the declared types, which are occasionally wider. */
    val types: Vector[T] = Vector.fill(n)(oneOf(numeric ::: numeric ::: List(Z, Str, Str)))
    val declared: Vector[T] = types.map(t => if (chance(0.15) && widenings(t).nonEmpty) oneOf(widenings(t)) else t)
    val decls = ListBuffer[(String, String)]()

    def oneOf[Elem](as: Seq[Elem]): Elem = as(rnd.nextInt(as.length))
    def chance(p: Double) = rnd.nextDouble() < p

    def paren(e: E, min: Int): String = if (e.prec >= min) e.code else s"(${e.code})"
    def binary(x: E, op: String, y: E, prec: Int) = E(s"${paren(x, prec)} $op ${paren(y, prec + 1)}", prec)
    def unary(op: String, x: E) = {
      val operand = paren(x, Unary)
      E(if (operand.startsWith("+") || operand.startsWith("-")) s"$op $operand" else s"$op$operand", Unary)
    }
    def cast(t: T, x: E) = E(s"(${if (t == Str && chance(0.5)) "java.lang.String" else t.name}) ${paren(x, Unary)}", Unary)
    def negated(lit: String) = E(if (lit.startsWith("-")) lit else "-" + lit, Unary)

    // Literals

    def intValue(): Long = oneOf(List[Long](
      0, 1, 2, 7, 8, 31, 32, 33, 63, 64, 65, 100, 127, 128, 255, 256, 1000, 32767, 32768, 65535, 65536,
      Int.MaxValue, 1L << 31, 0xFFFFFFFFL, rnd.nextInt(1000), rnd.nextInt() & 0xFFFFFFFFL))

    /** A literal for `value` in a random radix. Negative values and values above `decimalMax` are written as bit patterns. */
    def integralLiteral(value: Long, suffix: String, decimalMax: Long): String = {
      val radix = if (value == 0) 0 else if (value < 0 || value > decimalMax) 1 + rnd.nextInt(3) else rnd.nextInt(4)
      val (prefix, digits) = radix match {
        case 1 => (oneOf(List("0x", "0X")), if (chance(0.5)) value.toHexString else value.toHexString.toUpperCase)
        case 2 => ("0", value.toOctalString)
        case 3 => (oneOf(List("0b", "0B")), value.toBinaryString)
        case _ => ("", value.toString)
      }
      // underscores are allowed between digits, and after the leading 0 of octal literals
      val underscored =
        if (digits.length > 1 && chance(0.2)) {
          val i = (if (radix == 2) 0 else 1) + rnd.nextInt(digits.length - 1)
          digits.substring(0, i) + "__".take(1 + rnd.nextInt(2)) + digits.substring(i)
        } else digits
      prefix + underscored + suffix
    }

    def intLiteral(): E = {
      val v = intValue()
      if (v == (1L << 31) && chance(0.5)) E("-2147483648", Unary) // only allowed as the operand of unary minus
      else {
        val lit = integralLiteral(v, "", Int.MaxValue)
        if (chance(0.3)) negated(lit) else E(lit, Primary)
      }
    }

    def longLiteral(): E = {
      val suffix = if (chance(0.5)) "L" else "l"
      val v = oneOf(List(intValue(), Long.MaxValue, Long.MinValue, -1L, rnd.nextLong(), 1L << rnd.nextInt(64)))
      if (v == Long.MinValue && chance(0.5)) E("-9223372036854775808" + suffix, Unary)
      else if (v < 0 && v != Long.MinValue && chance(0.5)) negated(integralLiteral(-v, suffix, Long.MaxValue))
      else {
        val lit = integralLiteral(v, suffix, Long.MaxValue)
        if (chance(0.3)) negated(lit) else E(lit, Primary)
      }
    }

    def floatLiteral(): E = {
      val suffix = if (chance(0.5)) "f" else "F"
      val lit = oneOf(List(
        "0", "0.0", ".5", "5.", "1", "1e0", "1E+0", "1e-0", "0.1", "1e10", "16777216", "16777217", "16777219", "33554435",
        "3.4028235e38", "3.4028235677973366e38", "1.17549435E-38", "1.17549421E-38", "1.4e-45", "1.401298464324817e-45",
        "2.1e-45", "1.00000017881393432617187499", "1.000000178813934326171875",
        "0x1p-149", "0x1.fffffeP+127", "0x.8p1", "0X1P0", "0x1.000001p0", "0x1.0000011p0", "0x1p-126", "0x0.000002p-126",
        "123456789", "4294967295", "9223372036854775807",
      ).appended {
        val f = java.lang.Float.intBitsToFloat(rnd.nextInt() & 0x7FFFFFFF)
        if (f.isNaN || f.isInfinite || f == 0) "2" else java.lang.Float.toString(f)
      })
      val e = E(lit + suffix, Primary)
      if (chance(0.3)) negated(e.code) else e
    }

    def doubleLiteral(): E = {
      val suffix = oneOf(List("", "", "d", "D"))
      val lit = oneOf(List(
        "0.0", "0.", ".0", "1.0", "0.1", "1e23", "8.41e21", "5e-324", "4.9e-324", "2.4703282292062328e-324",
        "1.7976931348623157e308", "1.7976931348623158e308", "2.2250738585072014E-308", "2.225073858507201e-308",
        "9007199254740993.0", "0x1p-1074", "0x1.fffffffffffffp1023", "0x1.0000000000000801p0", "0x.0000000000001P-1022",
        "1e-5", "3.4028235677973366e38", "1.401298464324817e-45",
      ).appended {
        val d = java.lang.Double.longBitsToDouble(rnd.nextLong() & Long.MaxValue)
        if (d.isNaN || d.isInfinite || d == 0) "2.0" else java.lang.Double.toString(d)
      }.appended(if (suffix.isEmpty) "1e0" else oneOf(List("1", "9007199254740993", "123"))))
      val e = E(lit + suffix, Primary)
      if (chance(0.3)) negated(e.code) else e
    }

    def charContent(quote: Char): String = {
      def unicodeEscape(c: Int) = (if (chance(0.1)) "\\uu" else "\\u") + f"$c%04x"
      rnd.nextInt(5) match {
        case 0 => oneOf(List("\\n", "\\t", "\\b", "\\r", "\\f", "\\0", "\\7", "\\77", "\\377", "\\\\", "\\'", "\\\"") ++ Option.when(isJavaAtLeast(15))("\\s"))
        case 1 => oneOf(List("a", "Z", "0", " ", "~", "é", "€", "中", if (quote == '"') "😀" else "x"))
        case 2 => unicodeEscape(oneOf(List(0, 1, 0x7f, 0x80, 0xff, 0x100, 0x7fff, 0x8000, 0xd800, 0xdbff, 0xdc00, 0xdfff, 0xfffe, 0xffff)))
        case _ =>
          val c = rnd.nextInt(0x10000)
          if (c == '\n' || c == '\r' || c == '\'' || c == '"' || c == '\\') "q"
          else unicodeEscape(c)
      }
    }
    def charLiteral() = E(s"'${charContent('\'')}'", Primary)
    def stringLiteral() = E("\"" + List.fill(rnd.nextInt(3))(charContent('"')).mkString + "\"", Primary)

    // Leaves

    def libraryConstant(t: T): Option[E] = {
      val qual = (cls: String) => if (chance(0.3)) s"java.lang.$cls" else cls
      val choices: List[String] = t match {
        case B => List("Byte.MIN_VALUE", "Byte.MAX_VALUE")
        case S => List("Short.MIN_VALUE", "Short.MAX_VALUE")
        case C => List("Character.MIN_VALUE", "Character.MAX_VALUE", "Character.MIN_HIGH_SURROGATE", "Character.MAX_LOW_SURROGATE")
        case I => List("Integer.MIN_VALUE", "Integer.MAX_VALUE", "Integer.SIZE", "Long.SIZE", "Character.MAX_CODE_POINT", "Double.MAX_EXPONENT", "Float.MIN_EXPONENT")
        case J => List("Long.MIN_VALUE", "Long.MAX_VALUE")
        case F => List("Float.NaN", "Float.POSITIVE_INFINITY", "Float.NEGATIVE_INFINITY", "Float.MIN_VALUE", "Float.MAX_VALUE", "Float.MIN_NORMAL")
        case D => List("Double.NaN", "Double.POSITIVE_INFINITY", "Double.NEGATIVE_INFINITY", "Double.MIN_VALUE", "Double.MAX_VALUE", "Double.MIN_NORMAL")
        case _ => Nil
      }
      if (choices.isEmpty) None
      else {
        val c = oneOf(choices)
        Some(E(qual(c.takeWhile(_ != '.')) + c.dropWhile(_ != '.'), Primary))
      }
    }

    /** Not constant expressions, but valid Java of the given type. */
    def nonConstant(t: T): Option[E] = t match {
      case Z   => Some(E("Boolean.TRUE", Primary))
      case I   => Some(E(oneOf(List("Integer.parseInt(\"1\")", "\"ab\".length()", "new int[3].length")), Primary))
      case Str => Some(E(oneOf(List("java.io.File.separator", "String.valueOf(1)")), Primary))
      case D   => Some(E("Math.PI * Math.random()", Multiplicative))
      case _   => None
    }

    def fieldRef(t: T, self: Int): Option[E] = {
      // assignable by widening, so that sub-int fields are used where an int is expected
      val candidates = declared.indices.filter(i => declared(i) == t || (t == I && subInt.contains(declared(i))))
      if (candidates.isEmpty) None
      else {
        val i = oneOf(candidates)
        // a simple name may not be used before its declaration (JLS 8.3.3)
        val qualified = i >= self || chance(0.3)
        val e = E(if (qualified) s"J.f$i" else s"f$i", Primary)
        Some(if (chance(0.1)) E(s"(${e.code})", Primary) else e)
      }
    }

    def leaf(t: T, self: Int): E =
      Option.when(chance(0.02))(nonConstant(t)).flatten
        .orElse(Option.when(chance(0.25))(fieldRef(t, self)).flatten)
        .orElse(Option.when(chance(0.15))(libraryConstant(t)).flatten)
        .getOrElse(literal(t))

    def literal(t: T): E = t match {
        case Z   => E(if (chance(0.5)) "true" else "false", Primary)
        case B   => cast(B, intLiteral())
        case S   => cast(S, intLiteral())
        case C   => charLiteral()
        case I   => if (chance(0.15)) charLiteral() else intLiteral()
        case J   => longLiteral()
        case F   => floatLiteral()
        case D   => doubleLiteral()
        case Str => stringLiteral()
    }

    // Expressions

    /** Numeric types whose binary numeric promotion (JLS 5.6) with `t` is `t`. */
    def promotableTo(t: T): List[T] = numeric.filter(_.rank <= t.rank)

    def gen(t: T, depth: Int, self: Int): E = {
      def sub(u: T) = gen(u, depth - 1, self)
      if (depth <= 0 || chance(0.2)) return leaf(t, self)
      def ternary(): E = {
        val (x, y) = t match {
          case B | S | C if chance(0.5) =>
            // the type of `?:` is `t` if the other operand is a constant int representable in `t`
            val lit = t match {
              case B => oneOf(List(0, 1, -1, 127, -128))
              case S => oneOf(List(0, 1, -1, 127, -128, 32767, -32768))
              case _ => oneOf(List(0, 1, 97, 65535))
            }
            (sub(t), if (lit < 0) negated(lit.toString) else E(lit.toString, Primary))
          case S if chance(0.5) => (sub(S), sub(B))
          case I | J | F | D    => (sub(t), sub(oneOf(promotableTo(t))))
          case _                => (sub(t), sub(t))
        }
        val (a, b) = if (chance(0.5)) (x, y) else (y, x)
        E(s"${paren(sub(Z), OrOr)} ? ${paren(a, OrOr)} : ${paren(b, Ternary)}", Ternary)
      }
      def arithmetic(ops: List[(String, Int)]): E = {
        val (op, prec) = oneOf(ops)
        val (x, y) = (sub(t), sub(oneOf(promotableTo(t))))
        if (chance(0.5)) binary(x, op, y, prec) else binary(y, op, x, prec)
      }
      val arith = List("+" -> Additive, "-" -> Additive, "*" -> Multiplicative, "/" -> Multiplicative, "%" -> Multiplicative)
      val bitwise = List("&" -> And, "|" -> Or, "^" -> Xor)
      val productions: List[() => E] = (t match {
        case Z => List(
          () => unary("!", sub(Z)),
          () => { val (op, p) = oneOf(List("&&" -> AndAnd, "||" -> OrOr) ++ bitwise ++ List("==" -> Equality, "!=" -> Equality)); binary(sub(Z), op, sub(Z), p) },
          () => { val (op, p) = oneOf(List("<", ">", "<=", ">=").map(_ -> Relational) ++ List("==", "!=").map(_ -> Equality)); binary(sub(oneOf(numeric)), op, sub(oneOf(numeric)), p) },
          () => binary(sub(Str), oneOf(List("==", "!=")), sub(Str), Equality),
          () => cast(Z, sub(Z)),
        )
        case Str => List(
          () => if (chance(0.5)) binary(sub(Str), "+", sub(oneOf(Z :: Str :: numeric)), Additive) else binary(sub(oneOf(Z :: Str :: numeric)), "+", sub(Str), Additive),
          () => cast(Str, sub(Str)),
        )
        case B | S | C => List(() => cast(t, sub(oneOf(numeric))))
        case I | J => List(
          () => arithmetic(arith),
          () => arithmetic(bitwise),
          () => {
            val op = oneOf(List("<<", ">>", ">>>"))
            // javac 21 doesn't fold `long >>> long`, unlike later versions
            binary(sub(t), op, sub(oneOf(if (t == J && op == ">>>") I :: subInt else integral)), Shift)
          },
          () => unary(oneOf(List("-", "+", "~")), sub(if (t == I) oneOf(I :: subInt) else J)),
          () => cast(t, sub(oneOf(numeric))),
        )
        case F | D => List(
          () => arithmetic(arith),
          () => unary(oneOf(List("-", "+")), sub(t)),
          () => cast(t, sub(oneOf(numeric))),
        )
      }) :+ (() => ternary())
      oneOf(productions)()
    }

    /** Types to which `t` is assignable by widening (JLS 5.1.2). */
    def widenings(t: T): List[T] = t match {
      case B => List(S, I, J, F, D)
      case S => List(I, J, F, D)
      case C => List(I, J, F, D)
      case I => List(J, F, D)
      case J => List(F, D)
      case F => List(D)
      case _ => Nil
    }

    lazy val source: String = {
      val sb = new StringBuilder("public class J {\n")
      for (i <- 0 until n) {
        val e = gen(types(i), depth = 1 + rnd.nextInt(4), self = i)
        val decl = s"static final ${declared(i).name} f$i = ${e.code};"
        decls += (s"f$i" -> decl)
        sb.append("  ").append(decl).append('\n')
      }
      sb.append("}\n").toString
    }
  }
}
