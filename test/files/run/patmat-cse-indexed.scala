// More than 32 tests in a match (`doCSE`'s `IndexAbove`) with shared prefixes, so prior tests are looked up in an index.
sealed trait T
final case class D0(a: Int, b: Int) extends T
final case class D1(a: Int, b: Int) extends T
final case class D2(a: Int, b: Int) extends T
final case class D3(a: Int, b: Int) extends T
final case class D4(a: Int, b: Int) extends T
final case class D5(a: Int, b: Int) extends T
final case class D6(a: Int, b: Int) extends T
final case class D7(a: Int, b: Int) extends T
final case class D8(a: Int, b: Int) extends T
final case class D9(a: Int, b: Int) extends T
final case class D10(a: Int, b: Int) extends T
final case class D11(a: Int, b: Int) extends T

object Test {
  def f(a: T, b: T): Int = (a, b) match {
    case (D0(_, _), D1(1, _)) => 1
    case (D0(_, _), D2(1, _)) => 2
    case (D0(_, _), D4(1, _)) => 4
    case (D0(_, _), D5(1, _)) => 5
    case (D0(_, _), D7(1, _)) => 7
    case (D0(_, _), D8(1, _)) => 8
    case (D0(_, _), D10(1, _)) => 10
    case (D0(_, _), D11(1, _)) => 11
    case (D1(_, _), D0(1, _)) => 12
    case (D1(_, _), D1(1, _)) => 13
    case (D1(_, _), D3(1, _)) => 15
    case (D1(_, _), D4(1, _)) => 16
    case (D1(_, _), D6(1, _)) => 18
    case (D1(_, _), D7(1, _)) => 19
    case (D1(_, _), D9(1, _)) => 21
    case (D1(_, _), D10(1, _)) => 22
    case (D2(_, _), D0(1, _)) => 24
    case (D2(_, _), D2(1, _)) => 26
    case (D2(_, _), D3(1, _)) => 27
    case (D2(_, _), D5(1, _)) => 29
    case (D2(_, _), D6(1, _)) => 30
    case (D2(_, _), D8(1, _)) => 32
    case (D2(_, _), D9(1, _)) => 33
    case (D2(_, _), D11(1, _)) => 35
    case (D3(_, _), D1(1, _)) => 37
    case (D3(_, _), D2(1, _)) => 38
    case (D3(_, _), D4(1, _)) => 40
    case (D3(_, _), D5(1, _)) => 41
    case (D3(_, _), D7(1, _)) => 43
    case (D3(_, _), D8(1, _)) => 44
    case (D3(_, _), D10(1, _)) => 46
    case (D3(_, _), D11(1, _)) => 47
    case (D4(_, _), D0(1, _)) => 48
    case (D4(_, _), D1(1, _)) => 49
    case (D4(_, _), D3(1, _)) => 51
    case (D4(_, _), D4(1, _)) => 52
    case (D4(_, _), D6(1, _)) => 54
    case (D4(_, _), D7(1, _)) => 55
    case (D4(_, _), D9(1, _)) => 57
    case (D4(_, _), D10(1, _)) => 58
    case (D5(_, _), D0(1, _)) => 60
    case (D5(_, _), D2(1, _)) => 62
    case (D5(_, _), D3(1, _)) => 63
    case (D5(_, _), D5(1, _)) => 65
    case (D5(_, _), D6(1, _)) => 66
    case (D5(_, _), D8(1, _)) => 68
    case (D5(_, _), D9(1, _)) => 69
    case (D5(_, _), D11(1, _)) => 71
    case (D6(_, _), D1(1, _)) => 73
    case (D6(_, _), D2(1, _)) => 74
    case (D6(_, _), D4(1, _)) => 76
    case (D6(_, _), D5(1, _)) => 77
    case (D6(_, _), D7(1, _)) => 79
    case (D6(_, _), D8(1, _)) => 80
    case (D6(_, _), D10(1, _)) => 82
    case (D6(_, _), D11(1, _)) => 83
    case (D7(_, _), D0(1, _)) => 84
    case (D7(_, _), D1(1, _)) => 85
    case (D7(_, _), D3(1, _)) => 87
    case (D7(_, _), D4(1, _)) => 88
    case (D7(_, _), D6(1, _)) => 90
    case (D7(_, _), D7(1, _)) => 91
    case (D7(_, _), D9(1, _)) => 93
    case (D7(_, _), D10(1, _)) => 94
    case (D8(_, _), D0(1, _)) => 96
    case (D8(_, _), D2(1, _)) => 98
    case (D8(_, _), D3(1, _)) => 99
    case (D8(_, _), D5(1, _)) => 101
    case (D8(_, _), D6(1, _)) => 102
    case (D8(_, _), D8(1, _)) => 104
    case (D8(_, _), D9(1, _)) => 105
    case (D8(_, _), D11(1, _)) => 107
    case (D9(_, _), D1(1, _)) => 109
    case (D9(_, _), D2(1, _)) => 110
    case (D9(_, _), D4(1, _)) => 112
    case (D9(_, _), D5(1, _)) => 113
    case (D9(_, _), D7(1, _)) => 115
    case (D9(_, _), D8(1, _)) => 116
    case (D9(_, _), D10(1, _)) => 118
    case (D9(_, _), D11(1, _)) => 119
    case (D10(_, _), D0(1, _)) => 120
    case (D10(_, _), D1(1, _)) => 121
    case (D10(_, _), D3(1, _)) => 123
    case (D10(_, _), D4(1, _)) => 124
    case (D10(_, _), D6(1, _)) => 126
    case (D10(_, _), D7(1, _)) => 127
    case (D10(_, _), D9(1, _)) => 129
    case (D10(_, _), D10(1, _)) => 130
    case (D11(_, _), D0(1, _)) => 132
    case (D11(_, _), D2(1, _)) => 134
    case (D11(_, _), D3(1, _)) => 135
    case (D11(_, _), D5(1, _)) => 137
    case (D11(_, _), D6(1, _)) => 138
    case (D11(_, _), D8(1, _)) => 140
    case (D11(_, _), D9(1, _)) => 141
    case (D11(_, _), D11(1, _)) => 143
    case (D0(_, _), D0(_, 2)) => -2
    case _ => -1
  }
  def g(a: T, b: T): Int = (a, b) match {
    case (D0(_, _), D1(1, _)) => 1
    case (D0(_, _), D2(1, _)) => 2
    case (D0(_, _), D4(1, _)) => 4
    case (D0(_, _), D5(1, _)) => 5
    case (D0(_, _), D7(1, _)) => 7
    case (D0(_, _), D8(1, _)) => 8
    case (D0(_, _), D10(1, _)) => 10
    case (D0(_, _), D11(1, _)) => 11
    case (D1(_, _), D0(1, _)) => 12
    case (D1(_, _), D1(1, _)) => 13
    case (D1(_, _), D3(1, _)) => 15
    case (D1(_, _), D4(1, _)) => 16
    case (D1(_, _), D6(1, _)) => 18
    case (D1(_, _), D7(1, _)) => 19
    case (D1(_, _), D9(1, _)) => 21
    case (D1(_, _), D10(1, _)) => 22
    case (D2(_, _), D0(1, _)) => 24
    case (D2(_, _), D2(1, _)) => 26
    case (D2(_, _), D3(1, _)) => 27
    case (D2(_, _), D5(1, _)) => 29
    case (D2(_, _), D6(1, _)) => 30
    case (D2(_, _), D8(1, _)) => 32
    case (D2(_, _), D9(1, _)) => 33
    case (D2(_, _), D11(1, _)) => 35
    case (D3(_, _), D1(1, _)) => 37
    case (D3(_, _), D2(1, _)) => 38
    case (D3(_, _), D4(1, _)) => 40
    case (D3(_, _), D5(1, _)) => 41
    case (D3(_, _), D7(1, _)) => 43
    case (D3(_, _), D8(1, _)) => 44
    case (D3(_, _), D10(1, _)) => 46
    case (D3(_, _), D11(1, _)) => 47
    case (D4(_, _), D0(1, _)) => 48
    case (D4(_, _), D1(1, _)) => 49
    case (D4(_, _), D3(1, _)) => 51
    case (D4(_, _), D4(1, _)) => 52
    case (D4(_, _), D6(1, _)) => 54
    case (D4(_, _), D7(1, _)) => 55
    case (D4(_, _), D9(1, _)) => 57
    case (D4(_, _), D10(1, _)) => 58
    case (D5(_, _), D0(1, _)) => 60
    case (D5(_, _), D2(1, _)) => 62
    case (D5(_, _), D3(1, _)) => 63
    case (D5(_, _), D5(1, _)) => 65
    case (D5(_, _), D6(1, _)) => 66
    case (D5(_, _), D8(1, _)) => 68
    case (D5(_, _), D9(1, _)) => 69
    case (D5(_, _), D11(1, _)) => 71
    case (D6(_, _), D1(1, _)) => 73
    case (D6(_, _), D2(1, _)) => 74
    case (D6(_, _), D4(1, _)) => 76
    case (D6(_, _), D5(1, _)) => 77
    case (D6(_, _), D7(1, _)) => 79
    case (D6(_, _), D8(1, _)) => 80
    case (D6(_, _), D10(1, _)) => 82
    case (D6(_, _), D11(1, _)) => 83
    case (D7(_, _), D0(1, _)) => 84
    case (D7(_, _), D1(1, _)) => 85
    case (D7(_, _), D3(1, _)) => 87
    case (D7(_, _), D4(1, _)) => 88
    case (D7(_, _), D6(1, _)) => 90
    case (D7(_, _), D7(1, _)) => 91
    case (D7(_, _), D9(1, _)) => 93
    case (D7(_, _), D10(1, _)) => 94
    case (D8(_, _), D0(1, _)) => 96
    case (D8(_, _), D2(1, _)) => 98
    case (D8(_, _), D3(1, _)) => 99
    case (D8(_, _), D5(1, _)) => 101
    case (D8(_, _), D6(1, _)) => 102
    case (D8(_, _), D8(1, _)) => 104
    case (D8(_, _), D9(1, _)) => 105
    case (D8(_, _), D11(1, _)) => 107
    case (D9(_, _), D1(1, _)) => 109
    case (D9(_, _), D2(1, _)) => 110
    case (D9(_, _), D4(1, _)) => 112
    case (D9(_, _), D5(1, _)) => 113
    case (D9(_, _), D7(1, _)) => 115
    case (D9(_, _), D8(1, _)) => 116
    case (D9(_, _), D10(1, _)) => 118
    case (D9(_, _), D11(1, _)) => 119
    case (D10(_, _), D0(1, _)) => 120
    case (D10(_, _), D1(1, _)) => 121
    case (D10(_, _), D3(1, _)) => 123
    case (D10(_, _), D4(1, _)) => 124
    case (D10(_, _), D6(1, _)) => 126
    case (D10(_, _), D7(1, _)) => 127
    case (D10(_, _), D9(1, _)) => 129
    case (D10(_, _), D10(1, _)) => 130
    case (D11(_, _), D0(1, _)) => 132
    case (D11(_, _), D2(1, _)) => 134
    case (D11(_, _), D3(1, _)) => 135
    case (D11(_, _), D5(1, _)) => 137
    case (D11(_, _), D6(1, _)) => 138
    case (D11(_, _), D8(1, _)) => 140
    case (D11(_, _), D9(1, _)) => 141
    case (D11(_, _), D11(1, _)) => 143
    case (x, y) if x == y => -3
    case _ => -1
  }
  def main(args: Array[String]): Unit = {
    val all = (0 until 12).map(i => (new D0(i, 1): T)) ++ (0 until 12).map(i => (D1(i, 2): T)) ++ List[T](D2(1, 1), D3(1, 1), D11(1, 1), D5(2, 2))
    var s = 0
    for (a <- all; b <- all) { s = s * 31 + f(a, b) + g(a, b) }
    println(s)
  }
}
