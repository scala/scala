object Test extends App {

def test[A](name: String, expect: A, actual: => A) {
    if (expect != actual) throw new AssertionError("test " + name + " failed")
    else println("test " + name + " completed properly")
}

def testNoBraces = 1
test("no braces", 1, testNoBraces)

val testNoBracesR = testNoBraces _
test("no braces r", 1, testNoBracesR())

def testPlain(x: String, y: String): String = x + y
test("plain", "ab", testPlain("a", "b"))

val testPlainR = testPlain _
test("plain r", "cd", testPlainR("c", "d"))

def testOldByName(x: => Int) = x + 1
test("old by name", 3, testOldByName(1 + 1))

val testOldByNameR = testOldByName _
test("old by name r", 3, testOldByNameR(1 + 1))

val testOldByNameS: (=> Int) => Int = testOldByName _
test("old by name s", 3, testOldByNameS(2))

def testRegThenByName(x: Int, y: => Int): Int = x + y
test("reg then by name", 7, testRegThenByName(3, 2 * 2))

val testRegThenByNameS: (Int, =>Int) => Int = testRegThenByName _
test("reg then by name s", 8, testRegThenByNameS(2, 12 / 2))

def testVarargs(x: Int*) = x.reduceLeft((x: Int, y: Int) => x + y)
test("varargs", 4, testVarargs(1, 2, 1))

val testVarargsR = testVarargs _
test("varargs r", 4, testVarargsR(Seq(1, 2, 1)))

def testAll(x: Int, y: => Int, z: Int*) = x + y + z.size
test("all", 5, testAll(1, 2, 22, 23))

val testAllR = testAll _
test("all r", 7, testAllR(2, 3, Seq(34, 35)))

val testAllS: (Int, =>Int, Int*) => Int = testAll _
test("all s", 8, testAllS(1, 5, 78, 89))

// test currying

def testC00()(): Int = 1
test("c00", 1, testC00()())

val testC00R = testC00 _
test("c00 r", 1, testC00R()())

val testC00RR = testC00() _
test("c00 rr", 1, testC00RR())


def testCBB(a: => Int)(b: => Int) = a + b
test("cbb", 3, testCBB(1)(2))

val testCBBR = testCBB _
test("cbb r", 5, testCBBR(1)(4))

val testCBBRR = testCBB(4) _
test("cbb rr", 6, testCBBRR(2))


def testCVV(a: Int*)(z: String, b: Int*) = a.size + b.size
test("cvv", 3, testCVV(1, 2)("", 8))

val testCVVR = testCVV _
test("cvv r", 3, testCVVR(Seq(1))("", Seq(8, 9)))

val testCVVRS: (String, Int*) => Int = testCVV(2, 3)
test("cvv rs", 4, testCVVRS("", 5, 6))

println("$")

// vim: set ts=4 sw=4 et:
}
