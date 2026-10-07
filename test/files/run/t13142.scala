trait ConTU[-T] extends (T => Unit) {
  def apply(t: T): Unit
}

trait ConTI[-T] extends (T => Int) {
  def apply(t: T): Int
}

trait ConTS[-T] extends (T => String) {
  def apply(t: T): String
}

trait ConIR[+R] extends (Int => R) {
  def apply(t: Int): R
}

trait ConSR[+R] extends (String => R) {
  def apply(t: String): R
}

trait ConUR[+R] extends (() => R) {
  def apply(): R
}

trait ConII extends (Int => Int) {
  def apply(t: Int): Int
}

trait ConSI extends (String => Int) {
  def apply(t: String): Int
}

trait ConIS extends (Int => String) {
  def apply(t: Int): String
}

trait ConUU extends (() => Unit) {
  def apply(): Unit
}

trait ConVCVC extends (IntVal => IntVal) {
  def apply(t: IntVal): IntVal
}

trait F1[-T, +R] {
  def apply(t: T): R
}

trait SFTU[-T] extends F1[T, Unit] {
  def apply(t: T): Unit
}

trait SFTI[-T] extends F1[T, Int] {
  def apply(t: T): Int
}

trait SFTS[-T] extends F1[T, String] {
  def apply(t: T): String
}

trait SFIR [+R] extends F1[Int, R] {
  def apply(t: Int): R
}

trait SFSR [+R] extends F1[String, R] {
  def apply(t: String): R
}

trait SFII extends F1[Int, Int] {
  def apply(t: Int): Int
}

trait SFSI extends F1[String, Int] {
  def apply(t: String): Int
}

trait SFIS extends F1[Int, String] {
  def apply(t: Int): String
}

trait SFIU extends F1[Int, Unit] {
  def apply(t: Int): Unit
}

trait SFVCVC extends F1[IntVal, IntVal] {
  def apply(t: IntVal): IntVal
}

trait F1U[-T] {
  def apply(t: T): Unit
}

trait SF2T[-T] extends F1U[T] {
  def apply(t: T): Unit
}

trait SF2I extends F1U[Int] {
  def apply(t: Int): Unit
}

trait SF2S extends F1U[String] {
  def apply(t: String): Unit
}

case class IntVal(value: Int) extends AnyVal

object Test {
  def main(args: Array[String]): Unit = {
    val fIU: (Int => Unit) = (x: Int) => println(x) // closure by JFunction1
    fIU(1)

    val fIS: (Int => String) = (x: Int) => x.toString // closure
    println(fIS(2))

    val fUI: (() => Int) = () => 3 // closure
    println(fUI())

    val conITU: ConTU[Int] = (x: Int) => println(x) // expanded
    conITU(11)
    val conITI: ConTI[Int] = (x: Int) => x // closure
    println(conITI(12))
    val conITS: ConTS[Int] = (x: Int) => x.toString // closure
    println(conITS(13))
    val conSTS: ConTS[String] = (x: String) => x // closure
    println(conSTS("14"))

    val conIRS: ConIR[String] = (x: Int) => x.toString // expanded
    println(conIRS(15))
    val conIRI: ConIR[Int] = (x: Int) => x // expanded
    println(conIRI(16))
    val conIRU: ConIR[Unit] = (x: Int) => println(x) // expanded
    conIRU(17)

    val conSRI: ConSR[Int] = (x: String) => x.toInt // closure
    println(conSRI("18"))
    val conURI: ConUR[Int] = () => 19 // closure
    println(conURI())
    val conURU: ConUR[Unit] = () => println("20") // closure
    conURU()

    val conII: ConII = (x: Int) => x // expanded
    println(conII(21))
    val conSI: ConSI = (x: String) => x.toInt // closure
    println(conSI("22"))
    val conIS: ConIS = (x: Int) => x.toString // expanded
    println(conIS(23))
    val conUU: ConUU = () => println("24") // expanded
    conUU()
    val conVCVC: ConVCVC = (x: IntVal) => IntVal(x.value + 1) // expanded
    println(conVCVC(IntVal(24)).value)

    val ffIU: F1[Int, Unit] = (x: Int) => println(x) // closure
    ffIU(31)
    val ffIS: F1[Int, String] = (x: Int) => x.toString // closure
    println(ffIS(32))
    val ffSU: F1[String, Unit] = (x: String) => println(x) // closure
    ffSU("33")
    val ffSI: F1[String, Int] = (x: String) => x.toInt // closure
    println(ffSI("34"))

    val sfITU: SFTU[Int] = (x: Int) => println(x) // expanded
    sfITU(41)
    val sfSTU: SFTU[String] = (x: String) => println(x) // expanded
    sfSTU("42")

    val sfITI: SFTI[Int] = (x: Int) => x // closure
    println(sfITI(43))
    val sfSTI: SFTI[String] = (x: String) => x.toInt // closure
    println(sfSTI("44"))

    val sfITS: SFTS[Int] = (x: Int) => x.toString // closure
    println(sfITS(45))
    val sfSTS: SFTS[String] = (x: String) => x // closure
    println(sfSTS("46"))

    val sfIRI: SFIR[Int] = (x: Int) => x // expanded
    println(sfIRI(51))
    val sfIRS: SFIR[String] = (x: Int) => x.toString // expanded
    println(sfIRS(52))
    val sfIRU: SFIR[Unit] = (x: Int) => println(x) // expanded
    sfIRU(53)

    val sfSRI: SFSR[Int] = (x: String) => x.toInt // closure
    println(sfSRI("55"))
    val sfSRS: SFSR[String] = (x: String) => x // closure
    println(sfSRS("56"))
    val sfSRU: SFSR[Unit] = (x: String) => println(x) // closure
    sfSRU("57")

    val sfII: SFII = (x: Int) => x // expanded
    println(sfII(61))
    val sfSI: SFSI = (x: String) => x.toInt // closure
    println(sfSI("62"))
    val sfIS: SFIS = (x: Int) => x.toString // expanded
    println(sfIS(63))
    val sfIU: SFIU = (x: Int) => println(x) // expanded
    sfIU(64)
    val sfVCVC: SFVCVC = (x: IntVal) => IntVal(x.value + 1) // expanded
    println(sfVCVC(IntVal(64)).value)

    val f2ITU: F1U[Int] = (x: Int) => println(x) // closure
    f2ITU(71)
    val f2STU: F1U[String] = (x: String) => println(x) // closure
    f2STU("72")

    val sf2IT: SF2T[Int] = (x: Int) => println(x) // closure
    sf2IT(75)
    val sf2ST: SF2T[String] = (x: String) => println(x) // closure
    sf2ST("76")

    val sf2I: SF2I = (x: Int) => println(x) // expanded
    sf2I(81)
    val sf2S: SF2S = (x: String) => println(x) // closure
    sf2S("82")
  }
}
