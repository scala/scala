//> using options -Xlint:deprecation,eta-zero,eta-sam

trait AcciSamZero { def apply(): Int }

@FunctionalInterface
trait SamZero { def apply(): Int }

trait AcciSamOne { def apply(i: Int): Int }

@FunctionalInterface
trait SamOne { def apply(i: Int): Int }

class EtaExpand214 {
  def m2() = 1
  def m3(x: Int) = x

  val t2: () => Any  = m2         // eta-expanded with lint warning
  val t2AcciSam: AcciSamZero = m2 // error, nilary methods don't eta-expand to SAM types under -Xsource:3
  val t2Sam: SamZero = m2         // error, nilary methods don't eta-expand to SAM types under -Xsource:3

  val t3: Int => Any = m3         // no warn
  val t3AcciSam: AcciSamOne = m3  // warn
  val t3Sam: SamOne = m3          // no warn
}
