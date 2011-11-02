object Test {
  def main(args: Array[String]): Unit = {

    // SI-4981: avoid being limited by math context when not needed
    val big = BigDecimal("32432875832753287583275382753288325325328532875325")
    val f = big % BigDecimal(scala.math.Pi)

    // SI-1812: use math context to limit decimal expansion
    val a = BigDecimal(1) / BigDecimal(3)
    val b = BigDecimal(1) / big

    // SI-2199: implicit conversions from java.math.BigDecimal to BigDecimal
    val c = BigDecimal(1) + (new java.math.BigDecimal(3))

    // SI-2024: correctly use BigDecimal.valueOf
    assert(BigDecimal(123) + 1.1 == BigDecimal("124.1"))

    // SI-3206: BigDecimal cache errors
    val d = BigDecimal(2, new java.math.MathContext(33))
    val e = BigDecimal(2, new java.math.MathContext(34))
    assert(d.mc != e.mc)

    // SI-921
    assert(BigDecimal(2) / BigDecimal(0.5) == BigDecimal(4))

    // SI-2304: enforce equals/hashCode contract
    assert(BigDecimal("2").hashCode == BigDecimal("2.00").hashCode)

    // SI-4547: implicit conversion
    assert(5 + BigDecimal(3) == BigDecimal(8))
    
    // meaningless sanity check
    List[BigDecimal](a, b, c, d, e, f) map (_.scale) foreach println
  }
}
