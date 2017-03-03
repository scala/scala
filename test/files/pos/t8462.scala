
trait ConstantOps {
  def exprs = (
    1   << 2L : Int,  // was: error: type mismatch; found   : Long(4L)
    64  >> 2L : Int,  // was: error: type mismatch; found   : Long(4L)
    64 >>> 2L : Int,  // was: error: type mismatch; found   : Long(4L)
    'a' << 2L : Int,
    'a' >> 2L : Int,
    'a'>>> 2L : Int
  )
}
