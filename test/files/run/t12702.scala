object Test {
  trait MFSS[X <: MFSS[_]]
  trait CS extends MFSS[CS]
  trait MFI { type ST }
  case class MFSD[S](mFI: MFI {type ST = S})
  case object IOS extends MFI { type ST = CS }
  type SD = MFSD[S] forSome {
    type S <: MFSS[S]
  }
  def bad(sd: SD) = sd.mFI match {
    case ios: IOS.type => println(ios)
  }
  def main(args: Array[String]): Unit = {
    val x = MFSD(IOS)
    bad(x)
  }
}
