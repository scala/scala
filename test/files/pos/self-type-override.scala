trait TCommon {
  def f: String
}

class C1 extends TCommon {
  def f = "in C1"
}

trait TOverrider { this: TCommon =>
  override def f = "in TOverrider"   // The overridden self-type member...
}

class C2 extends C1 with TOverrider  // ... fails to override, here.
