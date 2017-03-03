package scala

trait DeprecatedOverriding {
  @deprecatedOverriding val x = 1
}

class COverride extends DeprecatedOverriding {
  override val x = 2
}

class CSynthImpl extends DeprecatedOverriding