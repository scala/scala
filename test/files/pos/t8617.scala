object Test {
  def foo[A] = implicitly[OptManifest[A]] // was "unpositioned tree" under -Yrangepos

  // These did not crash, but testing for good measure.
  implicitly[OptManifest[String]]
  implicitly[Manifest[String]]

  implicitly[reflect.ClassTag[String]]
  implicitly[reflect.runtime.universe.TypeTag[String]]
}
