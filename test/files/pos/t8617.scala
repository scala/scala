object Test {
  implicitly[reflect.ClassTag[String]]
  implicitly[reflect.runtime.universe.TypeTag[String]]
}
