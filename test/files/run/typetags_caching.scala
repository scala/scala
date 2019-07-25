object Test {

  def materializeTag = reflect.runtime.universe.typeTag[Option[String]]

  def materializeTagBinder[T: reflect.runtime.universe.TypeTag] = reflect.runtime.universe.typeTag[Option[T]]

  def main(args: Array[String]): Unit = {
    val tag1 = materializeTag
    val tag2 = materializeTag
    assert(tag1 eq tag2) // materialized TypeTags are now cached
    assert(tag1.tpe eq tag2.tpe) // TypeTags themselves have always cached the created Type in a lazy val.

    assert(materializeTagBinder[String] ne materializeTagBinder[Object]) // type creators that splice bound types aren't cacheable.
  }
}
