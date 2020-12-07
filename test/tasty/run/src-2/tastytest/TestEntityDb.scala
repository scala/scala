package tastytest

/** contains transitive erasure of opaque types: Entity -> Id -> Long */
object TestEntityDb extends Suite("TestEntityDb") {
  import EntityDb._, Ids._, Data._

  test {
    val p = Data.newPerson(Id.nextId)(name = "Sarah", age = 23)

    assert(p.name === "Sarah", "p.name")
    assert(p.age === 23, "p.age")
  }

}
