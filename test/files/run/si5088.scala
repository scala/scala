object Test extends App {
  val gecos = """|name, office, work number, home number
                 |Joe, New York,,
                 |Mary, Los Angeles, 555-5555,
                 |John,,,
                 |""".stripMargin.lines.toList

  // This will have arrays of size 2
  val nameAndContact = gecos map (", *".r split (_, 2))
  println(nameAndContact map (_.size) mkString ", ")

  // This will have arrays of size 1, 2, 3 and 4
  val data = gecos map (", *".r split (_, 0))
  println(data map (_.size) mkString ", ")

  // This will have arrays of size 4
  val csv = gecos map (", *".r split (_, -1))
  println(csv map (_.size) mkString ", ")
}
