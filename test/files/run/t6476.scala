object Test {
  def main(args: Array[String]): Unit = {
    val person = "Alice"
    println(s"\"Hello\", $person")
    println(s"""\"Hello\", $person""")

    println(f"\"Hello\", $person")
    println(f"""\"Hello\", $person""")

    println(raw"\"Hello\", $person")
    println(raw"""\"Hello\", $person""")

    println(s"\\TILT\\")
    println(f"\\TILT\\")
    println(raw"\\TILT\\")

    println(s"""\\TILT\\""")
    println(f"""\\TILT\\""")
    println(raw"""\\TILT\\""")

    println(raw"""\TILT\""")
  }
}
