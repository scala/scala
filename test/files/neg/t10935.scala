
object a {
  var size = 0
  size += 1 + "foo".lengt
}

/*
 *
test/files/neg/t10935.scala:4: error: value += is not a member of Int
  size += 1 + "foo".lengt
       ^
one error found
 */
