object Test {
  val x: Byte = Iterator.empty.next
    // error: polymorphic expression cannot be instantiated to expected type;
    // found   : [T]()T
    // required: Byte
    // val x: Byte = Iterator.empty.next
    //                              ^
}
