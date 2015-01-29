object Test {
  locally {
    Sites: Config.driver.Table
  }
}

// Under separate compilation, the pickler is foiled by the 
// overloaded term `Config.driver`, and results in:

// qbin/scalac test/files/pos/t7596/A_1.scala && qbin/scalac -explaintypes test/files/pos/t7596/B_2.scala
// test/files/pos/t7596/B_2.scala:3: error: type mismatch;
//  found   : Sites.type
//  required: Config.driver.Table
//     Sites: Config.driver.Table
//     ^
// Sites.type <: Config.driver.Table?
//   Driver.this.type = Config.driver.type?
//   false
// false