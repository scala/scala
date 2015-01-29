object Test {
  locally {
    Sites: Config.driver.Table
  }
}

// This variation worked by avoiding referring to the
// overloaded term `Config.driver` in the parent type of
// Sites