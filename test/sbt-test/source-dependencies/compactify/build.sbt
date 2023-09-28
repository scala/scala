// apparently Travis CI stopped allowing long file names
// it fails with the default setting of 255 characters so
// we have to set lower limit ourselves
scalacOptions ++= Seq("-Xmax-classfile-name", "240")
