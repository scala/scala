//> abusing options -Vdebug -Xverify
class C {
  type Level1 = Tuple4[Unit, Unit, Unit, Unit]
  type Level2 = Tuple4[Level1, Level1, Level1, Level1]
  type Level3 = Tuple4[Level2, Level2, Level2, Level2]
  type Level4 = Tuple4[Level3, Level3, Level3, Level3]
  type Level5 = Tuple4[Level4, Level4, Level4, Level4]
  type Level6 = Tuple4[Level5, Level5, Level5, Level5]
  type Level7 = Tuple4[Level6, Level6, Level6, Level6]

  def crash(x: Level6): Unit = ???
}
