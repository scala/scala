// stretching more flexible try/catch's legs a bit
object o {
  try Integer.parseInt("xxxx") catch { case e => 5 }
  try 5
  try try try 10
  try try try 10 catch { case e => 20 } finally 30
  try try try 10 catch { case e => 20 } finally 30 finally 40
  try try try 10 catch { case e => 20 } finally 30 finally 40 finally 50
  try try try 10 finally 50
}
