// Distinct type members of the same name are still the same type
trait P { type T; def mkP: T }
trait Q { type T; def useQ(t: T) = t.toString }
trait Y { self: P with Q =>
  def run = useQ(mkP)
}
