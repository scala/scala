import concurrent.duration.{ Deadline, Duration }

class T {
  val d: Duration = Duration.Zero
  d.fromNow
  Deadline.now + d
  Deadline.now - d
}
