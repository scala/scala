object CatchAll {
  try { "warn" } catch { case _ => }

  try { "warn" } catch { case x => }

  try { "warn" } catch { case _: RuntimeException => ; case x => }

  val t = T

  try { "okay" } catch { case T => }

  try { "okay" } catch { case `t` => }

  try { "okay" } catch { case x @ T => }

  try { "okay" } catch { case x @ `t` => }

  try { "okay" } catch { case _: Throwable => }

  try { "okay" } catch { case _: Exception => }

  try { "okay" } catch { case okay: Throwable => }

  try { "okay" } catch { case okay: Exception => }

  try { "okay" } catch { case _ if "".isEmpty => }

  "okay" match { case _ => "" }
}

object T extends Throwable
