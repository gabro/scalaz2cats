package fix

import mouse.boolean._

object MigrateBooleanTest {
  val res: Option[Int] = true.option(12)
  val res3: Either[String, Int] = true either("abc", 12)
  val res4: Option[String] = true option "abc"
  val res5: String = {
    true fold("abc", "ww")
  }
  val res6: String = List().isEmpty fold("abc", "ww")
}
