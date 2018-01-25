/*
rules = [
  "class:fix.MigrateEither",
  "class:fix.MigrateBoolean",
]
*/
package fix

import scalaz.\/
import scalaz.syntax.std.boolean._

object MigrateBooleanTest {
  val res: Option[Int] = true.option(12)
  val res3: \/[String, Int] = true either 12 or "abc"
  val res4: Option[String] = true option "abc"
  val res5: String = {
    true ? "abc" | "ww"
  }
  val res6: String = List().isEmpty ? "abc" | "ww"
}
