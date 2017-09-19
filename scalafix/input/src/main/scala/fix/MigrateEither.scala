/*
rule = "class:fix.MigrateEither"
*/
package fix

import scalaz.{ \/, -\/, \/- }
import scalaz.syntax.either._

object Mylibrary_1_0_Test {
  def myMethod(x: Int): String \/ Int =
    if (x > 2) \/-(x)
    else "nope".left

  myMethod(43) match {
    case \/-(n) => println(n)
    case -\/(s) => println(s)
  }
}
