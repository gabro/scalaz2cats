/*
rule = "class:fix.MigrateEither"
*/
package fix

import scalaz._
import Scalaz._

object Mylibrary_1_0_Test2 {
  def myMethod(x: Int): String \/ Int =
    if (x > 2) \/-(x)
    else "nope".left
}
