/*
rules = [
  "class:fix.MigrateEither",
  "class:fix.RemoveGlobalImports",
]
*/
package fix

import scalaz._
import Scalaz._
import scalaz.Scalaz._

object Mylibrary_1_0_Test2 {
  def myMethod(x: Int): String \/ Int =
    if (x > 2) x.right
    else "nope".left
}
