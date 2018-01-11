/*
rules = [
  "class:fix.MigrateOptionSyntax"
  "class:fix.RemoveGlobalImports",
]
*/
package fix

import scalaz._
import Scalaz._

object MigrateOptionSyntax2Test {
  class User(name: String)
  val a: Option[User] = new User("foo").some
  val b: Option[User] = new User("foo").some
}
