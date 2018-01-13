package fix

import cats.syntax.option._

object MigrateOptionSyntaxTest {
  class User(name: String)
  val a: Option[User] = new User("foo").some
  val ab: Int = 10.some getOrElse 10
  none[Int]
}
