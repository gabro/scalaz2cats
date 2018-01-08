/*
rule = "class:fix.MigrateValidation"
*/
package fix

import scalaz.{ Success, Failure, Validation }

object Validation_1_0_Test {
  def myMethod(x: Int): Validation[String, Int] =
    if (x > 2) Success(x)
    else Failure("wat")

  def myMethod2(x: Int): Validation[String, Int] = ???

  myMethod(43) match {
    case Success(n) => println(n)
    case Failure(s) => println(s)
  }

  myMethod2(1) match {
    case scalaz.Success(n) => println(n)
    case scalaz.Failure(s) => println(s)
  }
}
