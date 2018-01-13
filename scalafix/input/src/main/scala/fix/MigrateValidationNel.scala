/*
rule = "class:fix.MigrateValidationNel"
*/
package fix

import scalaz.{ Success, Failure, ValidationNel, NonEmptyList }
import scalaz.syntax.validation._
import scalaz.syntax.apply._

object ValidationNel_1_0_Test {
  def myMethod(x: Int): ValidationNel[String, Int] =
    if (x > 2) x.successNel[String]
    else "wat".failureNel[Int]

  def myMethod2(
    r1: ValidationNel[String, Int],
    r2: ValidationNel[String, Int]
  ): ValidationNel[String, Int] =
    (r1 |@| r2)((b1, b2) => b1 + b2)

  def myMethod3(
    r1: ValidationNel[String, Int],
    r2: ValidationNel[String, Int]
  ): ValidationNel[String, Int] =
    (r1 |@| r2 |@| r2 |@| r1 |@| r1)((b1, b2, _, _, _) => b1 + b2)

  def myMethod4(x: Int): ValidationNel[String, Int] =
    if (x > 10) Success(x)
    else if (x > 5) Failure(NonEmptyList("wat"))
    else if (x > 3) Failure(NonEmptyList("wat", "wat"))
    else Failure(NonEmptyList("wat", "wat", "wat"))

  def myMethod5(r1: ValidationNel[String, Int],
                r2: ValidationNel[String, Int]): ValidationNel[String, Int] =
    (r1 |@| r2 |@| r2 |@| r1 |@| r1).apply((b1, b2, _, _, _) => b1 + b2)

  def myMethod6(r1: ValidationNel[String, Int],
                r2: ValidationNel[String, Int]): ValidationNel[String, Int] =
    r1 |@| r2 |@| r2 |@| r1 |@| r1 apply ((b1, b2, _, _, _) => b1 + b2)

  myMethod(43) match {
    case Success(n) => println(n)
    case Failure(s) => println(s)
  }
}
