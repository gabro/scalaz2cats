package fix

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }

object Validation_1_0_Test {
  def myMethod(x: Int): Validated[String, Int] =
    if (x > 2) Valid(x)
    else Invalid("wat")

  def myMethod2(x: Int): Validated[String, Int] = ???

  myMethod(43) match {
    case Valid(n) => println(n)
    case Invalid(s) => println(s)
  }

  myMethod2(1) match {
    case Validated.Valid(n) => println(n)
    case Validated.Invalid(s) => println(s)
  }
}
