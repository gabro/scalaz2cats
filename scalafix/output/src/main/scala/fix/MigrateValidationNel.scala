package fix

import cats.syntax.validated._
import cats.syntax.apply._
import cats.data.{ NonEmptyList, ValidatedNel }
import cats.data.Validated.{ Invalid, Valid }

object ValidationNel_1_0_Test {
  def myMethod(x: Int): ValidatedNel[String, Int] =
    if (x > 2) x.validNel[String]
    else "wat".invalidNel[Int]

  def myMethod2(
    r1: ValidatedNel[String, Int],
    r2: ValidatedNel[String, Int]
  ): ValidatedNel[String, Int] =
    (r1, r2).mapN((b1, b2) => b1 + b2)

  def myMethod3(
    r1: ValidatedNel[String, Int],
    r2: ValidatedNel[String, Int]
  ): ValidatedNel[String, Int] =
    (r1, r2, r2, r1, r1).mapN((b1, b2, _, _, _) => b1 + b2)

  def myMethod4(x: Int): ValidatedNel[String, Int] =
    if (x > 10) Valid(x)
    else if (x > 5) Invalid(NonEmptyList.of("wat"))
    else if (x > 3) Invalid(NonEmptyList.of("wat", "wat"))
    else Invalid(NonEmptyList.of("wat", "wat", "wat"))

  def myMethod5(r1: ValidatedNel[String, Int],
                r2: ValidatedNel[String, Int]): ValidatedNel[String, Int] =
    (r1, r2, r2, r1, r1).mapN((b1, b2, _, _, _) => b1 + b2)

  def myMethod6(r1: ValidatedNel[String, Int],
                r2: ValidatedNel[String, Int]): ValidatedNel[String, Int] =
    (r1, r2, r2, r1, r1) mapN ((b1, b2, _, _, _) => b1 + b2)

  myMethod(43) match {
    case Valid(n) => println(n)
    case Invalid(s) => println(s)
  }
}
