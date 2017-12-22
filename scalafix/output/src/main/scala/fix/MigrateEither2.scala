package fix

import scalaz._
import Scalaz._
import cats.syntax.either._

object Mylibrary_1_0_Test2 {
  def myMethod(x: Int): Either[String, Int] =
    if (x > 2) Right(x)
    else "nope".asLeft
}
