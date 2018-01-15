package fix

import cats.Eq
import cats.syntax.eq._

object MigrateEqual2Test {

  def equalA[A]: Eq[A] = Eq.fromUniversalEquals[A]

  def test[A](a: A, b: A)(implicit eq: Eq[A]) = eq.eqv(a, b)
  def test2[A](a: A, b: A)(implicit eq: Eq[A]) = a === b
  def test3[A](a: A, b: A)(implicit eq: Eq[A]) = a =!= b
  def test4[A](a: A, b: A)(implicit eq: Eq[A]) = a =!= b
  def test5[A](a: A, b: A)(implicit eq: Eq[A]) = a =!= b
  def test6[A](a: A, b: A)(implicit eq: Eq[A]) = a === b
  def test7[A: Eq, B: Eq, C: Eq](a: A, b: A) = a === b

  class Abc[A: Eq]
}
