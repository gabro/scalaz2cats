/*
rules = [
  "class:fix.RemoveGlobalImports",
  "class:fix.MigrateEqual",
]
*/
package fix

import scalaz._
import scalaz.Scalaz._

object MigrateEqual2Test {

  def equalA[A]: Equal[A] = Equal.equalA[A]

  def test[A](a: A, b: A)(implicit eq: Equal[A]) = eq.equal(a, b)
  def test2[A](a: A, b: A)(implicit eq: Equal[A]) = a === b
  def test3[A](a: A, b: A)(implicit eq: Equal[A]) = a =/= b
  def test4[A](a: A, b: A)(implicit eq: Equal[A]) = a /== b
  def test5[A](a: A, b: A)(implicit eq: Equal[A]) = a ≠ b
  def test6[A](a: A, b: A)(implicit eq: Equal[A]) = a ≟ b
  def test7[A: Equal, B: Equal, C: Equal](a: A, b: A) = a ≟ b

  class Abc[A: Equal]
}
