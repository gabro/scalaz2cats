package fix

import org.scalatest.FunSuiteLike
import scalafix.testkit._

class Mylibrary_Tests extends AbstractSemanticRuleSuite with FunSuiteLike {
  runAllTests()
}
