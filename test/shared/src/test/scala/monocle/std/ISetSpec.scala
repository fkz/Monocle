package monocle.std

import monocle.MonocleSuite
import monocle.law.discipline.function.{AtTests, EmptyTests}

class ISetSpec extends MonocleSuite {
  checkAll("at ISet", AtTests[Set[Int], Int, Boolean])
  checkAll("empty ISet", EmptyTests[Set[Int]])
}
