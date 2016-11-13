package monocle.std

import monocle.MonocleSuite
import monocle.function.Plated._
import monocle.law.discipline.TraversalTests
import monocle.law.discipline.function.{ConsTests, EmptyTests, ReverseTests}


class IListSpec extends MonocleSuite {
  checkAll("IList Reverse ", ReverseTests[List[Char]])
  checkAll("IList Empty", EmptyTests[List[Char]])
  checkAll("IList Cons", ConsTests[List[Char], Char])
  checkAll("IList Snoc", ConsTests[List[Char], Char])

  checkAll("plated IList", TraversalTests(plate[List[Char]]))
}
