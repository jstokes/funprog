package funprog.datastructures

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object ListTest extends Specification with ScalaCheck {
  import List2._
  lazy val empty = List2[Int]()

  "apply should create a list" in {
    List2[Int]() mustEqual Nil
    List2(1) mustEqual Cons(1, Nil)
    List2(1, 2, 3) mustEqual Cons(1, Cons(2, Cons(3, Nil)))
  }

  "len of empty list is 0" in {
    len(empty) mustEqual 0
  }

  "len of a one element list is 1" in {
    len(List2(1)) mustEqual 1
  }

  "tail on an empty lists results in an exception" in {
    tail(List2()) must throwA[UnsupportedOperationException]
  }

  "set head changes the head of a list without changing the tail" in {
    setHead(empty, 3) must throwA[UnsupportedOperationException]
    setHead(List2(1), 3) mustEqual List2(3)
    setHead(List2(2, 2, 3), 1) mustEqual List2(1, 2, 3)
  }

  implicit def arbList: Arbitrary[List2[Int]] = Arbitrary(
    arbitrary[List[Int]].map(l => List2(l: _*))
  )
  def nonEmptyList = arbitrary[List2[Int]] suchThat (l => len(l) > 0)

  "obey properties" in {
    "length of a n element n is n" ! prop { as: List[Int] =>
      as.length == len(List2(as: _*))
    }

    "the length of the tail is 1 less than the length of the list" !
      forAll(nonEmptyList) { as: List2[Int] =>
        len(as) == 1 + len(tail(as))
      }

    "the tail of a list is the list without it's head" ! prop { as: List2[Int] =>
      as match {
        case Cons(h, t) => tail(as) == t
        case Nil => true
      }
    }

    "setHead can change the head without changing the tail" ! forAll(nonEmptyList) { as: List2[Int] =>
      setHead(as, 3) match {
        case Cons(h, t) => h == 3 && t == tail(as)
        case _ => false
      }
    }

    "drop(as, n) should result in a list that is shorter when n > 0" ! prop { (n: Int, as: List2[Int]) =>
      drop(as, n) match {
        case Nil => n >= len(as) || as == Nil
        case l if n > 0 => len(as) == n + len(l)
        case l if n <= 0 => l == as
      }
    }

    "drop should remove from the start of the list" ! forAll(nonEmptyList) { as: List2[Int] =>
      drop(as, 1) match {
        case Nil => len(as) == 1
        case l => l == tail(as)
      }
    }
  }
}
