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

  def isEven(i: Int) = i % 2 == 0
  def times2(i: Int) = i * 2

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

  "drop removes items from the front of a list" in {
    drop(List2(), 100) mustEqual List2()
    drop(List2(1, 2, 3), 2) mustEqual List2(3)
  }

  "init drops the last element in a list" in {
    init(List2(1)) mustEqual List2()
    init(List2(1, 2, 3)) mustEqual List2(1, 2)
  }

  "map takes a List2[A] and a function from A => B and returns a List2[B]" in {
    List2.map(empty)(times2) mustEqual empty
    List2.map(List2(1, 2, 3))(times2) mustEqual List2(2, 4, 6)
  }

  "append takes two lists and creates a single list of the combined elements l1 + l2" in {
    append(empty, empty) mustEqual empty
    append(List2(1, 2, 3), List2(4, 5, 6)) mustEqual List2(1, 2, 3, 4, 5, 6)
  }

  "filter takess a predicate function only keeps the elements that return true" in {
    filter(empty)(isEven) mustEqual empty
    filter(List2(1, 2, 3))(isEven) mustEqual List2(2)
    filter(List2(1, 3, 5))(isEven) mustEqual empty
    filter(List2(2, 4, 6))(isEven) mustEqual List2(2, 4, 6)
  }

  "dropWhile removes elements from the head of the list using a predicate function until it returns false" in {
    dropWhile(empty, isEven) mustEqual empty
    dropWhile(List2(1, 3, 5), isEven) mustEqual List2(1, 3, 5)
    dropWhile(List2(2, 4, 6), isEven) mustEqual empty
    dropWhile(List2(2, 4, 6, 7), isEven) mustEqual List2(7)
  }

  "concat 'flattens' a list of lists by appending them all into a single list" in {
    concat(List2(List2(empty), List2(empty), List2(empty))) mustEqual List2(empty, empty, empty)
    concat(List2(List2(1), List2(2), List2(3))) mustEqual List2(1, 2, 3)
  }

  "folding" in {
    def plus(a: Int, b: Int) = a + b
    def minus(a: Int, b: Int) = a - b

    "foldRight takes an initial value, and a merging function, and returns a final result" in {
      foldRight(empty, 0)(plus) mustEqual 0
      foldRight(List2(1, 2, 3), 0)(plus) mustEqual (1 + (2 + (3 + 0)))

      foldRight(empty, 0)(minus) mustEqual 0
      foldRight(List2(1, 2, 3), 0)(minus) mustEqual (1 - (2 - (3 - 0))) // 2
    }

    "foldLeft reduces from right to left" in {
      foldLeft(empty, 0)(plus) mustEqual 0
      foldLeft(List2(1, 2, 3), 0)(plus) mustEqual (0 + (3 + (2 + 1)))

      foldLeft(empty, 0)(minus) mustEqual 0
      foldLeft(List2(1, 2, 3), 0)(minus) mustEqual (((0 - 3) - 2) - 1) // -6

      foldLeft(List2(1, 2, 3), 0)((a, b) => minus(b, a)) mustEqual foldRight(List2(1, 2, 3), 0)(minus)
    }
  }

  "reverse flips the order of a list" in {
    reverse(empty) mustEqual empty
    reverse(List2(1, 2, 3)) mustEqual List2(3, 2, 1)
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
