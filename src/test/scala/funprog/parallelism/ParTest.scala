package funprog.parallelism

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object ParTest extends Specification with ScalaCheck {
  import funprog.parallelism.Par._
  val testPool = Executors.newFixedThreadPool(10)

  "unit" in {
    "unit is a function that raises the value into Par" ! prop { a: AnyVal =>
      unit(a)(testPool).get mustEqual a
    }

    "unit evaluates parameters as they are passed in" in {
      unit(throw new RuntimeException("failure!")) must throwA[RuntimeException]
    }
  }

  "lazyUnit" in {
    "lazy unit evaluates parameters lazily" in {
      lazyUnit(throw new RuntimeException("failure!")) must not(throwA[RuntimeException])
    }
  }

  "map2" in {
    "map2 combines two parallel operations" ! prop { (a: Int, b: Int) =>
      map2(unit(a), unit(b))(_ + _)(testPool) mustEqual unit(a + b)(testPool)
    }
  }

  "parMap" in {
    "functionally equivalent to map" ! prop { as: List[Int] =>
      as.map(_ + 1) mustEqual parMap(as)(_ + 1)(testPool).get(10, TimeUnit.SECONDS)
    }
  }

  "parFilter" in {
    "functionally equivalent to filter" ! prop { as: List[Int] =>
      as.filter(_ < 0) mustEqual parFilter(as)(_ < 0)(testPool).get(10, TimeUnit.SECONDS)
    }
  }
}
