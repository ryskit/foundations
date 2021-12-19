package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits only returns numbers") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  test("secret example") {
    assert(secret("Welcome123") == "**********")
  }

  test("secret PBT") {
    forAll { (text: String) =>
      val once  = secret(text)
      val twice = secret(secret(text))
      assert(once == twice)
    }
  }

  test("isValidUsername") {
    forAll { (username: String) =>
      assert(isValidUsername(username.reverse) == isValidUsername(username))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("Point isPositive") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("Point isEven") {
    forAll { (x: Int, y: Int, z: Int) =>
      val even: (Int) => Int = (a: Int) => if (a % 2 == 0) a else a + 1
      assert(Point(even(x), even(y), even(z)).isEven)
    }
  }

  test("Point forAll") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      Point(x, y, z).forAll(predicate) == List(x, y, z).forall(predicate)
    }
  }
}
