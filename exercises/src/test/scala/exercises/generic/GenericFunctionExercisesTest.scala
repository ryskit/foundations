package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0, 1).swap == Pair(1, 0))
    assert(Pair("John", "Doe").swap == Pair("Doe", "John"))
  }

  test("Pair map") {
    assert(Pair("John", "Doe").map(_.length) == Pair(4, 3))
  }

  test("Pair decoded") {
    val tmp = secret.map(_.reverse)
    assert(tmp == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    assert((isEven && isPositive)(12))
    assert(!(isEven && isPositive)(11))
    assert(!(isEven && isPositive)(-4))
    assert(!(isEven && isPositive)(-7))
  }

  test("Predicate && PBT") {
    forAll { (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)

      assert((p1 && Predicate.True)(value) == p1(value))
      assert(!(p1 && Predicate.False)(value))
    }
  }

  test("Predicate ||") {
    assert((isEven || isPositive)(12))
    assert((isEven || isPositive)(11))
    assert((isEven || isPositive)(-4))
    assert(!(isEven || isPositive)(-7))
  }

  test("Predicate || PBT") {
    forAll { (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)
      assert((p1 || Predicate.False)(value) == p1(value))
      assert((p1 || Predicate.True)(value))
    }
  }

  test("Predicate flip") {
    assert(isPositive.flip(5) == false)
    assert(isPositive.flip(-5) == true)
  }

  test("Predicate isValidUser") {
    assert(isValidUser(User("John", 20)) == true)
    assert(isValidUser(User("John", 17)) == false)
    assert(isValidUser(User("john", 20)) == false)
    assert(isValidUser(User("x", 23)) == false)
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(userIdDecoder.decode("-1") == UserId(-1))
    assert(Try(userIdDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder UserId round-trip") {
    forAll { (number: Int) =>
      val json = number.toString
      assert(userIdDecoder.decode(json) == UserId(number))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assert(Try(localDateDecoder.decode("2020-03-26")).isFailure)
    assert(Try(localDateDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder LocalDate round-trip") {
    forAll { (localDate: LocalDate) =>
      val json = "\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(localDate) + "\""
      assert(localDateDecoder.decode(json) == localDate)
    }
  }

  val genLocalDate: Gen[LocalDate] =
    Gen
      .choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
      .map(LocalDate.ofEpochDay)

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] =
    Arbitrary(genLocalDate)

  test("JsonDecoder weirdLocalDateDecoder") {
    forAll { (localDate: LocalDate) =>
      val json1 = "\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(localDate) + "\""
      val json2 = localDate.toEpochDay.toString
      assert(weirdLocalDateDecoder.decode(json1) == localDate)
      assert(weirdLocalDateDecoder.decode(json2) == localDate)
    }
  }

}
