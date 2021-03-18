package zio.flow

import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ DefaultRunnableSpec, ZSpec, _ }

object RemoteNumericSpec extends DefaultRunnableSpec {

  val suite1: Spec[Any,TestFailure[Nothing],TestSuccess] = suite("RemoteIntAddition")(test("Test addition of Remote") {
    val result = (Remote(1) + Remote(2)).eval
    assert(result)(isRight(equalTo(3)))
  })

  //val suite2 = suite("Associativity of addition")(test("Associativity of addition"){
  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("All tests")(suite1)
}
