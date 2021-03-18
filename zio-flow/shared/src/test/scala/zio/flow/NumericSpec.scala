package zio.flow

import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ DefaultRunnableSpec, ZSpec, _ }

object RemoteNumericSpec extends DefaultRunnableSpec {

  val suite1 = suite("Test addition of Remote Int111")(
    test("Basic addition.") {
      assert((Remote(1)+ Remote(2)).eval)(equalTo(Remote(3).eval))
    },
    testM("Associativity of addition.") {
      check(Gen.anyInt, Gen.anyInt, Gen.anyInt) { (int1, int2, int3) =>
        val remoteInt1 = Remote(int1)
        val remoteInt2 = Remote(int2)
        val remoteInt3 = Remote(int3)
        val r1         = ((remoteInt1 + remoteInt2) + remoteInt3).eval
        val r2         = (remoteInt1 + (remoteInt2 + remoteInt3)).eval
        assert(r1)(equalTo(r2))
      }
    },
    testM("Commutativity of addition.") {
      check(Gen.anyInt, Gen.anyInt) { (int1, int2) =>
        val remoteInt1 = Remote(int1)
        val remoteInt2 = Remote(int2)
        val r1         = (remoteInt1 + remoteInt2).eval
        val r2         = (remoteInt2 + remoteInt1).eval
        assert(r1)(equalTo(r2))
      }
    })

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("All tests")(suite1)
}
