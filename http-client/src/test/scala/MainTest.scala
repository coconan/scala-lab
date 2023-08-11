import org.scalatest.funsuite.AnyFunSuite

class MainTest extends AnyFunSuite {
  test("http-client should be able to get") {
    assert(Main.get().body.right.get.size > 0)
  }
}
