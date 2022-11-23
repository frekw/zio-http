package zio.http.netty

import zio._
import zio.test._

import zio.http._
import zio.http.model._
import zio.test.TestAspect.{timeout}
import zio.http.netty.client.ConnectionPool

object FiberRefSpec extends ZIOSpecDefault with HExitAssertion {
  val config = ServerConfig.default.port(0)
  val ref    = ZLayer.fromZIO(FiberRef.make(0))

  val app: HttpApp[FiberRef[Int], Throwable] = Http.collectZIO[Request] { case Method.GET -> !! =>
    for {
      ref   <- ZIO.service[FiberRef[Int]]
      value <- ref.get
    } yield Response.text(value.toString)
  }

  def spec = suite("FiberRefSpec")(
    test("it propages the value") {
      for {
        ref  <- ZIO.service[FiberRef[Int]]
        port <- Server.install(app)
        _    <- ref.set(10)
        res  <- Client.request(s"http://localhost:$port").flatMap(_.body.asString)
      } yield assertTrue(res == "10")
    }.provideSome[Server & Client](Scope.default, ZLayer.fromZIO(FiberRef.make(0))),
  ).provideShared(
    Scope.default,
    ServerConfig.live(config),
    Server.live,
    ClientConfig.default,
    ConnectionPool.disabled,
    Client.live,
  ) @@ timeout(20.seconds)
}
