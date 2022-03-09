package zhttp.http.middleware

import zhttp.http._
import zhttp.http.headers.HeaderModifier
import zhttp.http.middleware.Web.{PartialInterceptPatch, PartialInterceptZIOPatch}
import zio.clock.Clock
import zio.console.Console
import zio.duration.Duration
import zio.{UIO, ZIO, clock, console}

import java.io.IOException

/**
 * Middlewares on an HttpApp
 */
private[zhttp] trait Web extends Cors with Csrf with Auth with HeaderModifier[HttpMiddleware[Any, Nothing, Nothing]] {
  self =>

  /**
   * Updates the provided list of headers to the response
   */
  final override def updateHeaders(update: Headers => Headers): HttpMiddleware[Any, Nothing, Nothing] =
    Middleware.updateResponse(_.updateHeaders(update))

  /**
   * Sets cookie in response headers
   */
  final def addCookie[EIn](cookie: Cookie): HttpMiddleware[Any, Nothing, Nothing] =
    self.withSetCookie(cookie)

  final def addCookieZIO[R, EIn, EOut](cookie: ZIO[R, EOut, Cookie]): HttpMiddleware[R, EIn, EOut] =
    patchZIO(_ => cookie.mapBoth(Option(_), c => Patch.addHeader(Headers.setCookie(c))))

  /**
   * Add log status, method, url and time taken from req to res
   */
  final def debug: HttpMiddleware[Console with Clock, IOException, IOException] =
    interceptZIOPatch(req => zio.clock.nanoTime.map(start => (req.method, req.url, start))) {
      case (response, (method, url, start)) =>
        for {
          end <- clock.nanoTime
          _   <- console
            .putStrLn(s"${response.status.asJava.code()} ${method} ${url.encode} ${(end - start) / 1000000}ms")
            .mapError(Option(_))
        } yield Patch.empty
    }

  /**
   * Logical operator to decide which middleware to select based on the header
   */
  final def ifHeaderThenElse[R, EIn, EOut](
    cond: Headers => Boolean,
  )(left: HttpMiddleware[R, EIn, EOut], right: HttpMiddleware[R, EIn, EOut]): HttpMiddleware[R, EIn, EOut] =
    Middleware.ifThenElse[Request](req => cond(req.headers))(_ => left, _ => right)

  /**
   * Logical operator to decide which middleware to select based on the method.
   */
  final def ifMethodThenElse[R, EIn, EOut](
    cond: Method => Boolean,
  )(left: HttpMiddleware[R, EIn, EOut], right: HttpMiddleware[R, EIn, EOut]): HttpMiddleware[R, EIn, EOut] =
    Middleware.ifThenElse[Request](req => cond(req.method))(_ => left, _ => right)

  /**
   * Logical operator to decide which middleware to select based on the
   * predicate.
   */
  final def ifRequestThenElse[R, EIn, EOut](
    cond: Request => Boolean,
  )(left: HttpMiddleware[R, EIn, EOut], right: HttpMiddleware[R, EIn, EOut]): HttpMiddleware[R, EIn, EOut] =
    Middleware.ifThenElse[Request](cond)(_ => left, _ => right)

  /**
   * Logical operator to decide which middleware to select based on the
   * predicate.
   */
  final def ifRequestThenElseZIO[R, EIn, EOut](
    cond: Request => ZIO[R, EOut, Boolean],
  )(left: HttpMiddleware[R, EIn, EOut], right: HttpMiddleware[R, EIn, EOut]): HttpMiddleware[R, EIn, EOut] =
    Middleware.ifThenElseZIO[Request](cond)(_ => left, _ => right)

  /**
   * Creates a new middleware using transformation functions
   */
  final def interceptPatch[S](req: Request => S): PartialInterceptPatch[S] = PartialInterceptPatch(req)

  /**
   * Creates a new middleware using effectful transformation functions
   */
  final def interceptZIOPatch[R, EIn, EOut, S](
    req: Request => ZIO[R, Option[EOut], S],
  ): PartialInterceptZIOPatch[R, EIn, EOut, S] =
    PartialInterceptZIOPatch(req)

  /**
   * Creates a middleware that produces a Patch for the Response
   */
  final def patch[R, EIn](f: Response => Patch): HttpMiddleware[R, EIn, Nothing] =
    Middleware.interceptPatch(_ => ())((res, _) => f(res))

  /**
   * Creates a middleware that produces a Patch for the Response effectfully.
   */
  final def patchZIO[R, EIn, EOut](f: Response => ZIO[R, Option[EOut], Patch]): HttpMiddleware[R, EIn, EOut] =
    Middleware.interceptZIOPatch(_ => ZIO.unit)((res, _) => f(res))

  /**
   * Runs the effect after the middleware is applied
   */
  final def runAfter[R, EIn, EOut](effect: ZIO[R, EOut, Any]): HttpMiddleware[R, EIn, EOut] =
    Middleware.interceptZIO[Request, Response](_ => ZIO.unit)((res, _) => effect.mapBoth(Option(_), _ => res))

  /**
   * Runs the effect before the request is passed on to the HttpApp on which the
   * middleware is applied.
   */
  final def runBefore[R, EIn, EOut](effect: ZIO[R, EOut, Any]): HttpMiddleware[R, EIn, EOut] =
    Middleware.interceptZIOPatch(_ => effect.mapError(Option(_)).unit)((_, _) => UIO(Patch.empty))

  /**
   * Creates a new middleware that always sets the response status to the
   * provided value
   */
  final def setStatus(status: Status): HttpMiddleware[Any, Nothing, Nothing] = patch(_ => Patch.setStatus(status))

  /**
   * Creates a middleware for signing cookies
   */
  final def signCookies(secret: String): HttpMiddleware[Any, Nothing, Nothing] =
    updateHeaders {
      case h if h.header(HeaderNames.setCookie).isDefined =>
        Headers(
          HeaderNames.setCookie,
          Cookie.decodeResponseCookie(h.header(HeaderNames.setCookie).get._2.toString).get.sign(secret).encode,
        )
      case h                                              => h
    }

  /**
   * Times out the application with a 408 status code.
   */
  final def timeout(duration: Duration): HttpMiddleware[Clock, Nothing, Nothing] =
    Middleware.identity.race(Middleware.fromHttp(Http.status(Status.REQUEST_TIMEOUT).delayAfter(duration)))

  /**
   * Creates a middleware that updates the response produced
   */
  final def updateResponse[R, EIn, EOut](f: Response => Response): HttpMiddleware[R, EIn, EOut] =
    Middleware.intercept[Request, Response](_ => ())((res, _) => f(res))

  /**
   * Applies the middleware only when the condition for the headers are true
   */
  final def whenHeader[R, EIn, EOut](
    cond: Headers => Boolean,
    middleware: HttpMiddleware[R, EIn, EOut],
  ): HttpMiddleware[R, EIn, EOut] =
    middleware.when[Request](req => cond(req.headers))

  /**
   * Applies the middleware only if the condition function evaluates to true
   */
  final def whenRequest[R, EIn, EOut](cond: Request => Boolean)(
    middleware: HttpMiddleware[R, EIn, EOut],
  ): HttpMiddleware[R, EIn, EOut] =
    middleware.when[Request](cond)

  /**
   * Applies the middleware only if the condition function effectfully evaluates
   * to true
   */
  final def whenRequestZIO[R, EIn, EOut](
    cond: Request => ZIO[R, EOut, Boolean],
  )(middleware: HttpMiddleware[R, EIn, EOut]): HttpMiddleware[R, EIn, EOut] =
    Middleware.ifThenElseZIO[Request](cond)(
      _ => middleware,
      _ => Middleware.identity,
    )
}

object Web {
  final case class PartialInterceptPatch[S](req: Request => S) extends AnyVal {
    def apply(res: (Response, S) => Patch): HttpMiddleware[Any, Nothing, Nothing] = {
      Middleware.intercept[Request, Response](req(_))((response, state) => res(response, state)(response))
    }
  }

  final case class PartialInterceptZIOPatch[R, EIn, EOut, S](req: Request => ZIO[R, Option[EOut], S]) extends AnyVal {
    def apply[R1 <: R](res: (Response, S) => ZIO[R1, Option[EOut], Patch]): HttpMiddleware[R1, EIn, EOut] =
      Middleware
        .interceptZIO[Request, Response](req(_))((response, state) =>
          res(response, state).map(patch => patch(response)),
        )
  }
}
