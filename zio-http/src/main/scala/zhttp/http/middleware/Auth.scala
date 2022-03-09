package zhttp.http.middleware

import io.netty.handler.codec.http.HttpHeaderNames
import zhttp.http.Headers.BasicSchemeName
import zhttp.http._
import zhttp.http.middleware.Auth.Credentials
import zio.{UIO, ZIO}

private[zhttp] trait Auth {

  /**
   * Creates a middleware for basic authentication
   */
  final def basicAuth[E](f: Credentials => Boolean): HttpMiddleware[Any, E, Nothing] =
    basicAuthZIO(credentials => UIO(f(credentials)))

  /**
   * Creates a middleware for basic authentication using an effectful
   * verification function
   */
  final def basicAuthZIO[R, EIn, EOut](f: Credentials => ZIO[R, EOut, Boolean]): HttpMiddleware[R, EIn, EOut] =
    customAuthZIO(
      _.basicAuthorizationCredentials match {
        case Some(credentials) => f(credentials)
        case None              => UIO(false)
      },
      Headers(HttpHeaderNames.WWW_AUTHENTICATE, BasicSchemeName),
    )

  /**
   * Creates a middleware for basic authentication that checks if the
   * credentials are same as the ones given
   */
  final def basicAuth[E](u: String, p: String): HttpMiddleware[Any, E, Nothing] =
    basicAuth { case credentials => (credentials.uname == u) && (credentials.upassword == p) }

  /**
   * Creates an authentication middleware that only allows authenticated
   * requests to be passed on to the app.
   */
  final def customAuth[E](
    verify: Headers => Boolean,
    responseHeaders: Headers = Headers.empty,
  ): HttpMiddleware[Any, E, Nothing] =
    customAuthZIO(headers => UIO(verify(headers)), responseHeaders)

  /**
   * Creates an authentication middleware that only allows authenticated
   * requests to be passed on to the app using an effectful verification
   * function.
   */
  final def customAuthZIO[R, EIn, EOut](
    verify: Headers => ZIO[R, EOut, Boolean],
    responseHeaders: Headers = Headers.empty,
  ): HttpMiddleware[R, EIn, EOut] =
    Middleware.ifThenElseZIO[Request](req => verify(req.headers))(
      _ => Middleware.identity,
      _ => Middleware.fromHttp(Http.status(Status.FORBIDDEN).addHeaders(responseHeaders)),
    )
}

object Auth {
  case class Credentials(uname: String, upassword: String)
}
