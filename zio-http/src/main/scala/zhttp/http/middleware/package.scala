package zhttp.http

package object middleware {
  type HttpMiddleware[-R, -EIn, +EOut] = Middleware[R, EIn, Request, Response, EOut, Request, Response]
}
