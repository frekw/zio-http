package zhttp.http

import zhttp.http.middleware.Web
import zio.clock.Clock
import zio.duration.Duration
import zio._

/**
 * Middlewares are essentially transformations that one can apply on any Http to
 * produce a new one. They can modify requests and responses and also transform
 * them into more concrete domain entities.
 *
 * You can think of middlewares as a functions —
 *
 * {{{
 *   type Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut] = Http[R, EIn, AIn, BIn] => Http[R, EOut, AOut, BOut]
 * }}}
 *
 * The `AIn` and `BIn` type params represent the type params of the input Http.
 * The `AOut` and `BOut` type params represent the type params of the output
 * Http.
 */
trait Middleware[-R, -EIn, +AIn, -BIn, +EOut, -AOut, +BOut] { self =>

  /**
   * Creates a new middleware that passes the output Http of the current
   * middleware as the input to the provided middleware.
   */
  final def >>>[R1 <: R, EIn1 >: EOut, AIn1 <: AOut, BIn1 >: BOut, EOut1, AOut1, BOut1](
    other: Middleware[R1, EIn1, AIn1, BIn1, EOut1, AOut1, BOut1],
  ): Middleware[R1, EIn, AIn, BIn, EOut1, AOut1, BOut1] = self andThen other

  /**
   * Applies self but if it fails, applies other.
   */
  final def <>[R1 <: R, EIn0 <: EIn, AIn0 >: AIn, BIn0 <: BIn, EOut0 >: EOut, AOut0 <: AOut, BOut0 >: BOut](
    other: Middleware[R1, EIn0, AIn0, BIn0, EOut0, AOut0, BOut0],
  ): Middleware[R1, EIn0, AIn0, BIn0, EOut0, AOut0, BOut0] = self orElse other

  /**
   * Combines two middleware that don't modify the input and output types.
   */
  final def ++[R1 <: R, E0 >: EOut, A0 >: AIn <: AOut, B0 >: BOut <: BIn](
    other: Middleware[R1, E0, A0, B0, E0, A0, B0],
  ): Middleware[R1, EIn, A0, B0, E0, A0, B0] =
    self combine other

  /**
   * Composes one middleware with another.
   */
  final def andThen[R1 <: R, EIn1 >: EOut, AIn1 <: AOut, BIn1 >: BOut, EOut1, AOut1, BOut1](
    other: Middleware[R1, EIn1, AIn1, BIn1, EOut1, AOut1, BOut1],
  ): Middleware[R1, EIn, AIn, BIn, EOut1, AOut1, BOut1] =
    new Middleware[R1, EIn, AIn, BIn, EOut1, AOut1, BOut1] {
      override def apply[R2 <: R1](http: Http[R2, EIn, AIn, BIn]): Http[R2, EOut1, AOut1, BOut1] =
        other(self(http))
    }

  /**
   * Applies middleware on Http and returns new Http.
   */
  def apply[R1 <: R](http: Http[R1, EIn, AIn, BIn]): Http[R1, EOut, AOut, BOut]

  /**
   * Makes the middleware resolve with a constant Middleware
   */
  final def as[BOut0](
    bout: BOut0,
  ): Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut0] =
    self.map(_ => bout)

  /**
   * Combines two middleware that operate on the same input and output types,
   * into one.
   */
  final def combine[R1 <: R, E0 >: EOut, A0 >: AIn <: AOut, B0 >: BOut <: BIn](
    other: Middleware[R1, E0, A0, B0, E0, A0, B0],
  ): Middleware[R1, EIn, A0, B0, E0, A0, B0] =
    self andThen other

  /**
   * Preprocesses the incoming value for the outgoing Http.
   */
  final def contramap[AOut0](f: AOut0 => AOut): Middleware[R, EIn, AIn, BIn, EOut, AOut0, BOut] =
    self.contramapZIO[AOut0](a => UIO(f(a)))

  /**
   * Preprocesses the incoming value using a ZIO, for the outgoing Http.
   */
  final def contramapZIO[AOut0]: Middleware.PartialContraMapZIO[R, EIn, AIn, BIn, AOut, EOut, BOut, AOut0] =
    new Middleware.PartialContraMapZIO(self)

  /**
   * Delays the production of Http output for the specified duration
   */
  final def delay(duration: Duration): Middleware[R with Clock, EIn, AIn, BIn, EOut, AOut, BOut] =
    self.mapZIO(b => UIO(b).delay(duration))

  /**
   * Creates a new Middleware from another
   */
  final def flatMap[R1 <: R, EIn0 <: EIn, EOut0 >: EOut, AIn0 >: AIn, BIn0 <: BIn, AOut0 <: AOut, BOut0](
    f: BOut => Middleware[R1, EIn0, AIn0, BIn0, EOut0, AOut0, BOut0],
  ): Middleware[R1, EIn0, AIn0, BIn0, EOut0, AOut0, BOut0] =
    new Middleware[R1, EIn0, AIn0, BIn0, EOut0, AOut0, BOut0] {
      override def apply[R2 <: R1](http: Http[R2, EIn0, AIn0, BIn0]): Http[R2, EOut0, AOut0, BOut0] =
        self(http).flatMap(f(_)(http))
    }

  /**
   * Flattens an Middleware of a Middleware
   */
  final def flatten[R1 <: R, EIn1 <: EIn, AIn0 >: AIn, BIn0 <: BIn, EOut1 >: EOut, AOut0 <: AOut, BOut0](implicit
    ev: BOut <:< Middleware[R1, EIn1, AIn0, BIn0, EOut1, AOut0, BOut0],
  ): Middleware[R1, EIn1, AIn0, BIn0, EOut1, AOut0, BOut0] =
    flatMap(identity(_))

  /**
   * Transforms the output type of the current middleware.
   */
  final def map[BOut0](f: BOut => BOut0): Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut0] =
    self.flatMap(b => Middleware.succeed(f(b)))

  /**
   * Transforms the output type of the current middleware using effect function.
   */
  final def mapZIO[R1 <: R, E1 >: EOut, BOut0](
    f: BOut => ZIO[R1, E1, BOut0],
  ): Middleware[R1, EIn, AIn, BIn, E1, AOut, BOut0] =
    self.flatMap(b => Middleware.fromHttp(Http.fromZIO(f(b))))

  /**
   * Applies self but if it fails, applies other.
   */
  final def orElse[R1 <: R, EIn1, AIn0 >: AIn, BIn0 <: BIn, EOut1 >: EOut, AOut0 <: AOut, BOut0 >: BOut](
    other: Middleware[R1, EIn1, AIn0, BIn0, EOut1, AOut0, BOut0],
  )(implicit ev: EIn1 <:< EIn): Middleware[R1, EIn, AIn0, BIn0, EOut1, AOut0, BOut0] =
    new Middleware[R1, EIn, AIn0, BIn0, EOut1, AOut0, BOut0] {
      override def apply[R2 <: R1](http: Http[R2, EIn, AIn0, BIn0]): Http[R2, EOut1, AOut0, BOut0] =
        self(http) <> other(http.asInstanceOf[Http[R1, EIn1, AIn0, BIn0]])
    }

  /**
   * Race between current and other, cancels other when execution of one
   * completes
   */
  final def race[R1 <: R, EIn1, AIn1 >: AIn, BIn1 <: BIn, EOut1 >: EOut, AOut1 <: AOut, BOut1 >: BOut](
    other: Middleware[R1, EIn1, AIn1, BIn1, EOut1, AOut1, BOut1],
  )(implicit ev: EIn1 <:< EIn): Middleware[R1, EIn, AIn1, BIn1, EOut1, AOut1, BOut1] =
    new Middleware[R1, EIn, AIn1, BIn1, EOut1, AOut1, BOut1] {
      override def apply[R2 <: R1](http: Http[R2, EIn, AIn1, BIn1]): Http[R2, EOut1, AOut1, BOut1] =
        self(http) race other(http.asInstanceOf[Http[R1, EIn1, AIn1, BIn1]])
    }

  final def runAfter[R1 <: R, E1 >: EOut](effect: ZIO[R1, E1, Any]): Middleware[R1, EIn, AIn, BIn, E1, AOut, BOut] =
    self.mapZIO(bOut => effect.as(bOut))

  final def runBefore[R1 <: R, E1 >: EOut](effect: ZIO[R1, E1, Any]): Middleware[R1, EIn, AIn, BIn, E1, AOut, BOut] =
    self.contramapZIO(b => effect.as(b))

  /**
   * Applies Middleware based only if the condition function evaluates to true
   */
  final def when[AOut0 <: AOut](cond: AOut0 => Boolean): Middleware[R, EIn, AIn, BIn, EOut, AOut0, BOut] =
    whenZIO(a => UIO(cond(a)))

  /**
   * Applies Middleware based only if the condition effectful function evaluates
   * to true
   */
  final def whenZIO[R1 <: R, EOut1 >: EOut, AOut0 <: AOut](
    cond: AOut0 => ZIO[R1, EOut1, Boolean],
  ): Middleware[R1, EIn, AIn, BIn, EOut1, AOut0, BOut] =
    Middleware.ifThenElseZIO[AOut0](cond(_))(
      isTrue = _ => self,
      isFalse = _ => Middleware.identity,
    )
}

object Middleware extends Web {

  /**
   * Creates a middleware using specified encoder and decoder
   */
  def codec[A, B]: PartialCodec[A, B] = new PartialCodec[A, B](())

  /**
   * Creates a middleware using specified effectful encoder and decoder
   */
  def codecZIO[A, B]: PartialCodecZIO[A, B] = new PartialCodecZIO[A, B](())

  /**
   * Creates a middleware using specified function
   */
  def collect[A]: PartialCollect[A] = new PartialCollect[A](())

  /**
   * Creates a middleware using specified effect function
   */
  def collectZIO[A]: PartialCollectZIO[A] = new PartialCollectZIO[A](())

  /**
   * Creates a middleware which always fail with specified error
   */
  def fail[E](e: E): Middleware[Any, Any, Nothing, Any, E, Any, Nothing] =
    new Middleware[Any, Any, Nothing, Any, E, Any, Nothing] {
      override def apply[R1 <: Any](http: Http[R1, Any, Nothing, Any]): Http[R1, E, Any, Nothing] =
        Http.fail(e)
    }

  /**
   * Creates a middleware with specified http App
   */
  def fromHttp[R, E, A, B](http: Http[R, E, A, B]): Middleware[R, Any, Nothing, Any, E, A, B] =
    new Middleware[R, Any, Nothing, Any, E, A, B] {
      override def apply[R1 <: R](other: Http[R1, Any, Nothing, Any]): Http[R1, E, A, B] = http
    }

  /**
   * An empty middleware that doesn't do anything
   */
  def identity[E >: Nothing]: Middleware[Any, Any, Nothing, Any, E, Any, Nothing] =
    new Middleware[Any, Any, Nothing, Any, E, Any, Nothing] {
      override def apply[R1 <: Any](http: Http[R1, Any, Nothing, Any]): Http[R1, E, Any, Nothing] =
        http.asInstanceOf[Http[R1, Nothing, Any, Nothing]]
    }

  /**
   * Logical operator to decide which middleware to select based on the
   * predicate.
   */
  def ifThenElse[A]: PartialIfThenElse[A] = new PartialIfThenElse(())

  /**
   * Logical operator to decide which middleware to select based on the
   * predicate effect.
   */
  def ifThenElseZIO[A]: PartialIfThenElseZIO[A] = new PartialIfThenElseZIO(())

  /**
   * Creates a new middleware using transformation functions
   */
  def intercept[A, B]: PartialIntercept[A, B] = new PartialIntercept[A, B](())

  /**
   * Creates a new middleware using effectful transformation functions
   */
  def interceptZIO[A, B]: PartialInterceptZIO[A, B] = new PartialInterceptZIO[A, B](())

  /**
   * Creates a middleware which always succeed with specified value
   */
  def succeed[B](b: B): Middleware[Any, Any, Nothing, Any, Nothing, Any, B] = fromHttp(Http.succeed(b))

  final class PartialCollect[AOut](val unit: Unit) extends AnyVal {
    def apply[R, EIn, AIn, EOut, BIn, BOut](
      f: PartialFunction[AOut, Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut]],
    ): Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut] =
      Middleware.fromHttp(Http.collect[AOut] { case aout if f.isDefinedAt(aout) => f(aout) }).flatten
  }

  final class PartialCollectZIO[AOut](val unit: Unit) extends AnyVal {
    def apply[R, EIn, AIn, EOut, BIn, BOut](
      f: PartialFunction[AOut, ZIO[R, EOut, Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut]]],
    ): Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut] =
      Middleware.fromHttp(Http.collectZIO[AOut] { case aout if f.isDefinedAt(aout) => f(aout) }).flatten
  }

  final class PartialIntercept[A, B](val unit: Unit) extends AnyVal {
    def apply[S, BOut](incoming: A => S)(
      outgoing: (B, S) => BOut,
    ): Middleware[Any, Any, A, B, Nothing, A, BOut] =
      interceptZIO[A, B](a => UIO(incoming(a)))((b, s) => UIO(outgoing(b, s)))
  }

  final class PartialInterceptZIO[A, B](val unit: Unit) extends AnyVal {
    def apply[R, EOut, S, BOut](
      incoming: A => ZIO[R, Option[EOut], S],
    ): PartialInterceptOutgoingZIO[R, EOut, A, S, B] =
      new PartialInterceptOutgoingZIO(incoming)
  }

  final class PartialInterceptOutgoingZIO[-R, +EOut, A, +S, B](val incoming: A => ZIO[R, Option[EOut], S])
      extends AnyVal {
    def apply[R1 <: R, EIn >: EOut, EOut1 >: EIn, BOut](
      outgoing: (B, S) => ZIO[R1, Option[EOut1], BOut],
    ): Middleware[R1, EIn, A, B, EOut1, A, BOut] =
      new Middleware[R1, EIn, A, B, EOut1, A, BOut] {
        override def apply[R2 <: R1](http: Http[R2, EIn, A, B]): Http[R2, EOut1, A, BOut] =
          Http.fromOptionFunction[A] { a =>
            for {
              s <- incoming(a)
              b <- http(a)
              c <- outgoing(b, s)
            } yield c
          }
      }
  }

  val f = Middleware.collect[Request] {
    case Method.OPTIONS -> _ =>
      Middleware.succeed(1)
    case Method.GET -> _     =>
      Middleware.fail("nej")
    case Method.GET -> _     =>
      Middleware.fail("nej")
  }

  val f2 = Middleware.interceptZIO[Int, String](i => ZIO.succeed(i.toString()))
  val f3 = f2 { case (_, _) => ZIO.fail("hi").mapError(Option(_)) }

  val app = Http.fail("foo")
  val e1  = Middleware.intercept[Int, String](_.toString) { case (_, _) => "HI" }

  app @@ e1

  final class PartialCodec[AOut, BIn](val unit: Unit) extends AnyVal {
    def apply[EIn, AIn, EOut, BOut](
      decoder: AOut => Either[EOut, AIn],
      encoder: BIn => Either[EOut, BOut],
    ): Middleware[Any, EIn, AIn, BIn, EOut, AOut, BOut] =
      Middleware.identity.mapZIO((b: BIn) => ZIO.fromEither(encoder(b))).contramapZIO(a => ZIO.fromEither(decoder(a)))
  }

  final class PartialIfThenElse[AOut](val unit: Unit) extends AnyVal {
    def apply[R, EIn, AIn, EOut, BIn, BOut](cond: AOut => Boolean)(
      isTrue: AOut => Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut],
      isFalse: AOut => Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut],
    ): Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut] =
      Middleware
        .fromHttp(Http.fromFunction[AOut] { a => if (cond(a)) isTrue(a) else isFalse(a) })
        .flatten
  }

  final class PartialIfThenElseZIO[AOut](val unit: Unit) extends AnyVal {
    def apply[R, EIn, AIn, EOut, BIn, BOut](cond: AOut => ZIO[R, EOut, Boolean])(
      isTrue: AOut => Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut],
      isFalse: AOut => Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut],
    ): Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut] =
      Middleware
        .fromHttp(Http.fromFunctionZIO[AOut] { a => cond(a).map(b => if (b) isTrue(a) else isFalse(a)) })
        .flatten
  }

  final class PartialCodecZIO[AOut, BIn](val unit: Unit) extends AnyVal {
    def apply[R, EIn, AIn, EOut, BOut](
      decoder: AOut => ZIO[R, EOut, AIn],
      encoder: BIn => ZIO[R, EOut, BOut],
    ): Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut] =
      Middleware.identity.mapZIO(encoder).contramapZIO(decoder)
  }

  final class PartialContraMapZIO[-R, -EIn, +AIn, -BIn, -AOut, +EOut, +BOut, AOut0](
    val self: Middleware[R, EIn, AIn, BIn, EOut, AOut, BOut],
  ) extends AnyVal {
    def apply[R1 <: R, EOut1 >: EOut](
      f: AOut0 => ZIO[R1, EOut1, AOut],
    ): Middleware[R1, EIn, AIn, BIn, EOut1, AOut0, BOut] =
      new Middleware[R1, EIn, AIn, BIn, EOut1, AOut0, BOut] {
        override def apply[R2 <: R1](http: Http[R2, EIn, AIn, BIn]): Http[R2, EOut1, AOut0, BOut] =
          self(http).contramapZIO(a => f(a))
      }
  }
}
