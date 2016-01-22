package philosophy.finallytagless

import language.higherKinds
import cats.{~>, Monad}

trait Term[F[_[_]],X] { self =>
  def run[M[_] : Monad]( interpreter: Interpreter[F,M] ) : M[X]
  def flatMap[A]( f : X => Term[F,A] ) : Term[F,A] = new Term[F,A] {
    def run[M[_] : Monad]( interpreter: Interpreter[F, M] ): M[A] =
      Monad[M].flatMap( self.run( interpreter ) ){ x => f(x).run( interpreter ) }
  }
  def map[A]( f: X => A ) = flatMap( f andThen { a => Term.pure[F,A](a) }  )
  def embed[G[_[_]]]( implicit E: Embed[F,G] ) : Term[G,X] = new Term[G,X] {
    def run[M[_] : Monad](interpreter: Interpreter[G, M]): M[X] = self.run( E( interpreter ) )
  }
}

object Term {
  def pure[F[_[_]],X]( x:X ) = new Term[F,X] {
    def run[M[_] : Monad](interpreter: Interpreter[F, M]): M[X] = Monad[M].pure( x )
  }
  implicit def monad[F[_[_]]] : Monad[Term[F,?]] = new Monad[Term[F,?]] {
    def flatMap[A, B](fa: Term[F,A])(f: (A) => Term[F,B]): Term[F,B] = fa.flatMap( f )
    def pure[A](x: A): Term[F,A] = Term.pure[F,A]( x )
  }
  def apply[F[_[_]]] : TermBuilder[F] = new TermBuilder[F] {}
}

trait TermBuilder[F[_[_]]] {
  type X[_]
  def apply[A]( f : F[X]=>X[A] ) : Term[F,A] = new Term[F,A] {
    def run[M[_] : Monad](interpreter: Interpreter[F, M]): M[A] =
      interpreter.nt( f.asInstanceOf[F[interpreter.G] => interpreter.G[A]]( interpreter.init ) )
  }
}


trait Embed[F[_[_]],G[_[_]]] {
  def apply[M[_] : Monad]( f: Interpreter[G,M] ) : Interpreter[F,M]
}
trait ~~>[F[_[_]],G[_[_]]] extends Embed[F,G] {
  def embed[M[_] : Monad]( f : Interpreter[G,M] ) : F[M]
  def apply[M[_] : Monad]( f: Interpreter[G,M] ) : Interpreter[F,M] = Interpreter( embed( f ) )
}

object Embed {
  implicit def embedLeft[F[_[_]],G[_[_]]] = new Embed[F,IPair[F,G,?[_]]] {
    def apply[M[_] : Monad](f: Interpreter[IPair[F,G,?[_]],M]): Interpreter[F,M] = Interpreter.leftOf( f )
  }
  implicit def embedRefl[F[_[_]]] = new Embed[F,F] {
    def apply[M[_] : Monad](f: Interpreter[F, M]): Interpreter[F, M] = f
  }
  implicit def embedRight[F[_[_]],G[_[_]],H[_[_]]]( implicit E: Embed[F,H]) = new Embed[F,IPair[G,H,?[_]]] {
    def apply[M[_] : Monad](f: Interpreter[IPair[G,H,?[_]], M]): Interpreter[F, M] = E( Interpreter.rightOf(f) )
  }
}

case class IPair[F[_[_]],G[_[_]],M[_]]( left: Interpreter[F,M], right: Interpreter[G,M] )
trait Interpreter[F[_[_]],M[_]] { self =>
  type G[_]
  def init : F[G]
  def nt : G ~> M
  def andThen[H[_]]( n : M ~> H ) : Interpreter[F,H] =
    InterpreterNT[F,G,H]( init, nt andThen n )
  def and[H[_[_]]]( i: Interpreter[H,M] ) : Interpreter[IPair[F,H,?[_]],M] =
    InterpreterInit[IPair[F,H,?[_]],M]( IPair(self, i ) )
  def apply[A]( f: F[G] => G[A] ) : M[A] = nt( f( init ) )
}
case class InterpreterInit[F[_[_]],M[_]]( init: F[M] ) extends Interpreter[F,M] {
  type G[X] = M[X]
  val nt = new (M ~> M) {
    def apply[A](fa: M[A]): M[A] = fa
  }
}
case class InterpreterNT[F[_[_]],G0[_],M[_]]( init: F[G0], nt: G0 ~> M) extends Interpreter[F,M] {
  type G[X] = G0[X]
}
object Interpreter {
  def apply[F[_[_]],M[_]]( fm : F[M] ) : Interpreter[F,M] = InterpreterInit[F,M]( fm )
  def apply[F[_[_]],H[_],M[_]]( fm : F[H], nt: H ~> M ) : Interpreter[F,M] = InterpreterNT( fm, nt )
  def rightOf[F[_[_]],G[_[_]],M[_]]( i : Interpreter[IPair[F,G,?[_]],M] ) : Interpreter[G,M] = i.init.right andThen i.nt
  def leftOf[F[_[_]],G[_[_]],M[_]]( i : Interpreter[IPair[F,G,?[_]],M] ) : Interpreter[F,M] = i.init.left andThen i.nt
  def pairOf[F[_[_]],G[_[_]],M[_]]( i : Interpreter[IPair[F,G,?[_]],M] ) : (Interpreter[F,M],Interpreter[G,M]) = (Interpreter.leftOf(i),Interpreter.rightOf(i))
}

