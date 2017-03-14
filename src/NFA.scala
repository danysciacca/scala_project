/**
  * Created by Daniele on 08/03/2017.
  */
case class NFA[Q, Stati](trans: (Q, Stati)=> Set[Q], S0:Q, f:Q=>Boolean) {
  def Union[_Q](another: NFA[_Q, Stati])={
  def asNFA = this

  sealed abstract trait dummyS
  object dummyS extends dummyS  //uno stato iniziale dello stato iniziale.

  sealed trait epsi
  object epsi extends epsi

  val _trans:(Either[dummyS, Either[Q, _Q]], Either[Stati, epsi]) => Set[Either[dummyS, Either[Q, _Q]]]={
    case (Left(dummyS), Right(epsi)) => Set(Right(Left(this.q0)), Right(Right(another.q0)))
    case (Right(Left(q1)),Left(_x)) => this.trans(q1,_x) map (q => Right(Left(q)))
    case (Right(Right(q2)),Left(_x)) => another.trans(q2,_x) map (q => Right(Right(q)))
    case _ => Set.empty
  }

  val _f = (q:Either[dummyS,Either[Q, _Q]]) => q match{
      case Right(Left(q1)) => this.f(q1)
      case Right(Right(q2)) => another.f(q2)
      case _ => false
    }
    
}
