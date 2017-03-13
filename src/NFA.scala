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
    }
  }
}
