/**
  * Created by Daniele on 08/03/2017.
  */
case class NFA[Q, Stati](trans: (Q, Stati)=> Set[Q], S0:Q, f:Q=>Boolean) {
  def Union[_Q](another: NFA[_Q, Stati])= {
    def asNFA = this

    sealed abstract trait dummyS
    object dummyS extends dummyS //uno stato iniziale dello stato iniziale.

    sealed trait epsi
    object epsi extends epsi

    val _trans: (Either[dummyS, Either[Q, _Q]], Either[Stati, epsi]) => Set[Either[dummyS, Either[Q, _Q]]] = {
      case (Left(dummyS), Right(epsi)) => Set(Right(Left(this.S0)), Right(Right(another.S0)))
      case (Right(Left(q1)), Left(_x)) => this.trans(q1, _x) map (q => Right(Left(q)))
      case (Right(Right(q2)), Left(_x)) => another.trans(q2, _x) map (q => Right(Right(q)))
      case _ => Set.empty
    }

    val _f = (q: Either[dummyS, Either[Q, _Q]]) => q match {
      //costruisco qui l' NFA
      case Right(Left(q1)) => this.f(q1) //al solito, parto dal dummy per settare il primo stato (ME STESSO)
      case Right(Right(q2)) => another.f(q2) // qui il doppio passo a destra per andare avanti
      case _ => false // in tutti gli altri casi pongi pongi
    }


  }


    //concatenazione
    def concat[_Q](another:NFA[_Q, Stati])={


      sealed trait epsi
      object epsi extends epsi


      val _trans:(Either[Q, _Q],Either[Stati, epsi])=> Set[Either[Q, _Q]]={
        case (Left(a1), Right(epsi)) if this.f(a1) => Set(Right(another.S0))
        case (Left(q1), Left(_x))=> this.trans(q1, _x) map (q=>Left(q))
        case (Right(q2), Left(_x))=> another.trans(q2, _x) map(q=> Right(q))
        case _=>Set.empty

      }
      val _f: Either[Q, _Q] => Boolean={//riconosce uno stato
        case Right(q2)=> another.f(q2)
        case _=>false
      }
      //parte NFA epsi



    }












}
