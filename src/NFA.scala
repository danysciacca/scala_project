/**
  * Created by lucatosto on 15/03/17.
  */

case class NFA[Q, Stati](trans: (Q, Stati)=> Set[Q], S0:Q, f:Q=>Boolean) {
  println("sono dentro NFA")
  val Q= this
  val Stati=this
  val unione= Union(this)
  sealed trait epsi

  def Union[_Q](another: NFA[_Q, Stati])= {
    println("--------------UNIONE")
    def asNFA = this

    sealed abstract trait dummyS
    object dummyS extends dummyS //uno stato iniziale dello stato iniziale.
    object epsi extends epsi
    println("inizio TRANS")
    val _trans: (Either[dummyS, Either[Q, _Q]], Either[Stati, epsi]) => Set[Either[dummyS, Either[Q, _Q]]] = {


      case (Left(dummyS), Right(epsi)) => Set(Right(Left(this.S0)), Right(Right(another.S0)))
      case (Right(Left(q1)), Left(_x)) => this.trans(q1, _x) map (q => Right(Left(q)))
      case (Right(Right(q2)), Left(_x)) => another.trans(q2, _x) map (q => Right(Right(q)))
      case _ => Set.empty

    }
    println("------TRANS EFFETTUATA, il suo valore è: "+_trans)

    println("inizio calcolo di _f")
    val _f = (q: Either[dummyS, Either[Q, _Q]]) => q match {
      //costruisco qui l' NFA
      case Right(Left(q1)) => this.f(q1) //al solito, parto dal dummy per settare il primo stato (ME STESSO)
      case Right(Right(q2)) => another.f(q2) // qui il doppio passo a destra per andare avanti
      case _ => false // in tutti gli altri casi pongi pongi
    }
    println("fine calcolo _f, il suo valore è : "+ _f)
    println("inizio funzione epsiNFA")
    epsiNFA(_trans, Left(dummyS).asInstanceOf[Either[dummyS, Either[Q, _Q]]],_f)
    println("---epsiNFA finita, il valore di trans è : " +_trans + " e il valore di f è: "+ _f +", se i valori sono uguali vuol dire che non ci sono epsilon mosse")
  }

  //concatenazione
  def concat[_Q](another:NFA[_Q, Stati])={
    println("-----------CONCATENAZIONE")
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
    epsiNFA(_trans, Left(this.S0), _f)
  }
  //stella di kleene
  def kleene[_Q](another: NFA[_Q, Stati]) = {
    println("-------STELLA DI KLEENE")
    object epsi extends epsi

    val _trans: (Q,Either[Stati,epsi])=>Set[Q] ={
      case (q,Right(epsi)) if this.f(q) => Set(this.S0)
      case (q,Right(epsi)) if !this.f(q) => Set.empty
      case (q,Left(x)) => trans(q,x)
      case _ => Set.empty
    }

    val _f = (q:Q) => q == S0 || f(q)

    epsiNFA(_trans, S0, _f)
  }

  def epsiNFA[Q, Stati](trans:(Q, Either[Stati, epsi])=> Set[Q], S0:Q, f:Q=> Boolean) {
    println("-------in esecuzione il calcolo di epsilon mosse")
    def chiusura(q: Q): Set[Q] = {
      object epsi extends epsi
      var visitato = Set.empty[Q]
      val queue = new scala.collection.mutable.Queue[Q]()
      queue.enqueue(q)
      while (!queue.isEmpty) {
        val nuovo_stato = trans(queue.dequeue(), Right(epsi)) -- visitato
        queue ++= nuovo_stato
        visitato ++= nuovo_stato
      }
      visitato
    }
  }


  def ValueToReturn[Q, Stati](): Unit ={  //Manca passaggio paramentri
    //che ne pensi di costruire qui la tabella? nel senso che ci facciamo mandare direttamente i dati pronti a execution

    //val array = Array.fill(2,2)(0)
    //array: Array[Array[Int]] = Array(Array(0, 0), Array(0, 0))
  }
}
