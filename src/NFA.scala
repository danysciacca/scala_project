/**
  * Created by Daniele on 08/03/2017.
  */
case class NFA[Q, Stati](trans: (Q, Stati)=> Set[Q], S0:Q, f:Q=>Boolean) {



}
