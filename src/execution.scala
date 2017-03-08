/**
  * Created by Daniele on 08/03/2017.
  */
import scala.io.StdIn.readLine
import scala.collection.Seq
object execution {
  def main(args: Array[String]): Unit = {




    println("questo algoritmo consente di trasformare una espressione regolare in nfa e poi in dfa")
    println("inserisci espressione regolare: ") //ab*

   /* val line = Console.readLine()
    println("hai inserito: "+line)*/

    //

    //alfabeto che in questo caso è a,b
    sealed abstract class Linguaggio
    case object A extends Linguaggio
    case object B extends Linguaggio

    sealed class Stato
    case object S0 extends Stato
    case object S1 extends Stato
    case object S2 extends Stato
    case object S3 extends Stato
    case object S4 extends Stato


    val trans:(Stato, Linguaggio)=> Set[Stato]
    = {
      case (S0, A)=> Set(S0, S1)
      case (S0, B)=> Set(S0)
      case (S1, B)=> Set(S2)
      case _=>Set.empty //la stringa non appartiene al linguaggio

    }

    val AB_NFA=NFA(trans, S0, Set[Stato](S2))

    //algorithms
    println("%s".format(Seq(A,B))+"=>"+AB_NFA(Seq(A,B)))
  }

}
