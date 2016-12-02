package morpion

/**
  * Created by cyril on 21/06/16.
  */
case class Ordinateur(
                       protected var tour:Boolean = false,
                       val symbol:String = "O") extends Player {


  def message = s"Ordinateur $symbol réféchis......."

  def joue(tab :Array[Int]) = {

    tab(Evaluation2(tab,value)) = value

    tour = !tour
  }


}

object Ordinateur {
  def X = new Ordinateur(symbol ="X")
  def O = new Ordinateur(symbol ="O")
}
