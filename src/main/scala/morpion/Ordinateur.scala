package morpion

/**
  * Created by cyril on 21/06/16.
  */
case class Ordinateur(

                       private var _tour: Boolean = false,
                       symbol: String = "O") extends Player {

  def tour = _tour

  def tour_=(t: Boolean) = _tour = t

  def message = s"Ordinateur $symbol réféchis......."

  def joue(tab: Array[Int]) = {

    tab(Evaluation2(tab, value)) = value

    tour = !tour
  }


}

object Ordinateur {
  def X = new Ordinateur(symbol = "X")

  def O = new Ordinateur(symbol = "O")
}
