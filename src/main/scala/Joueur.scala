package morpion

/**
  * Created by cyril on 21/06/16.
  */
import io.StdIn.readInt

case class Joueur(
                   protected var tour:Boolean = false, val symbol:String = "X") extends Player {

  def message = "A vous de jouer " + symbol

  def joue(tab: Array[Int]): Unit = {

    println("entrer le chiffre de la case")
    try{
      val i = readInt()
      if (tab(i)==2) tab(i) = value else joue(tab)
      tour = !tour
    }catch{
      case e:ArrayIndexOutOfBoundsException => joue(tab)
      case e:NumberFormatException => joue(tab)
    }

  }


}

object Joueur  {
  def X = new Joueur(symbol = "X")
  def O = new Joueur(symbol = "O")
}