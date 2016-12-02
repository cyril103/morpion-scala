package morpion

/**
  * Created by cyril on 21/06/16.
  */
class GrilleVueConsole(val tab: Array[Int]) {

  def affiche = {
    var vue = ""
    tab.zip(tab.indices).map{_ match{
      case (2,i) => vue += s" $i " ; if(i==2||i==5) vue += "|\n|---------|\n|"
      case (3,i) => vue += " X "   ; if(i==2||i==5) vue += "|\n|---------|\n|"
      case (5,i) => vue += " O "   ; if(i==2||i==5) vue += "|\n|---------|\n|"
    }

    }
    val res = " _________\n\n|" + vue + "|\n _________\n"
    res
  }
}
