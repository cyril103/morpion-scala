package morpion

/**
  * Created by cyril on 21/06/16.
  */
object Evaluation  {
  val Gain = Array((0,1,2),(3,4,5),(6,7,8),(0,3,6),(1,4,7),(2,5,8),(0,4,8),(2,4,6))

  def apply(tab: Array[Int],value:Int) = {
    val note = Array.fill(9)(-1000)


    for (i <- tab.indices) {
      if (tab(i) == 2){
        tab(i) = value
        note(i) =hypo(i,tab,value)
        tab(i) = 2
      }

    }
    val coups =note.zip(note.indices).filter{case(x,y) => x==note.max} map{case(x,y) =>y}

    def noIndice(listCoups: List[Int]) = listCoups match {
      case Nil => throw new Error("pas de coups possible")
      case List(n) => n
      case xs => val nbcoups = xs.size
        val i = (math.random*nbcoups).toInt

        xs(i)
    }
    if(tab(4) ==2) 4 else noIndice(coups.toList)
  }

  private def hypo(i: Int, tab: Array[Int],value:Int):Int = {

    var cl = 0
    var co = 0
    val A = if(value==5) 12 else 20 ; val B = if(value==5) 18 else 50 ;
    val C = value*4 ; val D = value*value*2 ; val E = value*value*value ;
    val F = if(value==5) 45 else 75
    val G = 15
    val produits = Gain.map{case(x,y,z) =>(tab(x)*tab(y)*tab(z),(x,y,z))}
    produits foreach  { _ match{
      case (A,t) => cl += 1
      case (B,t) => cl += 2
      case (C,t) => co += 1
      case (D,t) => co += 2
      case (E,(x,y,z)) => if(x == i || y == i || z == i) co += 1000
      case (F,(x,y,z)) => if(x == i || y == i || z == i) co += 500
      case _ =>
    }
    }
    if (tab(i)*tab(8-i) == G) co += 2




    co-cl

  }

}
