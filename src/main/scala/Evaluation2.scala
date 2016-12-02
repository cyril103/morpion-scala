package morpion

/**
  * Created by cyril on 21/06/16.
  */
import scala.math.Ordered



object Evaluation2 {
  val Gain = Array((0, 1, 2), (3, 4, 5), (6, 7, 8), (0, 3, 6), (1, 4, 7), (2, 5, 8), (0, 4, 8), (2, 4, 6))

  trait Node extends Ordered[Node] {







    def compare(that:Node): Int = {
      if(that.note < this.note) 1 else if (that.note > this.note) -1 else 0
    }

    val hypot: Int = hypo(posact,tableau,value)
    val note: Int = if(value == 3) -hypot else hypot
    val pos: Int
    val posact: Int

    val produits = Gain.map { case (x, y, z) => (tableau(x) * tableau(y) * tableau(z)) }
    def value: Int
    def depth: Int
    def parent: Node
    def childs: List[Child] = if (!this.isLeaf){
      var lst: List[Child] = Nil
      for (i <- 0 to 8) {
        val tabcopy: Array[Int] = Array.fill(9)(0)
        tableau.copyToArray(tabcopy)
        if (tabcopy(i) == 2) {
          tabcopy(i) = {if(value==5) 3 else 5}
          lst ::= new Child(this, tabcopy, if (depth == 0) i else pos, i, depth + 1, { if (value==5) 3 else 5})

        }

      }
      lst.reverse
    }else Nil

    def tableau: Array[Int]

    override def toString = tableau.mkString + " " + depth + " " + pos + " " + note

    def isLeaf: Boolean = {
      produits.contains(27) || produits.contains(125) || !tableau.contains(2)

    }

  }

  class Origine( val tableau: Array[Int], tour: Boolean , val depth: Int = 0) extends Node {

    def parent = throw new Error("origine")
    def value = if (tour) 3 else 5
    val pos = 0
    val posact = 0
    override val note :Int = if( tour) 99999 else -99999
  }

  class Child(val parent: Node,val tableau: Array[Int], val pos: Int, val posact: Int, val depth: Int, val value: Int) extends Node {

  }






  def maxnode(n1 :Node, n2 :Node) : Node = {
    if  (n1 <= n2) n2 else n1
  }

  def minnode(n1 :Node, n2 :Node) : Node = {
    if  (n1 >= n2) n2 else n1
  }


  def minimax2(noeud: Node, profondeur: Int, maxjoueur: (Boolean, Int)): Node =  {

    if (profondeur == 0 || noeud.isLeaf) noeud



    else if (maxjoueur._1 ) {
      var meilleurnoeud : Node = new Origine(noeud.tableau,false)

      noeud.childs.foreach{ x => meilleurnoeud = maxnode(meilleurnoeud,minimax2(x, profondeur -1,(false,{ if (maxjoueur._2 == 5) 3 else 5})))

      }
      meilleurnoeud

    } else {

      var meilleurnoeud : Node = new Origine(noeud.tableau,true)

      noeud.childs.foreach{ x => meilleurnoeud = minnode(meilleurnoeud,minimax2(x, profondeur -1,(true,{ if (maxjoueur._2 == 5) 3 else 5})))

      }

      meilleurnoeud
    }


  }


  def hypo(i: Int, tab: Array[Int], value: Int): Int = {

    var cl = 0
    var co = 0
    val A = if (value == 5) 12 else 20; val B = if (value == 5) 18 else 50;
    val C = value * 4; val D = value * value * 2; val E = value * value * value;
    val F = if (value == 5) 45 else 75
    val G = 15
    val produits = Gain.map { case (x, y, z) => (tab(x) * tab(y) * tab(z), (x, y, z)) }
    produits foreach {
      _ match {
        case (A, t)         => cl += 1
        case (B, t)         => cl += 2
        case (C, t)         => co += 1
        case (D, t)         => co += 2
        case (E, (x, y, z)) => if (x == i || y == i || z == i) co += 1000
        case (F, (x, y, z)) => if (x == i || y == i || z == i) co += 500
        case _              =>
      }
    }
    if (tab(i) * tab(8 - i) == G) co += 2

    co - cl

  }



  def apply(tab: Array[Int], value: Int): Int ={
    val tour = if(value==5) true else false
    minimax2(new Origine(tab,tour),9, (tour,5)).pos



  }








}
