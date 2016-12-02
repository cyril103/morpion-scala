package morpion

/**
  * Created by cyril on 21/06/16.
  */
trait Resultat
case object OGagne extends Resultat
case object Matchnul extends Resultat
case object Xgagne extends Resultat

case class Partie(
                   playerX: Player = Joueur.X,
                   playerO: Player = Ordinateur.O) {
  val rangee = Array((0, 1, 2), (3, 4, 5), (6, 7, 8), (0, 3, 6), (1, 4, 7), (2, 5, 8), (0, 4, 8), (2, 4, 6))

  val grille = Array(2, 2, 2, 2, 2, 2, 2, 2, 2)

  private def clear = for (i <- 1 to 50) Console.println()

  def init = {

    println("MORPION avec Minimax par Cyril Bertin")

    (0 to 8).map(grille(_) = 2)
    playerX.settour(false)
    playerO.settour(false)
  }

  def encore: Boolean = {

    println("encore une fois? o/n : ")
    val question = io.StdIn.readLine()
    question match {
      case "o" => true
      case "n" => false
      case _   => encore
    }
  }

  def peindre = new GrilleVueConsole(grille).affiche

  def question: Unit = {
    clear
    println("qui commence? X ou O : X/O :")
    val reponse = io.StdIn.readLine()

    if (reponse.toUpperCase().contains('X'))
      playerX.settour(true) else playerO.settour(true)

  }

  def parcours = {
    val test = { (joueurx: Player, joueuro: Player) =>
    {

      if (joueurx.dejouer && !check._2) {
        println(joueurx.message)
        joueurx match {
          case Ordinateur(_, _) => Thread.sleep(2000)
          case Joueur(_, _)     =>
        }
        joueurx.joue(grille); joueuro.settour(true); check
      }

    }
    }

    do {

      test(playerX, playerO)
      println(peindre)
      test(playerO, playerX)
      println(peindre)

    } while (!(check._2))

  }

  def check: (Resultat, Boolean) = {

    val produits = rangee.map { case (x, y, z) => (grille(x) * grille(y) * grille(z)) }

    if (produits.contains(125)) (OGagne, true)
    else if (produits.contains(27)) (Xgagne, true)
    else if (!grille.contains(2)) (Matchnul, true)
    else (Matchnul, false)

  }

  def commentaire = {
    check match {
      case (OGagne, b)   => println(s"le joueur ${playerO.symbol}  gagne")
      case (Xgagne, b)   => println(s"le joueur ${playerX.symbol} gagne")
      case (Matchnul, b) => println("match nul")
      case (_, _)        => println("fin partie")
    }
  }

}
