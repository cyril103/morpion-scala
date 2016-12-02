package morpion

/**
  * Created by cyril on 21/06/16.
  */
object Main extends App {
  val partie = Partie()
  do{

    partie.init


    Thread.sleep(2000)
    partie.question
    println(partie.peindre)
    partie.parcours
    partie.commentaire
  }
  while(partie.encore)

}
