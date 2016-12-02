package morpion

/**
  * Created by cyril on 21/06/16.
  */
trait Player {

  protected var tour:Boolean

  def message :String
  val symbol:String

  val value: Int = if(symbol =="X") 3 else 5
  def joue(tab: Array[Int]): Unit

  def dejouer = tour
  def settour(boo: Boolean) { tour = boo}
}