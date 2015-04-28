package calculator

object TweetLength {
  final val MaxTweetLength = 140

   
  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    //println("Calculando remaining chars")
    /*Signal 
        {
          val string = tweetText()
          println(string)
          MaxTweetLength-tweetLength(string)
        }*/
    Signal(MaxTweetLength-tweetLength(tweetText()))
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    val remaining = remainingCharsCount()
    Signal{
      //println("Prueba de que no depende del tiempo esta evaluacion:" + remaining)  //Fijate en el console. Siempre es el mismo. Porque no se actualiza.
      //println("Numero de chars remanentes: "+remainingCharsCount())
      remainingCharsCount() match {
        case good if 14  < good                => "green"
        case warn if 0   <= warn && warn <= 14 => "orange"
        case bad  if bad < 0                   => "red"
      }
    }
    //Signal("red")
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
