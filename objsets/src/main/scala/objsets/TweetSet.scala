package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

}

abstract class TweetSet {

  /** This method takes a predicate and returns a subset of all the elements
   *  in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = filter0(p, null) 
  
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet = {
    if (this.isEmpty) that
    else (this.tail.union(that)).incl(this.head)
  }

  // Hint: the method "remove" on TweetSet will be very useful.
  def ascendingByRetweet: Trending = {
    this.ascendingByRetweet0(new EmptyTrending)
  }

  def ascendingByRetweet0(accu: Trending): Trending = {
    if (this.isEmpty) accu
    else {
      val minVal = this.findMin
      this.remove(minVal).ascendingByRetweet0(accu + minVal)
    }
  }
  
  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  /** This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
  // -------------------------------------------------------------------------
}

class Empty extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = if (accu == null) this else accu
  
  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
  // -------------------------------------------------------------------------
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {
    if(accu == null) filter0(p, new Empty)
    else if (p(this.head)) this.tail.filter0(p,accu.incl(this.head))
    else this.tail.filter0(p,accu)
  }

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------
}


/** This class provides a linear sequence of tweets.
 */
abstract class Trending {
  def + (tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def + (tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  /** Appends tw to the end of this sequence.
   */
  def + (tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val allTweets = TweetReader.allTweets;
  
  def getMax0(tweets: Trending, curr: Tweet): Tweet = {
    if(tweets.isEmpty) curr
    else if (tweets.head.retweets > curr.retweets) getMax0(tweets.tail, tweets.head)
    else getMax0(tweets.tail,curr)
  }
  
  def getMax(tweets: Trending): Tweet = getMax0(tweets,tweets.head)
  
  def maxRetweets(tw1: Tweet, tw2: Tweet): Tweet = if (tw1.retweets > tw2.retweets) tw1 else tw2
  
  val googleTweets: TweetSet = allTweets.filter((t: Tweet) => {
    google.exists(keyword => {t.text.contains(keyword)} )
  })

  val appleTweets: TweetSet = allTweets.filter((t: Tweet) => {
    apple.exists(keyword => {t.text.contains(keyword)} )
  })

  // Q: from both sets, what is the tweet with highest #retweets?
  val trending: Trending = (googleTweets.union(appleTweets)).ascendingByRetweet
  
  val mostTweets = getMax(trending)
}

object Main extends App {
  // Some help printing the results:
  //println("RANKED:")
  //GoogleVsApple.trending foreach println
  val tw1 = new Tweet("me","what it is",10)
  val tw2 = new Tweet("me","what it's not",3)
  val tw3 = new Tweet("me","dont want to think it aint what it be but it do",4)
  val tw4 = new Tweet("me","what it is",3)
 
   
  val twSet = new Empty
  val twSet2 = twSet.incl(tw1).incl(tw2).incl(tw3)
  
  println("Printing the trending set")
  val trnd = twSet2.ascendingByRetweet
  trnd foreach println
  
  println()
  println()

  println("Printing the first set")
  twSet2 foreach println

  println("Printing the second set")  
  val tw5 = new Tweet("me","what am",5)
  val twSet3 = (new Empty).incl(tw5).incl(tw3)
  twSet3 foreach println
  
  println("Printing the sets for the union")
  twSet2 foreach println
  twSet3 foreach println
  println("Now printing the union")
  val unionSet = twSet2.union(twSet3)
  unionSet foreach println
  
  println("Printing the filter set")
  val filterSet = unionSet.filter((t: Tweet) => {t.retweets > 4})
  filterSet foreach println
  
  println("Printing an filter set from an empty tweet set")
  val emptyFilter = new Empty
  val filteredFromEmpty = emptyFilter.filter(t => true)
  filteredFromEmpty foreach println
 
  //val allTweets = TweetReader.allTweets;
  //allTweets foreach println
  println("Here are all the trending tweets");
  GoogleVsApple.trending foreach println
  
  val mostTweets = GoogleVsApple.mostTweets;
  println("Here is the most relevant tweet between google and apple: " + mostTweets)
}
