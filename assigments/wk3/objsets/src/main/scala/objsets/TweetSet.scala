package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the smallest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = {
    def formList(input: TweetSet, acc: TweetList): TweetList = {
      if (input.isEmpty) acc else {
        val mostRetweettedTweet = input.mostRetweeted
        formList(input.remove(mostRetweettedTweet), new Cons(mostRetweettedTweet, acc))
      }
    }
    // def reverseIter(x: TweetList, acc: TweetList): TweetList = if (x.isEmpty) acc else reverseIter(x.tail, new Cons(x.head, acc))
    // def reverseList(x: TweetList) = reverseIter(x, Nil)
    formList(this, Nil)
  }

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def isEmpty: Boolean
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def union(that: TweetSet): TweetSet = that

  def foreach(f: Tweet => Unit): Unit = ()

  def isEmpty = true
}

class UnionSet(a: TweetSet, b: TweetSet) extends TweetSet {
  override def filter(p: Tweet => Boolean): TweetSet = {
    val af = a.filter(p)
    val bf = b.filter(p)
    if(af.isEmpty && bf.isEmpty) new Empty 
    else if(af.isEmpty) bf
    else if(bf.isEmpty) af
    else new UnionSet(af, bf)
  }
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = throw new java.util.NoSuchElementException
  def union(that: TweetSet): TweetSet = if(that.isEmpty) this else new UnionSet(this, that)
  def mostRetweeted: Tweet = {
    assert((!a.isEmpty) || (!b.isEmpty))
    if (a.isEmpty) b.mostRetweeted
    else if (b.isEmpty) a.mostRetweeted
    else {
      val am = a.mostRetweeted
      val bm = b.mostRetweeted
      if (am.retweets < bm.retweets) am else bm
    }
  }
  def incl(tweet: Tweet): TweetSet = new UnionSet(a.incl(tweet), b)
  def remove(tweet: Tweet): TweetSet = new UnionSet(a.remove(tweet), b.remove(tweet))
  def contains(tweet: Tweet): Boolean = { a.contains(tweet) || b.contains(tweet) }
  def foreach(f: Tweet => Unit): Unit = { a.foreach(f); b.foreach(f) }
  def isEmpty: Boolean = { a.isEmpty && b.isEmpty }
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val ret = left.filterAcc(p, right.filterAcc(p, acc))
    if (p(elem)) ret.incl(elem) else ret
  }

  def union(that: TweetSet): TweetSet = 
  //{ filterAcc(x => true, that) }

  // if(that.isEmpty) this else if(left.isEmpty) (right union that) incl elem else if(right.isEmpty) (left union that) incl elem else ((left union right) union that) incl elem 
 new UnionSet(this, that)
  //((left union right) union that) incl elem

  def mostRetweeted: Tweet = {
    def min(tweet: Tweet, set: TweetSet): Tweet = {
      // Checking that set is not empty
      if (set.isEmpty) tweet else {
        val v = set.mostRetweeted
        if (tweet.retweets < v.retweets) tweet else v
      }
    }
    min(min(elem, left), right)
  }
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def isEmpty = false
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def checkLst(keywords: List[String])(t: Tweet): Boolean = {
    def pred(s: String) = t.text.contains(s)
    keywords.exists(pred)
  }

  // lazy val
  def allTweets = TweetReader.allTweets
  
  def googleTweets: TweetSet = TweetReader.allTweets.filter(checkLst(google))
  def appleTweets: TweetSet = TweetReader.allTweets.filter(checkLst(apple))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  def trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    (System.nanoTime - now) / 1000
  }
  
  var microsUnion: Long = 0
  for (i <- 1 to 10000) {
    microsUnion += time(GoogleVsApple.allTweets)
  }

  var microsTrending: Long = 0
  for (i <- 1 to 10000) {
    microsTrending += time(GoogleVsApple.trending)
  }

  
  println("Average for union: %f microseconds".format((microsUnion.toDouble) / 10000))
  println("Average for trending: %f microseconds".format((microsTrending.toDouble) / 10000))
  
  // Print the trending tweets
  //GoogleVsApple.trending foreach println
}