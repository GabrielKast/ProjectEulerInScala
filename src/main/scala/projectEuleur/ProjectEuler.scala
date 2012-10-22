package projecteuler

import scala.annotation.tailrec

// see http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html

/*
sealed trait Bounce[A]
case class Done[A](result: A) extends Bounce[A]
case class Call[A](func: () => Bounce[A]) extends Bounce[A]

def trampoline[A](bounce: Bounce[A]): A = bounce match {
  case Call(func) => trampoline(func())
  case Done(x) => x
}
*/

object utils {
  // lazy val fibs: Stream[Int] = 0 #:: 1 #:: ((fibs zip fibs.tail) map { case (c1,c2) => c1 + c2 })
  lazy val fibs: Stream[Int] = Stream.cons(0, Stream.cons(1, fibs.zip(fibs.tail) map { case (c1,c2) => c1 + c2 } ))

  // lazy naturals :Strean[Int] = 2 #:: 3:: 
  def isPrime(n :Long) :Boolean = { 
    val nSquare = math.sqrt(n)
    def isDividedBy(n :Long, m :Long): Boolean = 
      if (m > nSquare) false
      else if (n % m == 0) true
      else isDividedBy(n, m+2)
    if (n <= 3) true
    else if (n % 2 ==0) false 
    else ! isDividedBy(n, 3)
  }

  def isPalindrom(n :Long)=n.toString.reverse == n.toString


  // from 1 to 1000
  def num2letters(n: Int): String= {
    val ciffers = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 
		      5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 
		      9 -> "nine", 10 -> "ten", 11 -> "eleven", 12 -> "twelve",
		      13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen",
		      16 -> "sixteen",  17 -> "seventeen", 
		      18 -> "eighteen", 19 -> "nineteen",
		      20 -> "twenty", 30 -> "thirty", 40 -> "forty",
		      50 -> "fifty", 60 -> "sixty", 70 -> "seventy", 
		      80 -> "eighty", 90 -> "ninety")
    def cifferOf(n: Int)=
	if (ciffers contains n) ciffers(n)::Nil
	else Nil

    def numberOf(n: Int)= {
      val rest = n - n/100*100
      if (rest<20)
	cifferOf(rest)
      else {
	val dozens = rest/10*10
	val cifferRest = rest - dozens
	cifferOf(dozens) ::: cifferOf(cifferRest)
      }
    }

    def hundredsOf(n: Int) = {
      val hundreds = (n/100) % 10
      cifferOf(hundreds) match {
	case List() => List[String]()
	case h => h ::: List("hundred")
      }
    }

    def thousandsOf(n: Int) = {
      val thousands = (n/1000) % 10
      cifferOf(thousands) match {
	case List() => List[String]()
	case h => h ::: List("thousand")
      }
    }
    val dozens = numberOf(n) mkString " " 
    val hundreds = (thousandsOf(n):::hundredsOf(n)) mkString " "
    hundreds match {
      case ""=> dozens
      case _ if dozens=="" => hundreds
      case _ => List(hundreds, dozens) mkString " and "
    }
    // thousandsOf(n)+hundredsOf(n)+dozenOf(n)+numberOf(n)
  }
}


object Euler{
  import utils._
  def problem1(x :Int) = List.range(1, x).filter(x => x % 3==0 || x % 5 ==0).sum
  def problem2(lim :Int) = utils.fibs takeWhile( _ < lim) filter (_ % 2 ==0) sum

  //   3. What is the largest prime factor of the number 317584931803?
  def problem3(n :Long) :Long= {
    def findValid(x :Long) :Long= if (x==1) 1 
			    else if (n % x ==0 && isPrime(x)) x
			    else findValid(x-1)
    findValid(math.sqrt(n).toLong)
  }

  def problem4() :Long= {
    val range = List.range(1000l, 100l, -1)
    range.foldLeft(0l)((max :Long, y) => (max :: (range.map ( _ * y ).filter( x => (x >= max && isPalindrom(x)) ))).sortWith((a, b) => (a > b)).head)
  }

  def problem6(n :Int)= {
    val range :List[Int] = List.range(1, n+1)
    val sumOfSquare :Long = range.map ( x => x * x).sum
    val squareOfSum :Long = range.sum match {case n => n*n }
    math.abs(sumOfSquare - squareOfSum)
  }

  def problem7(nth :Int)= {
    def nthPrime(nth :Int, n :Long) :Long= {
      if (nth==1) n
      else {
	val newN = n + 2
	isPrime(newN) match {
	  case true =>  nthPrime(nth-1, newN)
	  case false => nthPrime(nth, newN)
	}
      }
    }
    nthPrime(nth -1 , 3)
  }


  def problem8(d :String) :Int= {
    val digits = """\D+""".r.replaceAllIn(d, m => "").toCharArray
    def maxOfFive(max :Int, digits :Array[Char]) :Int = 
      if (digits.length <5) max
      else maxOfFive(math.max(max, digits.take(5).foldLeft(1)((m, x) => m*(x -'0'))),
		     digits.tail)
    maxOfFive(0, digits)
  }

  def problem9() :Long= {
    val total = 1000
    List.range(total, 1, -1).map( z => List.range(1, z/2.toInt).map( x => (x, total - x - z, z) ).filter(tuple =>tuple match { case (x, y, z) => x*x + y*y == z*z })).filter(! _.isEmpty) match { case List(List((x, y, z))) => x*y*z case _ => throw new IllegalStateException }
    
  }

  def problem10(n :Int) :Long={
    List.range(3L, n, 2).foldLeft(2L) { (x, y) => if (isPrime(y)) x+y else x }
    // (2L :: List.range(3L, n, 2)).reduceLeft{ (x, y) => if (isPrime(y)) x+y else x }
    // (2L::List.range(3L, n, 2)).filter( x => isPrime (x)).sum
  }


  def problem11(square:Array[Array[Int]]) : Long= {
    val lineLength = 4
    val size = square.length // Let's assume the grid is a square
    val directions = List ((1,0), (1,1), (0, 1), (-1, 1), (-1, 0))
    def isOut(x :Int, xx :Int) = (x+lineLength*xx<0 || x+lineLength*xx>size)
    def findMax(point :(Int, Int), max :Long, direction :(Int, Int)) =
      (point, direction) match {
	case ((x, y), (dx, dy)) if isOut(x, dx) || isOut(y, dy) => max
	case ((x, y), (dx, dy)) =>
	  val tmpMax :Long = List.range(0, lineLength).foldLeft(1)((acc, d) => acc * square(x + d*dx)(y+d*dy) )
	  if (tmpMax>max) tmpMax else max
	case _ => throw new IllegalStateException
      }

    val points = List.range(0, size).map( y => List.range(0, size).map( x=> (x, y)) ).flatten
    
    points.foldLeft(0L)( (max, point) => directions.foldLeft(max)( (m, direction) => findMax(point, m, direction)))
  }

  // // Too brutal. Does not work for nb=500
  // def problem12(nb :Int) :Long= {
  //   def nbDivisors(n :Long) :Int = (1L to n).filter(n % _ ==0).length
  //   def solution(nth :Long, total :Long) :Long = {
  //     if (nth % 100000==0) println(nth) else 0
      
  //     if (nbDivisors(total) > nb) total 
  //     else if (nth<0) throw new IllegalStateException
  //     else solution(nth+1, total+nth)
  //   }
  //   solution(2L, 1L)
  // }

  // def problem14(limit :Int) :Int= {
  //   def next (n:Int) :Int =
  //     if (n == 1) throw new IllegalStateException
  //     else if (n%2==0) n/2
  //     else 3*n+1

  //   def computeLengthOf(lengths:Array[Int], x :Int) :Array[Int] = 
  //     if (x==1) {
  // 	lengths(1)=1
  // 	lengths
  //     } else {
  // 	val nextX :Int = next(x)
  // 	if (nextX<=limit && lengths(nextX)>=0) {
  // 	  val newLengths = computeLengthOf(lengths, nextX)
  // 	  newLengths(x)=newLengths(nextX)+1
  // 	  newLengths
  // 	} else {
  // 	  lengths(x)=lengths(nextX)+1
  // 	  lengths
  // 	}
  //     }
  //   var origin = Array.ofDim[Int](limit+1)
  //   val all = (1 to limit).foldLeft(origin)(computeLengthOf)
  //   println("size = "+all.size)
  //   all.max
  // }

/**
 * http://projecteuler.net/problem=17
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
*/

  def problem17(limit: Int): Int = {
   (1 to limit).foldLeft(0)( (accLength, x) => accLength+ num2letters(x).toList.filter(letter => (letter >='a' && letter <='z')).length)
  }


/**
 * http://projecteuler.net/problem=19
 * problem19
*
You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*/


/** problem20
 *

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
*/


/* http://projecteuler.net/problem=21
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
*/


/*
 * http://projecteuler.net/problem=23
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
*/

/** Problem24
 * 

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*/

/** Problem30
 * 

Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

    1634 = 14 + 64 + 34 + 44
    8208 = 84 + 24 + 04 + 84
    9474 = 94 + 44 + 74 + 44

As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
*/

/**
 * Problem31
 * 

In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
*/


  def problem27(limit :Int) :Int = {
    class Result(a :Int, b :Int) {
      def conseqPrimes(n :Int) :Int= {
	val fn = n*n+a*n+b
	if (fn<2 || ! isPrime(fn) ) n else conseqPrimes (n+1) 
      }
      val nbConseqPrimes :Int = conseqPrimes(0)
      override def toString :String="# ("+a+", "+b+") => "+ nbConseqPrimes
      def product :Int = a*b
    }
    val tuples :List[(Int, Int)]= List.range(-limit,limit).map(b => List.range(-limit, limit).map(a =>(a, b))).flatten
    val result :Result = tuples.map(t => new Result(t._1, t._2)).sortWith( (t1, t2) => t1.nbConseqPrimes > t2.nbConseqPrimes).head
    result.product
  }


  def problem81(grid:Array[Array[Int]]) :Int= {
    case class Cell(x: Int, y: Int, sum: Int)
    val limit: Int = grid.length -1
    def mymin(x :Int, y :Int) = if (y<0) x else if (x<0) y else math.min(x, y)
    def nextCell(cell :Cell, x :Int, y :Int) = Cell(x, y, cell.sum+grid(y)(x))
    def moveForward(cell :Cell, min :Int) :Int = cell match {
      case Cell(x, y, sum) if x==limit && y == limit => mymin(sum, min)
      case Cell(x, y, sum) if sum>=min && min>0 => min
      case Cell(x, y, sum) if y==limit => 
	moveForward(nextCell(cell, x+1, y), min)
      case Cell(x, y, sum) if x==limit => 
	moveForward(nextCell(cell, x, y+1), min)
      case Cell(x, y, sum) => 
	mymin(moveForward(nextCell(cell, x+1, y), min), 
	      moveForward(nextCell(cell, x, y+1), min))
    }
    moveForward(Cell(0, 0, grid(0)(0)), -1)
  }
}
