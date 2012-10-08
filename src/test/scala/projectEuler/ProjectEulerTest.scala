package projecteuler

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import projecteuler.Euler._

@RunWith(classOf[JUnitRunner])
class ProjectEulerTestSuite extends FunSuite {
  test("1. Find the sum of all the multiples of 3 or 5 below 1000."){
    assert(problem1(10) === 23)
    assert(problem1(1000) === 233168)
  }

  test("2. By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.") {
    assert(problem2(90) ===  2 + 8 + 34)
    assert(problem2(4000000) ===  4613732)
  }

  test("3. What is the largest prime factor of the number 317584931803?") {
    assert(problem3(600851475143l) ===  6857)
  }

  test("4.  Find the largest palindrome made from the product of two 3-digit numbers."){
    assert(problem4()===906609)
  }

  test("6.  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."){
    assert(problem6(10)===2640)
    assert(problem6(100)===25164150)
  }

  /*
   * problem7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10 001st prime number?
*/
  test("7. 10 001st prime number"){
    assert(problem7(6)===13)
    assert(problem7(10001)===104743)
  }

  /*
   *
   * problem8
   Find the greatest product of five consecutive digits in the 1000-digit number.
   */
  test("8. Find the greatest product of five consecutive digits in the 1000-digit number.") {
    assert(problem8("123456543") === 4*5*6*5*4)

    assert(problem8("""
		    12345
		    6543
		    """) === 4*5*6*5*4)

    val digits= """
    73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450
    """
    assert(problem8(digits) === 40824)
  }
/*
 *
 * 

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
 */
  test("Find the only Pythagorean triplet, {a, b, c}, for which a^2 + b^2 + c^2 = 1000.") {
    assert(problem9() === 31875000) // 200,375,425
  }


 /* Problem10
  * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  * Find the sum of all the primes below two million.
*/
  test("10. Find the sum of all the primes below two million.") {
    assert(problem10(10)===17)
    assert(problem10(2000000) === 142913828922l)
  }
}



@RunWith(classOf[JUnitRunner])
class UtilsTestSuite extends FunSuite {
  import utils._
  test("IsPrime"){
    assert(isPrime(987654323) === true)
    assert(isPrime(13) === true)  
    assert(isPrime(3) === true)  
    assert(isPrime(2) === true)  
    assert(isPrime(4) === false)  
    assert(isPrime(35) === false)  
  }

  test("isPalindrom") {
    assert(isPalindrom(919)===true)
    assert(isPalindrom(918)===false)
    assert(isPalindrom(777)===true)
    assert(isPalindrom(707)===true)
    assert(isPalindrom(7)===true)
  }
}
 
