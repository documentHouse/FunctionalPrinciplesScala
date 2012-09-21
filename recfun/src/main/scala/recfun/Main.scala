package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    val str = new String("()");
    if(balance(str.toList))
    	println("The string " + str + " is balanced")
    else
    	println("The string " + str + " is not balanced")
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c < 0)||(r < 0)||(c > r))
      throw new IllegalArgumentException("Arguments outside of pascal triangle bounds")
    if ((c == 0) || (c == r))
      1;
    else
      pascal(c-1,r-1) + pascal(c,r-1);
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def balanceParen(parenValue: Char, chars: List[Char]): Boolean = {
    	//if((chars.head != '(')&&(chars.head != ')'))
    	
    	if(chars.isEmpty)
    		false;
    	else
    	{
    		if(chars.head == ')')
    			true;
    		else if(chars.head == '(')
    			balanceParen(parenValue,chars.tail)
    		else
    			balance(chars.tail)
    	}
    }
    
    if(chars.isEmpty)
    	true;
    else if((chars.head != '(')&&(chars.head != ')'))
    	balance(chars.tail);
    else
    {
    	if(chars.head == ')')
    		false;
    	else
    		balanceParen('(',chars.tail)
        //	false;
        //else
        //	balance(chars.tail);
    }
    
    //balance(chars.tail);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
	0;
  }
}
