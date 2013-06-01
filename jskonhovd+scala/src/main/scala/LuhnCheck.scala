import io.Source

object LuhnCheck {
     def luhnDoubleOddNumber(a: List[Integer]): List[Integer] = {
    	def helper(foo: List[Integer], acc: Integer):List[Integer] = {
    			foo match {
    			case List() => List()
    			case x::xs => if(acc%2 != 0) (2*x)::helper(xs,acc+1)
      		   			  	  else x::helper(xs,acc+1)
      		   }
    	}
    helper(a, 0)
  }                                               //> luhnDoubleOddNumber: (a: List[Integer])List[Integer]
    
     
  
  def integerBreakDown(a: Integer):List[Integer] = {
    def helper(i : Integer):List[Integer] = {
      if(i > 9)
      {
        (i%10)::helper(i/10)
      }
      else
      {
        List(i%10)
      }
      
    }
    helper(a)
  }                                               //> integerBreakDown: (a: Integer)List[Integer]
  
  def luhnList(a: List[Integer]):List[Integer] = { a match {
    case List() => List()
    case x::xs => if(x > 9) integerBreakDown(x).reverse:::luhnList(xs)
    			  else integerBreakDown(x).reverse:::luhnList(xs)
  }
  }                                               //> luhnList: (a: List[Integer])List[Integer]
  
  def sumLuhn(a:List[Integer]): Integer = { a match {
    case List() => 0
    case x::xs => x+sumLuhn(xs)
  }
    
  }                                               //> sumLuhn: (a: List[Integer])Integer
  
  def checkLuhn(a : List[Integer]): Boolean = {
    if(sumLuhn((luhnList(luhnDoubleOddNumber(a.reverse)))) % 10 == 0) true
    else false
  }                                               //> checkLuhn: (a: List[Integer])Boolean
  
  def isThisAllIntegers(str : String):Boolean = {
    def helper(charList: List[Char]): Boolean = { charList match {
        case List() => true
        case x::xs => if (x.isDigit) helper(xs)
        			  else false
        }
    }
    helper(str.toList)
  }                                               //> isThisAllIntegers: (str: String)Boolean
  
  def convertCharListToIntegerList(charList: List[Char]):List[Integer] = {
    charList match {
      case List() => List()
      case x::xs => x.asDigit::convertCharListToIntegerList(xs)
    }
    
  }                                               //> convertCharListToIntegerList: (charList: List[Char])List[Integer]
  
   def replaceCharWithX(str: String, lo: Int, high: Int): String = {
     def helper(strList : List[Char], acc: Int): List[Char] = {
      strList  match {
        case List() => List()
        case x::xs => if(acc >= lo && acc <= high)  if (x.equals('-') || x.equals(' ')) x::helper(xs,acc+1)
        											else 'X'::helper(xs,acc+1)
        			  else x::helper(xs,acc+1)
        
      }
    }
     helper(str.toList, 0).mkString
   }                                              //> replaceCharWithX: (str: String, lo: Int, high: Int)String
    
  def removeSpaceAndDash(str: String): (String, Int) = {
	  def helper(strList : List[Char], acc: Int): Int = strList match {
	  		case x::xs => if(x == '-' || x == ' ') helper(xs, acc+1)
	  					  else  helper(xs, acc)
	  		case List() => acc
		  }

	  (str.replace(" ", "").replace("-", ""), helper(str.toList, 0))
}                                                 //> removeSpaceAndDash: (str: String)(String, Int)
  
  def checkHelper(subStr: String, str:String, lo: Int, high:Int): String  = {
	  def checkRight(subString: String, _lo: Int, _hi:Int): (Boolean, String) = {
	    //println(subString, str, _lo, _hi)
	    if(subString.length() >= 14) if(isThisAllIntegers(subString)) if(checkLuhn(convertCharListToIntegerList(subString.toList)))  (true, replaceCharWithX(str, _lo, _hi-1))
	    							 								  else checkRight(subString.substring(0, subString.length() - 1), _lo, _hi-1)
	    					         else (false, str)
	    else (false, str)
	    		  	 
	    }
	  
	  def checkLeft(subString: String, strRight: String,  _lo: Int, _hi:Int, acc: Int): (Boolean, String) = {
	    //println(subString: String, strRight, _lo: Int, _hi:Int, acc: Int)
	    if(subString.length() >= 14) if(isThisAllIntegers(subString)) if(checkLuhn(convertCharListToIntegerList(subString.toList)))  (true, replaceCharWithX(strRight, _lo+acc, _hi-1))
	    							 								  else checkLeft(subString.substring(_lo+1, subString.length()), strRight, _lo, _hi, acc+1)
	    					         else (false, strRight)
	    else (false, str)
	    
	    
	  }
	  	var right = checkRight(subStr, lo, high)
	  	var rightWithOutDashes = removeSpaceAndDash(right._2.substring(lo,high))
	  	var left = checkLeft(rightWithOutDashes._1, right._2, lo, high, 0)._2.toString()
	  	left
	  }                                       //> checkHelper: (subStr: String, str: String, lo: Int, high: Int)String
	  
  def bruteString(str: String): String= {

	  def runBrute(lo: Int, hi: Int, strRet: String): String = {
		  
		  if(hi > strRet.length()) {
		    
		    var x = removeSpaceAndDash(strRet.substring(lo, strRet.length()))
		    //println(lo, hi, x._1, x._1.length(), x._2,  hi - lo - x._2)
		    if(x._1.length() >= 16) if(x._1.length() < 16) runBrute(lo, lo + 15, strRet)
	    							else if(x._1.length() > 16) runBrute(lo, strRet.length()-1, strRet)
	    							else runBrute(lo+1, strRet.length()+1, checkHelper(x._1, strRet, lo, strRet.length()))
		    else strRet
		  }
		  else {
		    
		    var x = removeSpaceAndDash(strRet.substring(lo, hi))
		    //println(lo, hi, hi+x._2+1, strRet, x._1, x._2, (strRet.length() < hi+x._2+1), x._1.length(), hi - lo - x._2 )
    		if(hi - lo - x._2 < 16) runBrute(lo, hi+x._2+1, strRet)
    		else if(hi - lo - x._2> 16) runBrute(lo+1, lo+16, strRet)
    		else runBrute(lo, hi+1, checkHelper(x._1, strRet, lo, hi))
	      
	      }
		  
	  }
	 
	  if(str.length() < 14) str
      else if(str.length() <= 16) checkHelper(str, str, 0, str.length())
	  else runBrute(0, 15,  str)
	  
  }  
  def main(args: Array[String]): Unit = {
    Source.stdin.getLines().foreach { line =>
    Console.println(bruteString(line)) }  
  }

}