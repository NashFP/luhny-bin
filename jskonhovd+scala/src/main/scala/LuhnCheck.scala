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
    }                                              
        
         
      
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
    }                                              
    
    def luhnList(a: List[Integer]):List[Integer] = { a match {
        case List() => List()
        case x::xs => if(x > 9) integerBreakDown(x).reverse:::luhnList(xs)
            		  else integerBreakDown(x).reverse:::luhnList(xs)
    }
    }                                              
    
    def sumLuhn(a:List[Integer]): Integer = { a match {
        case List() => 0
        case x::xs => x+sumLuhn(xs)
        }
    }                                               
    
    def checkLuhn(a : List[Integer]): Boolean = {
        if(sumLuhn((luhnList(luhnDoubleOddNumber(a.reverse)))) % 10 == 0) true
        else false
    }                                               
    
    def isThisAllIntegers(str : String):Boolean = {
        def helper(charList: List[Char]): Boolean = { charList match {
        case List() => true
        case x::xs => if (x.isDigit) helper(xs)
        			  else false
        }
        }
        helper(str.toList)
    }                                               
    
    def convertCharListToIntegerList(charList: List[Char]):List[Integer] = {
        charList match {
        case List() => List()
        case x::xs => x.asDigit::convertCharListToIntegerList(xs)
        }
    }                                               
    
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
    }                                              
    
    def removeSpaceAndDash(str: String): (String, Int) = {
        def helper(strList : List[Char], acc: Int): Int = strList match {
      		case x::xs => if(x == '-' || x == ' ') helper(xs, acc+1)
      					  else  helper(xs, acc)
      		case List() => acc
    	  }
    
        (str.replace(" ", "").replace("-", ""), helper(str.toList, 0))
    }                                                 
    
    
    def logicalOrMask(str: String, mask: String):String = {
        def helper(strList: List[Char], maskList: List[Char], newList: List[Char]): List[Char] = {
        (strList, maskList) match {
        case (x::xs,y::ys) => if(x == 'X' || y == 'X')  'X'::helper(xs, ys, newList)
        					  else x::helper(xs,ys, newList)
        case (List(),List()) => newList
        case _ => newList
        }
        }
        helper(str.toList, mask.toList, List()).mkString
    
    }                                               
    
        
    def checkLeft(subString: String, strRight: String,  _lo: Int, _hi:Int, acc: Int): (Boolean, String) = {
        //println(acc, subString.length()-1, subString, strRight, _lo, _hi, acc+1)
        if(subString.length() >= 14) if(isThisAllIntegers(subString)) if(checkLuhn(convertCharListToIntegerList(subString.toList)))  (true, replaceCharWithX(strRight, _lo+acc, _hi-1))
        							 								  else {
    				  													checkLeft(subString.substring(1, subString.length()), strRight, _lo, _hi, acc+1)
        							 								  }
        					         else (false, strRight)
        else (false, strRight)    
        
      }                                       
      
    def checkRight(subString: String, str: String, _lo: Int, _hi:Int): (Boolean, String) = {
        //println(subString, str, _lo, _hi)
        if(subString.length() >= 14) if(isThisAllIntegers(subString)) if(checkLuhn(convertCharListToIntegerList(subString.toList)))  (true, replaceCharWithX(str, _lo, _hi-1))
        							 								  else checkRight(subString.substring(0, subString.length() - 1), str, _lo, _hi-1)
        					         else (false, str)
        else (false, str)
        		  	 
        }                                     
        
    def checkHelper(subStr: String, str:String, lo: Int, high:Int, mask: String): String  = {
    
      	var right = checkRight(subStr,str, lo, high)
      	var rightWithOutDashes = removeSpaceAndDash(right._2.substring(lo,high))
      	var left = checkLeft(rightWithOutDashes._1, right._2, lo, high, 0)._2.toString()
      	logicalOrMask(left, mask)
      }                                       
      
      
    def bruteString(str: String): String= {
    
        def runBrute(lo: Int, hi: Int, strRet: String, mask:String): String = {
          
          if(hi > strRet.length()) {
            
            var x = removeSpaceAndDash(strRet.substring(lo, strRet.length()))
            //println(lo, hi, x._1, x._1.length(), x._2,  hi - lo - x._2)
            if(x._1.length() >= 16) if(x._1.length() < 16) runBrute(lo, lo + 15, strRet, mask)
        							else if(x._1.length() > 16) runBrute(lo, strRet.length()-1, strRet, mask)
        							else runBrute(lo+1, strRet.length()+1, strRet, checkHelper(x._1, strRet, lo, strRet.length(), mask))
            else mask
          }
          else {
            
            var x = removeSpaceAndDash(strRet.substring(lo, hi))
            //println(lo, hi, hi+x._2+1, strRet, x._1, x._2, (strRet.length() < hi+x._2+1), x._1.length(), hi - lo - x._2 )
        	if(hi - lo - x._2 < 16) runBrute(lo, hi+x._2+1, strRet, mask)
        	else if(hi - lo - x._2> 16) runBrute(lo+1, lo+16, strRet, mask)
        	else runBrute(lo, hi+1, strRet, checkHelper(x._1, strRet, lo, hi, mask))
          
          }
          
        }
     
        if(str.length() < 14) str
        else if(str.length() <= 16) checkHelper(str, str, 0, str.length(), str)
        else runBrute(0, 15,  str, str)
      
    }  
    
    def main(args: Array[String]): Unit = {
        Source.stdin.getLines().foreach { line =>
        Console.println(bruteString(line)) }  
    }
}
