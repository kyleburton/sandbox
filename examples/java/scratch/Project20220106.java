// javac Project20220106.java  && java Project20220106

import java.util.Random;
import algorithm.OrderedNums;


/*

  You will be given a class which on creation of an object of that class generates a list of numbers contained in an int array.  That array may be retrieved by calling the getList() method from the object.  You will take the numbers in that array and create a new list with the following requirements:

  All the perfect squares need to be listed first
  All the prime numbers must be listed after the perfect squares
  All numbers whose digits add up to be a multiple of 6 and are not prime must be at the end of the list
  All numbers that do not fall into categories 1-3 above must be listed between the prime numbers and the digits that add to multiples of 6 numbers
  Each list must be separated by a 0.
  The final list must be the same size as the original list + 3 (to account for the 0 separators)
  The numbers in each section must be in a new order without using any randomizing commands and must be unique from any other student in either AP Computer Science A period.  You will identify how you reordered the numbers in a comment in your code.  (Tip: if it does not reorder more than half the numbers in any list, it is not valid)
  Your new list MUST be printed at the end.

  You will write a class called WeirdSorter.  It must have 2 instance variables that are both int arrays, a constructor that takes an int array as an argument, a toString that outputs your newly generated list in a user friendly format, and at least 1 method in order to generate the list meeting the above requirements (you may make more than one method if you wish).  It must do all of this without altering the array brought in as an argument in the constructor.

  You will also write a class called WeirdSorterDriver.  This class will run the getList() method from the downloaded class I made and use that as the argument when you create your instance of the WeirdSorter object.  The driver will only do 5 things in this order:
  create an OrderedNums object
  run getList() from the OrderedNums object
  create a WeirdSorter object
  print the WeirdSorter object
  print the array obtained from running getList() on the orderedNums object.

  Other things to remember
  You may not print directly from your class, only from the driver.  (System.out.println() should not appear in your WeirdSorter class anywhere)
  Reversing the order of the numbers does not count as a new order.
  Plagiarized code, as in code copied from a website or another student will receive a 0
  You MAY NOT alter the OrderedNums class in any way other than changing the package name.  You MAY copy the text of the OrderedNums code into your eclipse project without fear of plagiarism as long as you paste all of it into the project without alteration beyond what was mentioned above.


*/

public class Project20220106 {
    public static void main ( String [] args ) {
        HasList hasList = new HasList();
        int [] myList = hasList.getList();

        System.out.println("The List is " + myList.length + " elements:");
        for (int ii = 0; ii < myList.length; ++ii ) {
            System.out.println(" [" + ii + "] = " + myList[ii]);
        }

        OrderedNums nn = new OrderedNums();
        int [] myList2 = nn.getList();

        System.out.println("The OrderedNums list is " + myList2.length + " elements:");
        for (int ii = 0; ii < myList2.length; ++ii ) {
            System.out.println(" [" + ii + "] = " + myList2[ii]);
        }
    }
}


class HasList {
    public int[] getList() {
        Random rng = new Random();
        int listLengh = rng.nextInt(23);
        int [] aList = new int[listLengh];

        for (int ii = 0; ii < listLengh; ++ii) {
            aList[ii] = rng.nextInt(listLengh*2);
        }

        return aList;
    }
}
