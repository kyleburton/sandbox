// javac Project20220106.java  && java Project20220106

import java.util.Random;
import algorithm.OrderedNums;


/*

  You will be given a class which on creation of an object of that
  class generates a list of numbers contained in an int array.  That
  array may be retrieved by calling the getList() method from the
  object.  You will take the numbers in that array and create a new
  list with the following requirements:

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

        WeirdSorter sorter = new WeirdSorter(myList);
        int [] weirdlySorted = sorter.getList();
        System.out.println("The WeirdSorter list is " + weirdlySorted.length + " elements:");
        for (int ii = 0; ii < myList.length; ++ii ) {
            System.out.print(myList[ii] + ", ");
            // System.out.println(" [" + ii + "] = " + myList[ii]);
        }
        System.out.println("");
        for (int ii = 0; ii < weirdlySorted.length; ++ii ) {
            System.out.print(weirdlySorted[ii] + ", ");
            // System.out.println(" [" + ii + "] = " + weirdlySorted[ii]);
        }
        System.out.println("");

    }
}


class HasList {
    public int[] getList() {
        Random rng = new Random();
        int listLengh = 5 + rng.nextInt(23);
        int [] aList = new int[listLengh];

        for (int ii = 0; ii < listLengh; ++ii) {
            aList[ii] = 1+rng.nextInt(listLengh*2);
        }

        return aList;
    }
}

class WeirdSorter {
    private int[] unsorted;
    private int[] sorted;

    public WeirdSorter(int []nums) {
        unsorted = nums;
        sorted = sortNums(nums);
    }

    /**
       All the perfect squares need to be listed first
       All the prime numbers must be listed after the perfect squares
       All numbers whose digits add up to be a multiple of 6 and are not prime must be at the end of the list
       All numbers that do not fall into categories 1-3 above must be listed between the prime numbers and the digits that add to multiples of 6 numbers
       Each list must be separated by a 0.

       33 => 6
       3333 => 12
    */
    public int[] sortNums(int [] givenNums) {
        int[] nums = new int[givenNums.length];
        int[] sortedNums = new int[nums.length+3];
        int targetPos = 0;

        System.arraycopy(givenNums, 0, nums, 0, nums.length);

        // first pass, grab all of the perfect squares
        for(int ii = 0; ii < nums.length; ++ii) {
            if (isPerfectSquare(nums[ii])) {
                // copy it to sortedNums
                sortedNums[targetPos++] = nums[ii];
                // blank it out
                nums[ii] = -1;
            }
        }

        // append a zero
        sortedNums[targetPos++] = 0;

        // second pass, grab all of the prime numbers
        for(int ii = 0; ii < nums.length; ++ii) {
            if (nums[ii] == -1) {
                continue;
            }

            if (isPrime(nums[ii])) {
                // copy it to sortedNums
                sortedNums[targetPos++] = nums[ii];
                // blank it out
                nums[ii] = -1;
            }
        }

        // append a zero
        sortedNums[targetPos++] = 0;

        // third pass, grab all of the nums who's digits are not div by 6
        for(int ii = 0; ii < nums.length; ++ii) {
            if (nums[ii] == -1) {
                continue;
            }

            if (sumDigits(nums[ii]) % 6 != 0) {
                // copy it to sortedNums
                sortedNums[targetPos++] = nums[ii];
                // blank it out
                nums[ii] = -1;
            }
        }

        // append a zero
        sortedNums[targetPos++] = 0;

        // final pass, grab all of the remaining nums
        for(int ii = 0; ii < nums.length; ++ii) {
            if (nums[ii] == -1) {
                continue;
            }

            sortedNums[targetPos++] = nums[ii];
        }

        return sortedNums;
    }

    public int sumDigits(int num) {
        int sum = 0;
        while (num > 0) {
            sum = sum + num % 10;
            num = num / 10;
        }
        return sum;
    }


    public boolean isPerfectSquare(int num) {
        int sqrt = (int)Math.sqrt(num);
        return num != 0 && num == (sqrt*sqrt);
    }

    public boolean isPrime(int num) {
        if (0 == num%2) {
            return false;
        }

        int sqrt = (int)Math.sqrt(num);
        for (int ii = 3; ii <= sqrt; ii += 2) {
            if (0 == num%ii) {
                return false;
            }
        }
        return true;
    }

    public int[] getList() {
        return sorted;
    }

}
