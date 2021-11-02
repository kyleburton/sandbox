public class Scratch {
  public static void main (String [] args) {
    System.out.println("this is my scratch program!");

    System.out.println("first line\nsecond line");

    String msg = "adding two numbers: 5 + 7 = "  +    5 + 7;
    System.out.println(msg);

    System.out.println("adding two numbers: 5 + 7 = "  +   (5 + 7));
    System.out.println(("adding two numbers: 5 + 7 = " + 5)   + 7);

    int n1 = 5;
    int n2 = 7;
    System.out.println("adding two numbers: " + n1 + " + " + n2 + " = " + (n1 + n2));

    int n3 = n1 + n2;
    System.out.println("adding two numbers: " + n1 + " + " + n2 + " = " + n3);

    n1 = 99;
    System.out.println("adding two numbers: " + n1 + " + " + n2 + " = " + n3);

    ////////////////////////////////////////////////////////////////////////////////
    int[] myNums = new int[6];
    myNums[0] = 1;
    myNums[1] = 2;
    myNums[2] = 3;
    myNums[3] = 4;
    myNums[4] = 5;
    myNums[5] = 6;

    System.out.println("myNums");
    printNums(myNums);

    // n1 + 1;
    n1++;
    System.out.println("n1=" + n1);
    n1++;
    System.out.println("n1=" + n1);
    n1++;
    System.out.println("n1=" + n1);
    n1--;
    System.out.println("n1=" + n1);

    System.out.println("myNums.2");
    int numAtPosition = 4;
    myNums[numAtPosition] = myNums[numAtPosition] * myNums[numAtPosition];
    printNums(myNums);

    System.out.println("more nums");
    printNums(new int[]{1,2,3,4,5,6,7,8,9,0,9,8,7,6,5,4,3,2,1});

    int [] evenMoreNums = {11,22,33,44,55,66,77,88};
    System.out.println("even more nums");
    printNums(evenMoreNums);

    int[] moreNums = {1,2,3,4,5};
    System.out.println("\n\nmoreNums");
    printNums(moreNums);

    System.out.println("\nbump by 1");
    increaseTheNums(1, moreNums);
    printNums(moreNums);

    System.out.println("\nmult by 3");
    multiplyTheNums(3, moreNums);
    printNums(moreNums);

    System.out.println("\nsquare");
    squareTheNums(moreNums);
    printNums(moreNums);


  }

  public static void increaseTheNums(int xx, int[] numbers) {
    for (int index = 0; index < numbers.length; ++index ) {
      numbers[index] = numbers[index] + xx;
    }
  }

  public static void multiplyTheNums(int xx, int[] numbers) {
    for (int index = 0; index < numbers.length; ++index ) {
      numbers[index] = numbers[index] * xx;
    }
  }

  public static void squareTheNums(int[] numbers) {
    for (int index = 0; index < numbers.length; ++index ) {
      numbers[index] = numbers[index] * numbers[index];
    }
  }

  public static void printNums(int[] numbers) {
    for (int index = 0; index < numbers.length; ++index ) {
      System.out.println("numbers[" + index + "]=" + numbers[index]);
    }
  }
}
