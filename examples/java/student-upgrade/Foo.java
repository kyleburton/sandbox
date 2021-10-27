public class Foo {
  public static void main (String [] args) {
    double[] theThings = {4.0, 3.75, 2.99, 3.50, 3.00, 3.25};

    for (int ii = 0; ii < theThings.length; ++ii ) {
      System.out.println(String.format("theThings[%d]=%f", ii, theThings[ii]));
    }

    double total = 0.0;
    for (int ii = 0; ii < theThings.length; ++ii ) {
      total = total + theThings[ii];
    }

    double average = total / theThings.length;
    System.out.println(String.format("total=%f; average=%f", total, average));
  }
}
