public class Foo {
  public static class MyBook {
    private String title;
    private String author;
    private int numPages;
    private boolean wouldRecommend;

    public MyBook(String title, String author, int numPages, boolean wouldRecommend) {
      this.title = title;
      this.author = author;
      this.numPages = numPages;
      this.wouldRecommend = wouldRecommend;
    }

    public String toString () {
      return String.format("MyBook{title=%s; author=%s; numPages=%d; wouldRecommend=%s}",
          this.title,
          this.author,
          this.numPages,
          this.wouldRecommend
          );
    }
  }

  public static void main (String [] args) {
    final boolean WOULD_RECOMMEND = true;
    final boolean DO_NOT_RECOMMEND = false;

    MyBook b1 = new MyBook("Hyperion", "Dan Simmons", 352, WOULD_RECOMMEND);
    MyBook b2 = new MyBook("Hyperion", "Dan Simmons", 352, true);

    System.out.println("thing");
  }

  public static void main02 (String [] args) {
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
