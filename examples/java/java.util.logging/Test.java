import java.util.logging.Logger;

public class Test {
  private static final Logger LOG = Logger.getLogger(Test.class.getName());

  public static void main (String [] args) {
    LOG.warning("warn from: " + Test.class.getName());
  }
}
