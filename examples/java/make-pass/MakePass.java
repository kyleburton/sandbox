import javax.swing.JOptionPane;

public class MakePass {

  public static String getStringInput(String prompt) {
    if (false) {
      return JOptionPane.showInputDialog(prompt);
    }
    else {
      System.out.println(prompt);
      java.util.Scanner scanner = new java.util.Scanner(System.in);
      return scanner.nextLine();
    }
  }

  public static void main(String [] args) {

    while (true) {
      String password = getStringInput("Please enter a password: ");

      if (null == password) {
        System.out.println("ok we'll give up then sheesh!");
        break;
      }

      if (CheckPassword.isValid(password)) {
        System.out.println("Very Good, '" + password + "' is an acceptible password.");
        break;
      }

      System.out.println("Nope, '" + password + "' is NOT an acceptible password, please try again.");
    }

  }
}

