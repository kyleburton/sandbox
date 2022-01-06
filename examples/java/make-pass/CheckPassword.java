public class CheckPassword {
  public static boolean isValid(String pass) {
    if (pass.length() < 6) {
      return false;
    }

    boolean hasNonalphaChar = false;

    for (int ii = 0; ii < pass.length(); ++ii) {
      String ch = pass.substring(ii, ii+1);
      if ("+".equals(ch)) { hasNonalphaChar = true; }
      if ("-".equals(ch)) { hasNonalphaChar = true; }
      if ("/".equals(ch)) { hasNonalphaChar = true; }
      if ("*".equals(ch)) { hasNonalphaChar = true; }
      if ("@".equals(ch)) { hasNonalphaChar = true; }
    }

    return hasNonalphaChar;
  }
}
