public class Scan {
  public static void main (String [] args) {
    java.util.Scanner inp = new java.util.Scanner(System.in);

    System.out.print("enter A: ");
    int inputA = inp.nextInt();

    System.out.print("enter B: ");
    int inputB = inp.nextInt();

    System.out.print("enter C: ");
    int inputC = inp.nextInt();

    int bSquared = inputB * inputB;
    System.out.println("bSquared=" + bSquared);
  }
}
