import java.lang.Math;

public class Scan {
  public static void main (String [] args) {
    java.util.Scanner inp = new java.util.Scanner(System.in);

    // requirement A
    System.out.print("enter A: ");
    int inputA = inp.nextInt();
    System.out.println("");

    System.out.print("enter B: ");
    int inputB = inp.nextInt();
    System.out.println("");

    System.out.print("enter C: ");
    int inputC = inp.nextInt();
    System.out.println("");

    // requirement B
    int bSquared = inputB * inputB;
    System.out.println("bSquared=" + bSquared);

    // requirement C
    int fourAC = 4 * inputA * inputC;
    System.out.println("fourAC=" + fourAC);

    // requirement D
    System.out.println("b^2-4ac=" + (bSquared - fourAC));
    double sqrtFourAC = Math.sqrt(Math.abs(bSquared - fourAC));
    System.out.println("sqrtFourAC=" + sqrtFourAC);

    double denom = 2 * inputA;

    double result1 = ((-1 * inputB) + sqrtFourAC) / (2 * inputA);
    double result2 = ((-1 * inputB) - sqrtFourAC) / (2 * inputA);

    System.out.println("result1=" + result1);
    System.out.println("result2=" + result2);
    System.out.println(String.format("%dx^2 + %dx + %d = 0", inputA, inputB, inputC));
    double res3 = inputA * (result1*result1) + inputB * result1 + inputC;
    System.out.println(String.format("%f + %f + %d = %f",
          inputA*(result1*result1),
          inputB*result1,
          inputC,
          res3
          ));

    double res4 = inputA * (result2*result2) + inputB * result2 + inputC;
    System.out.println(String.format("%f + %f + %d = %f",
          inputA*(result2*result2),
          inputB*result2,
          inputC,
          res4
          ));

  }
}
