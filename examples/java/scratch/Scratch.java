public class Scratch {

  public static void printBoard(String[][] board) {
    for (int ii = 0; ii < board.length; ii++) {
      String [] row = board[ii];
      for (int jj = 0; jj < row.length; jj++) {
        String val = row[jj];
        System.out.print(val);
      }
      System.out.println("");
    }
  }

  public static void main (String [] args) {

    String[][] board1 = new String[][] {
      {" ", "1", "1", " ", "1", "1", " "},
      {"1", "1", "1", "1", "1", "1", "1"},
      {" ", "1", "1", "1", "1", "1", " "},
      {" ", " ", "1", "1", "1", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
    };


    String[][] board2 = new String[][] {
      {" ", " ", "1", "1", "1", " ", " "},
      {" ", "1", "1", "1", "1", "1", " "},
      {" ", " ", "1", "1", "1", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "},
      {" ", " ", " ", "1", " ", " ", " "}, };


    System.out.println("");
    printBoard(board1);

    System.out.println("");
    printBoard(board2);

  }

}
