public class AskAndSearch {
    public static void main (String [] args) {
        int numArtists = 4;
        String [] artists = new String[numArtists];
        java.util.Scanner inp = new java.util.Scanner(System.in);

        for (int ii = 0; ii < artists.length; ++ii) {
            System.out.print(String.format("Please enter your favorite [%d of %d]: ", ii+1, artists.length));
            artists[ii] = inp.nextLine();
        }

        System.out.println("");
        for (int ii = 0; ii < artists.length; ++ii) {
            System.out.println(String.format("  %s is your fav? awesome!", artists[ii]));
        }


        System.out.print("please enter an artist to search for: ");
        String searchTerm = inp.nextLine();

        System.out.println("");
        for (int ii = 0; ii < artists.length; ++ii) {
            if (artists[ii].equals(searchTerm)) {
                System.out.println(String.format("match[%d] %s == %s", ii, artists[ii], searchTerm));
                continue;
            }

            System.out.println(String.format(" miss[%d] %s != %s", ii, artists[ii], searchTerm));
        }


    }
}
