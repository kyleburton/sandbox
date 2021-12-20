import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import java.io.IOException;

public class Test {
  public static void main (String[] args) throws IOException {
    String docId = "24312906";
    if (args.length > 0) {
      docId = args[0];
    }

    String url = "http://www.ncbi.nlm.nih.gov/pubmed/" + docId;
    Document doc = Jsoup.connect(url).timeout(60000).userAgent("Mozilla/25.0").get();
    Elements authors = doc.select("div.auths >*");

    System.out.println("os.name=" + System.getProperty("os.name"));
    System.out.println("os.arch=" + System.getProperty("os.arch"));

    // System.out.println("doc=" + doc);
    System.out.println("authors=" + authors);
    System.out.println("authors.length=" + authors.size());

    for (Element a : authors) {
      System.out.println("  author: " + a);
    }
  }
}
