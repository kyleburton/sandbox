
import com.github.krukow.clj_lang.IPersistentMap;
import com.github.krukow.clj_lang.PersistentHashMap;

public class Main {
  public static void main (String [] args) {
    IPersistentMap m = PersistentHashMap.emptyMap();
    System.out.println("map=" + m);
    m = m.assoc("this","that");
    System.out.println("map=" + m);
    m = m.assoc("other",42);
    System.out.println("map=" + m);
  }
}
