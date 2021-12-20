import com.github.krukow.clj_lang.IPersistentMap;
import com.github.krukow.clj_lang.PersistentHashMap;
import java.util.Map;

public class Main {
  public static void main (String [] args) {
    IPersistentMap<Object,Object> m = PersistentHashMap.emptyMap();
    System.out.println("map=" + m);
    m = m.assoc("this","that");
    System.out.println("map=" + m);
    m = m.assoc("other",42);
    System.out.println("map=" + m);

    for (Map.Entry<Object,Object> e : (PersistentHashMap<Object,Object>) m) {
      System.out.println("k[" + e.getKey() + "]=v[" + e.getValue() + "]");
    }
  }
}
