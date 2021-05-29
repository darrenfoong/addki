package addki.request;

import java.util.List;
import lombok.Data;

@Data
public class CollectEntriesRequest {
  private List<String> words;
  private String language;
}
