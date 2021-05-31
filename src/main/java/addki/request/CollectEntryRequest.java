package addki.request;

import lombok.Data;

@Data
public class CollectEntryRequest {
  private Long id;
  private String word;
  private String language;
}
