package addki.response;

import java.util.List;
import lombok.Data;

@Data
public class CollectEntryResponse {
  private Long id;
  private String language;
  private String word;
  private String definition;
  private String alternateForm;
  private String additionalInfo;
  private String pronunciation;
  private List<String> contexts;
  private List<String> tags;
}
