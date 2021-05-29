package expts.model;

import java.util.List;
import lombok.Data;

@Data
public class Entry {
  private String language;
  private String word;
  private String definition;
  private String alternateForm;
  private String additionalInfo;
  private String pronunciation;
  private String context;
  private List<String> tags;
}
