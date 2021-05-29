package expts.model;

import javax.persistence.Entity;
import javax.persistence.Table;
import lombok.Data;

@Entity
@Table(name = "entries")
@Data
public class Entry {
  private String language;
  private String word;
  private String definition;
  private String alternateForm;
  private String additionalInfo;
  private String pronunciation;
  private String context;
}
