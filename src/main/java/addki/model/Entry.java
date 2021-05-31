package addki.model;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import java.time.Instant;
import java.util.List;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import lombok.Data;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

@Entity
@Table(name = "entries")
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@Data
public class Entry {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Instant updated;

  private String language;
  private String word;
  private String definition;
  private String alternateForm;
  private String additionalInfo;
  private String pronunciation;

  @Type(type = "jsonb")
  @Column(columnDefinition = "jsonb")
  private List<String> contexts;

  @Type(type = "jsonb")
  @Column(columnDefinition = "jsonb")
  private List<String> tags;

  private EntryStatus status;
  private String errorMessage;

  public String toAnkiString() {
    StringBuilder stringBuilder = new StringBuilder();

    stringBuilder.append(word);

    if (additionalInfo != null) {
      stringBuilder.append(" ");
      stringBuilder.append(additionalInfo);
    }

    stringBuilder.append(";");

    if (alternateForm != null) {
      stringBuilder.append(alternateForm);
      stringBuilder.append("<br>");
    }

    stringBuilder.append(definition);

    if (pronunciation != null) {
      stringBuilder.append("<br>");
      stringBuilder.append("(" + pronunciation + ")");
    }

    stringBuilder.append(";");

    if (contexts != null) {
      stringBuilder.append(String.join("<br>", contexts));
    }

    stringBuilder.append(";");

    if (tags != null) {
      stringBuilder.append(String.join(" ", tags));
    }

    return stringBuilder.toString();
  }
}
