package addki.service;

import addki.model.Entry;
import java.util.List;
import org.springframework.data.domain.Page;

public interface EntryService {
  Page<Entry> getEntries(int number, int size);

  List<String> getLanguages();

  String printEntries(String language);

  void collectEntry(String word, String language);

  void cleanup();
}
