package addki.service;

import addki.model.Entry;
import java.util.List;
import org.springframework.data.domain.Page;

public interface EntryService {
  Page<Entry> getEntries(int number, int size);

  List<String> getSupportedLanguages();

  String printEntries(String language);

  void collectEntries(List<String> words, String language);

  void cleanup();
}
