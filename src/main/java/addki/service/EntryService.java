package addki.service;

import addki.model.Entry;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.scheduling.annotation.Async;

public interface EntryService {
  Page<Entry> getEntries(int number, int size);

  List<String> getLanguages();

  String printEntries(String language);

  @Async
  void collectEntries(List<String> words, String language);

  void cleanup();
}
