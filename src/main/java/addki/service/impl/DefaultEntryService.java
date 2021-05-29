package addki.service.impl;

import addki.dao.EntryRepository;
import addki.model.Entry;
import addki.model.EntryStatus;
import addki.model.projection.LanguageOnly;
import addki.service.EntryService;
import java.util.List;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class DefaultEntryService implements EntryService {
  @Autowired private EntryRepository entryRepository;

  @Override
  public Page<Entry> getEntries(int number, int size) {
    return entryRepository.findAll(PageRequest.of(number, size));
  }

  @Override
  public List<String> getLanguages() {
    return entryRepository.findDistinctByStatus(EntryStatus.COLLECTED).stream()
        .map(LanguageOnly::getLanguage)
        .collect(Collectors.toList());
  }

  @Transactional
  @Override
  public String printEntries(String language) {
    List<Entry> entries = entryRepository.findByLanguageAndStatus(language, EntryStatus.COLLECTED);

    for (Entry entry : entries) {
      entry.setStatus(EntryStatus.PRINTED);
    }

    entryRepository.saveAll(entries);

    return String.join(
        "\n", entries.stream().map(Entry::toAnkiString).collect(Collectors.toList()));
  }

  @Override
  public void collectEntry(String word, String language) {
    if (word.equals("공부하다")) {
      Entry entry = new Entry();
      entry.setLanguage("ko");
      entry.setWord(word);
      entry.setDefinition("to study");
      entry.setAlternateForm("工夫");
      entry.setAdditionalInfo(null);
      entry.setPronunciation(null);
      entry.setContexts(List.of("저는 매일 공부합니다"));
      entry.setTags(List.of("v"));
      entry.setStatus(EntryStatus.COLLECTED);

      entryRepository.save(entry);
    } else if (word.equals("勉強する")) {
      Entry entry = new Entry();
      entry.setLanguage("jp");
      entry.setWord(word);
      entry.setDefinition("공부하다");
      entry.setAlternateForm("べんきょうする");
      entry.setAdditionalInfo(null);
      entry.setPronunciation(null);
      entry.setContexts(null);
      entry.setTags(List.of("v"));
      entry.setStatus(EntryStatus.COLLECTED);

      entryRepository.save(entry);
    } else if (word.equals("读书")) {
      Entry entry = new Entry();
      entry.setLanguage("zh-cn");
      entry.setWord(word);
      entry.setDefinition("to study");
      entry.setAlternateForm(null);
      entry.setAdditionalInfo(null);
      entry.setPronunciation("du2 shu1");
      entry.setContexts(null);
      entry.setTags(List.of("v"));
      entry.setStatus(EntryStatus.COLLECTED);

      entryRepository.save(entry);
    }
  }

  @Transactional
  @Override
  public void cleanup() {
    entryRepository.deleteByStatus(EntryStatus.PRINTED);
    log.info("Cleaned up entries with status PRINTED");
  }
}
