package addki.service.impl;

import addki.dao.EntryRepository;
import addki.model.Entry;
import addki.model.EntryStatus;
import addki.model.projection.LanguageOnly;
import addki.request.CollectEntryRequest;
import addki.response.CollectEntryResponse;
import addki.service.EntryService;
import com.optimaize.langdetect.DetectedLanguage;
import com.optimaize.langdetect.LanguageDetector;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class DefaultEntryService implements EntryService {
  @Autowired private EntryRepository entryRepository;

  @Autowired private RabbitTemplate rabbitTemplate;

  @Autowired private LanguageDetector languageDetector;

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
      entry.setUpdated(Instant.now());
      entry.setStatus(EntryStatus.PRINTED);
    }

    entryRepository.saveAll(entries);

    return String.join(
        "\n", entries.stream().map(Entry::toAnkiString).collect(Collectors.toList()));
  }

  @Override
  public void collectEntries(List<String> words, String language) {
    for (String word : words) {
      Entry entry = new Entry();
      entry.setUpdated(Instant.now());
      entry.setWord(word);

      boolean error = false;

      if (language == null || language.isBlank()) {
        List<DetectedLanguage> probs = languageDetector.getProbabilities(word);

        if (!probs.isEmpty()) {
          entry.setLanguage(probs.get(0).getLocale().getLanguage());
        } else {
          log.error("Failed to detect language of {}", word);
          error = true;
        }
      }

      entry = entryRepository.save(entry);

      CollectEntryRequest collectEntryRequest = new CollectEntryRequest();
      collectEntryRequest.setId(entry.getId());
      collectEntryRequest.setWord(word);
      collectEntryRequest.setLanguage(language);

      if (!error) {
        entry.setStatus(EntryStatus.COLLECTING);
        rabbitTemplate.convertAndSend("request." + language, collectEntryRequest);
      } else {
        entry.setStatus(EntryStatus.ERROR);
      }
    }
  }

  @RabbitListener(queues = "response")
  public void handleResponse(CollectEntryResponse collectEntryResponse) {
    Optional<Entry> entryOpt = entryRepository.findById(collectEntryResponse.getId());

    if (entryOpt.isPresent()) {
      Entry entry = entryOpt.get();

      entry.setWord(collectEntryResponse.getWord());
      entry.setDefinition(collectEntryResponse.getDefinition());
      entry.setAlternateForm(collectEntryResponse.getAlternateForm());
      entry.setAdditionalInfo(collectEntryResponse.getAdditionalInfo());
      entry.setPronunciation(collectEntryResponse.getPronunciation());
      entry.setContexts(collectEntryResponse.getContexts());
      entry.setTags(collectEntryResponse.getTags());

      entry.setUpdated(Instant.now());
      entry.setStatus(EntryStatus.COLLECTED);

      entryRepository.save(entry);
    } else {
      log.error("Invalid CollectEntryResponse (id: {})", collectEntryResponse.getId());
    }
  }

  @Transactional
  @Override
  public void cleanup() {
    entryRepository.deleteByStatus(EntryStatus.PRINTED);
    log.info("Cleaned up entries with status PRINTED");
  }
}
