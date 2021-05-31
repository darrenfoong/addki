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
      if (language == null || language.isBlank()) {
        List<DetectedLanguage> probs = languageDetector.getProbabilities(word);

        if (!probs.isEmpty()) {
          language = probs.get(0).getLocale().getLanguage();
        } else {
          log.error("Failed to detect language of {}", word);
        }
      }

      Entry entry = new Entry();
      entry.setUpdated(Instant.now());
      entry.setLanguage(language);
      entry.setWord(word);
      entry.setStatus(EntryStatus.COLLECTING);

      entry = entryRepository.save(entry);

      CollectEntryRequest collectEntryRequest = new CollectEntryRequest();
      collectEntryRequest.setId(entry.getId());
      collectEntryRequest.setWord(word);
      collectEntryRequest.setLanguage(language);

      rabbitTemplate.convertAndSend("request." + language, collectEntryRequest);
    }
  }

  @RabbitListener(queues = "request.ko")
  public void handleKoRequest(CollectEntryRequest collectEntryRequest) {
    CollectEntryResponse collectEntryResponse = new CollectEntryResponse();
    collectEntryResponse.setId(collectEntryRequest.getId());
    collectEntryResponse.setLanguage(collectEntryRequest.getLanguage());
    collectEntryResponse.setWord(collectEntryRequest.getWord());
    collectEntryResponse.setDefinition("to study");
    collectEntryResponse.setAlternateForm("工夫");
    collectEntryResponse.setAdditionalInfo(null);
    collectEntryResponse.setPronunciation(null);
    collectEntryResponse.setContexts(List.of("저는 매일 공부합니다"));
    collectEntryResponse.setTags(List.of("v"));

    rabbitTemplate.convertAndSend("response", collectEntryResponse);
  }

  @RabbitListener(queues = "request.ja")
  public void handleJpRequest(CollectEntryRequest collectEntryRequest) {
    CollectEntryResponse collectEntryResponse = new CollectEntryResponse();
    collectEntryResponse.setId(collectEntryRequest.getId());
    collectEntryResponse.setLanguage(collectEntryRequest.getLanguage());
    collectEntryResponse.setWord(collectEntryRequest.getWord());
    collectEntryResponse.setDefinition("공부하다");
    collectEntryResponse.setAlternateForm("べんきょうする");
    collectEntryResponse.setAdditionalInfo(null);
    collectEntryResponse.setPronunciation(null);
    collectEntryResponse.setContexts(null);
    collectEntryResponse.setTags(List.of("v"));
    rabbitTemplate.convertAndSend("response", collectEntryResponse);
  }

  @RabbitListener(queues = "request.zh")
  public void handleZhRequest(CollectEntryRequest collectEntryRequest) {
    CollectEntryResponse collectEntryResponse = new CollectEntryResponse();
    collectEntryResponse.setId(collectEntryRequest.getId());
    collectEntryResponse.setLanguage(collectEntryRequest.getLanguage());
    collectEntryResponse.setWord(collectEntryRequest.getWord());
    collectEntryResponse.setDefinition("to study");
    collectEntryResponse.setAlternateForm(null);
    collectEntryResponse.setAdditionalInfo(null);
    collectEntryResponse.setPronunciation("du2 shu1");
    collectEntryResponse.setContexts(null);
    collectEntryResponse.setTags(List.of("v"));

    rabbitTemplate.convertAndSend("response", collectEntryResponse);
  }

  @RabbitListener(queues = "response")
  public void handleResponse(CollectEntryResponse collectEntryResponse) {
    try {
      Thread.sleep(5000);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

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
