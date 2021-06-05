package addki.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import addki.dao.HashMapEntryRepository;
import addki.model.Entry;
import com.optimaize.langdetect.DetectedLanguage;
import com.optimaize.langdetect.LanguageDetector;
import com.optimaize.langdetect.i18n.LdLocale;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;

@SpringBootTest(classes = {DefaultEntryServiceTest.Config.class, DefaultEntryService.class})
public class DefaultEntryServiceTest {
  static class Config {
    @Bean
    public HashMapEntryRepository getHashMapEntryRepository() {
      return DefaultEntryServiceTest.hashMapEntryRepository;
    }
  }

  @Autowired private DefaultEntryService defaultEntryService;

  private static final HashMapEntryRepository hashMapEntryRepository = new HashMapEntryRepository();

  @MockBean private RabbitTemplate rabbitTemplate;

  @MockBean private LanguageDetector languageDetector;

  @BeforeEach
  public void init() {
    hashMapEntryRepository.getMap().clear();
  }

  @Test
  public void collectEntriesMultipleWordsSupportedSuppliedLanguageTest() {
    final List<String> words = List.of("zhword1", "zhword2", "zhword3");
    final String language = "zh";

    defaultEntryService.collectEntries(words, language);

    assertEquals(words.size(), hashMapEntryRepository.getMap().size());

    for (Entry entry : hashMapEntryRepository.getMap().values()) {
      assertEquals(language, entry.getLanguage());
      assertNull(entry.getErrorMessage());
    }
  }

  @Test
  public void collectEntriesMultipleWordsUnsupportedSuppliedLanguageTest() {
    final List<String> words = List.of("thword1", "thword2", "thword3");
    final String language = "th";

    defaultEntryService.collectEntries(words, language);

    assertEquals(words.size(), hashMapEntryRepository.getMap().size());

    for (Entry entry : hashMapEntryRepository.getMap().values()) {
      assertEquals(language, entry.getLanguage());
      assertTrue(
          entry.getErrorMessage().startsWith("Detected language")
              && entry.getErrorMessage().endsWith("is not supported"));
    }
  }

  @Test
  public void collectEntriesMultipleWordsNoSuppliedLanguageTest() {
    final List<String> words = List.of("koword1", "jaword1", "zhword1", "thword1", "unknown");
    final String language = null;

    Mockito.when(languageDetector.getProbabilities("koword1"))
        .thenReturn(List.of(new DetectedLanguage(LdLocale.fromString("ko"), 1)));
    Mockito.when(languageDetector.getProbabilities("jaword1"))
        .thenReturn(List.of(new DetectedLanguage(LdLocale.fromString("ja"), 1)));
    Mockito.when(languageDetector.getProbabilities("zhword1"))
        .thenReturn(List.of(new DetectedLanguage(LdLocale.fromString("zh"), 1)));
    Mockito.when(languageDetector.getProbabilities("thword1"))
        .thenReturn(List.of(new DetectedLanguage(LdLocale.fromString("th"), 1)));
    Mockito.when(languageDetector.getProbabilities("unknown")).thenReturn(List.of());

    defaultEntryService.collectEntries(words, language);

    assertEquals(words.size(), hashMapEntryRepository.getMap().size());

    Entry entry;

    entry = hashMapEntryRepository.getMap().get(0L);
    assertEquals("ko", entry.getLanguage());
    assertNull(entry.getErrorMessage());

    entry = hashMapEntryRepository.getMap().get(1L);
    assertEquals("ja", entry.getLanguage());
    assertNull(entry.getErrorMessage());

    entry = hashMapEntryRepository.getMap().get(2L);
    assertEquals("zh", entry.getLanguage());
    assertNull(entry.getErrorMessage());

    entry = hashMapEntryRepository.getMap().get(3L);
    assertEquals("th", entry.getLanguage());
    assertEquals("Detected language (th) is not supported", entry.getErrorMessage());

    entry = hashMapEntryRepository.getMap().get(4L);
    assertNull(entry.getLanguage());
    assertEquals("Failed to detect language of unknown", entry.getErrorMessage());
  }
}
